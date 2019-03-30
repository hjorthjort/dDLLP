{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module DLLP where

import Control.Monad.Reader
import Data.List (concat, intersperse)
import qualified Data.Map.Lazy as M (Map, toList, fromList, empty, insertWith, insert, (\\), lookup, delete)
import Data.Maybe (fromJust, isJust)

-- TYPES

type Name = String
type Index = Int
type Problem = [Clause]

type Clause = [CNF Term]
type Assignments = M.Map Name Bool
type Relevants = M.Map Name [(Index, Bool)]

type ClauseTable = M.Map Int Clause
type Propagations = [(Name, Bool)]

data Env = Env { ass :: Assignments, rels :: Relevants, cls :: ClauseTable, props :: Propagations }

clsToCnf :: ClauseTable -> CNF Problem
clsToCnf cls = let clauses = map snd $ M.toList cls
               in problem $ map clause clauses

instance Show Env where
  show env = "{ ass: " ++ (show.ass) env ++ ", problem: " ++ (show.clsToCnf.cls) env ++ ", props: " ++ (show.props) env ++ " }"

data Solution = SAT Assignments deriving (Eq, Show)

data Term
data CNF a where
  Neg       :: Name -> CNF Term
  Pure      :: Name -> CNF Term
  Clause    :: [CNF Term] -> CNF Clause
  Problem   :: [CNF Clause] -> CNF Problem

getName :: CNF Term -> Name
getName (Neg  name) = name
getName (Pure name) = name

instance Show (CNF Term) where
  show (Neg name) = "¬" ++ name
  show (Pure name) = name

instance Show (CNF Clause) where
  show (Clause terms) = "(" ++ joinWith " ∨ " terms ++ ")"

instance Show (CNF Problem) where
  show (Problem cs) = joinWith " ∧ " cs

joinWith :: Show a => String -> [a] -> String
joinWith op = concat . intersperse op . map show

instance Eq (CNF Term) where
  (Neg  s) == (Neg  s') = s == s'
  (Pure s) == (Pure s') = s == s'
  _        == _         = False

neg       :: Name -> CNF Term
pur       :: Name -> CNF Term
clause    :: [CNF Term] -> CNF Clause
problem   :: [CNF Clause] -> CNF Problem
neg = Neg
pur = Pure
clause = Clause
problem = Problem

  -- Initializers

initialize :: CNF Problem -> Env
initialize cnfProb =
  let table = initializeClauses cnfProb
  in Env {
    ass  = M.empty
  , rels = initializeRelevants table
  , cls  = table
  , props = initializePropagations table }

initializeRelevants :: ClauseTable -> Relevants
initializeRelevants table = foldr insertRelevants M.empty $ M.toList table
  where
    insertRelevants :: (Index, Clause) -> Relevants -> Relevants
    insertRelevants (_,   []  ) rs = rs
    insertRelevants (idx, (Neg  name):ts) rs = M.insertWith (++) name [(idx, False)] $ insertRelevants (idx, ts) rs
    insertRelevants (idx, (Pure name):ts) rs = M.insertWith (++) name [(idx, True )] $ insertRelevants (idx, ts) rs

initializeClauses :: CNF Problem -> ClauseTable
initializeClauses (Problem cnfClauses) = M.fromList $ zip [0..] $ filter (not . null) $ map fromCnf cnfClauses
  where
    fromCnf :: CNF Clause -> Clause
    fromCnf (Clause terms) = terms

initializePropagations :: ClauseTable -> Propagations
initializePropagations = map toPropagation . filter isSingleton . map snd . (M.toList)
  where
    isSingleton [_] = True
    isSingleton  _  = False
    toPropagation :: Clause -> (Name, Bool)
    toPropagation [Neg  name] = (name, False)
    toPropagation [Pure name] = (name, True)

-- Propagating

dllp :: Reader Env (Maybe Solution)
dllp = do
  env <- ask
  if allResolved env
    then (return . Just . SAT) (ass env)
    else dllpStep

  where allResolved = null . M.toList . cls

dllpStep :: Reader Env (Maybe Solution)
dllpStep = do
  env <- ask
  case props env of
    []     -> do let name = decide env
                 -- TODO: Parallelize.
                 res1 <- local (propagate (name, True)) dllp
                 case res1 of
                   Nothing -> local (propagate (name, False)) dllp
                   x -> return x
    (p@(name, value):ps) -> let env' = (propagate p) env
                            in case checkRelevantClauses env' name of
                                 Left  _          -> return Nothing -- Some clause was empty, can't be satisfied.
                                 Right singletons -> let env'' = env'{props = ps ++ map singletonToProp singletons}
                                                     in local (const env'') dllp  -- TODO: remvoe const, make function for modifying environment in pure way.
  where checkRelevantClauses (Env{rels = rs, cls = cs}) name =
          let relevants   = map fromJust $ filter isJust $ map (\(idx, _) -> (M.lookup idx cs)) $ fromJust $ M.lookup name rs
              unsats      = filter null relevants
              singletons  = filter (\list -> case list of [_] -> True; _ -> False) relevants
          in if null unsats then Right singletons else Left unsats
        singletonToProp :: Clause -> (Name, Bool)
        singletonToProp [Neg  name] = (name, False)
        singletonToProp [Pure name] = (name, True)

-- If the algorithms for xonstructing and editing are correct, there should
-- always be at least one unset variable in the assignments map. If all
-- available variables were set in the list of assignments, then every clause
-- should have resolved to either true or empty. Therefore, the head operation
-- is safe.
decide :: Env -> Name
decide env = fst . head . M.toList $ (rels env) M.\\ (ass env)

propagate :: (Name, Bool) -> Env -> Env
propagate (name, value) env@(Env {ass = as, rels = rs, cls = cs}) =
  let as' = M.insert name value as
      relevants = fromJust (M.lookup name rs)
      cs' = foldr updateWithRel cs relevants
  in env{ass=as', cls=cs'}
  where
    updateWithRel :: (Index, Bool) -> ClauseTable -> ClauseTable
    updateWithRel (idx, expected) table =
      if value == expected
      then M.delete idx table -- We have a true term, clause is now true
      else let newClause = removeFromClause name $ fromJust $ M.lookup idx table
      -- The assignment is wrong, so remove the term from the clause.
           in M.insert idx newClause table
    removeFromClause :: Name -> Clause -> Clause
    removeFromClause name = filter ((name/=) . getName)
