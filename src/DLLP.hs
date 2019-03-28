{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module DLLP where

import Data.List (concat, intersperse)
import qualified Data.Map as M (Map, toList, fromList, empty, insertWith)

type Name = String
type Index = Int
type Problem = [Clause]

type Clause = [CNF Term]
type Assignments = M.Map Name Bool
type Relevants = M.Map Name [(Index, Bool)]
type ClauseTable = M.Map Int Clause
type Propagations = [(Name, Bool)]

data Env = Env { ass :: Assignments, rels :: Relevants, cls :: ClauseTable, props :: Propagations} deriving Show

data Term
data CNF a where
  Neg       :: Name -> CNF Term
  Pure      :: Name -> CNF Term
  Clause    :: [CNF Term] -> CNF Clause
  Problem   :: [CNF Clause] -> CNF Problem

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
pure      :: Name -> CNF Term
clause    :: [CNF Term] -> CNF Clause
problem   :: [CNF Clause] -> CNF Problem
neg = Neg
pure = Pure
clause = Clause
problem = Problem

initialize :: CNF Problem -> Env
initialize cnfProb =
  let table = initializeClauses cnfProb
  in Env {
    ass  = M.empty
  , rels = initializeRelevants table
  , cls  = table
  , props = initializePropagations table}

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
