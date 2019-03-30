import DLLP
import Lib
import qualified Data.Map as M
import Test.QuickCheck
import Control.Monad (mapM_)

main :: IO ()
main = do putStrLn "QUICKCHECK:"
          mapM_ quickCheck qcTests
          putStrLn "EXAMPLES:"
          mapM_ test unitTests

-- Properties

qcTests :: [Property]
qcTests = []

-- Examples

type Candidate = [(Name, Bool)] -- Candidate solution, as alist.

candToSol :: Candidate -> Maybe Solution
candToSol = Just . SAT . M.fromList

test :: (CNF Problem, [Candidate]) -> IO ()
test (prob, cands) = let sols = map candToSol cands
                         sol' = solve prob
                   in if any (sol' ==) (map candToSol cands) || (sol' == Nothing && null cands)
                      then return ()
                      else print $ "Expected one of " ++ show sols ++ ", got " ++ show sol'

unitTests :: [(CNF Problem, [Candidate])]
unitTests = [trivial, singletonPure, singletonNeg, lemish, lemish2, singletonsUnsolvable, refProb, refProbUnsat]

trivial = let prob = Problem [ ]
  in (prob, [[]])

singletonPure = let prob = Problem [ Clause [Pure "A"]]
  in (prob, [[("A", True)]])

singletonNeg  = let prob = Problem [ Clause [Neg  "A"]]
  in (prob, [[("A", False)]])

lemish = let prob = Problem [ Clause [Pure "A", Neg "A"]]
  in (prob, [[("A", False)], [("A", True)]])

lemish2 = let prob = Problem [ Clause [Pure "A", Neg "A"], Clause [Pure "B", Neg "A"]]
  in (prob, [[("A", False)], [("A", True), ("B", True)]])

singletonsUnsolvable = let prob = Problem [ Clause [Pure "B", Pure "A"], Clause [Neg "A"], Clause [Neg "B"]]
  in (prob, [])

refProb = let prob = problem [ clause [pur "A", pur "B"], clause [pur "C", pur "B"], clause [neg "B"]]
  in (prob, [[("B", False), ("C", True), ("A", True)]])

refProbUnsat = let prob = problem [ clause [pur "A", pur "B"], clause [pur "C", pur "B"], clause [neg "B"], clause [neg "A", neg "C"], clause [pur "B", neg "D"]]
  in (prob, [])
