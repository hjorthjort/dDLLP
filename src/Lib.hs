module Lib
    ( solve
    , example1
    , example2
    , example3
    ) where

import DLLP
import Control.Monad.Reader (runReader)

solve :: CNF Problem -> Maybe Solution
solve = runReader dllp . initialize

  -- Examples
example1 = let prob = Problem [ Clause [Pure "A", Neg "A"]]
  in (prob, initialize prob)

example2 = let prob = Problem [ Clause [Pure "A", Neg "A"], Clause [Pure "B", Neg "A"]]
  in (prob, initialize prob)

example3 = let prob = Problem [ Clause [Pure "B", Pure "A"], Clause [Neg "A"], Clause [Neg "B"]]
  in (prob, initialize prob)
