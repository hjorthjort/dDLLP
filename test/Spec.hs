import DLLP

main :: IO ()
main = putStrLn "Test suite not yet implemented"

example1 = let prob = Problem [ Clause [Pure "A", Neg "A"]]
  in (prob, initialize prob)

example2 = let prob = Problem [ Clause [Pure "A", Neg "A"], Clause [Pure "B", Neg "A"]]
  in (prob, initialize prob)

example3 = let prob = Problem [ Clause [Pure "A", Pure "B"], Clause [Neg "B"], Clause [Neg "A"]]
  in (prob, initialize prob)
