import Utils

main :: IO ()
main = do
  levels <- parseDay2 "./day2_input.txt"
  let numSafe = length (filter (isMostlyMonotonic 3) levels)
  print numSafe