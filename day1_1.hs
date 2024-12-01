import Utils

distance :: [Int] -> [Int] -> Int
distance [] [] = 0
distance (x:xs) (y:ys) = abs (x - y) + distance xs ys
distance _ _ = error "lists have different lengths"

main :: IO ()
main = do
  (list1, list2) <- parse "./day1_input.txt"
  let l1' = qSort list1
  let l2' = qSort list2
  let d = distance l1' l2'
  print d