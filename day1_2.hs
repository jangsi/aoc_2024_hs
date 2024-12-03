import Utils

-- first occurence via mid == 0
bSearch :: (Ord a) => a -> [a] -> Int -> Int -> Int
bSearch _ [] _ _ = -1
bSearch x xs low high
  | low > high = -1
  | xs !! mid < x = bSearch x xs (mid + 1) high
  | xs !! mid > x = bSearch x xs low (mid - 1)
  | otherwise = if mid == 0 || xs !! (mid - 1) < x
                then mid
                else bSearch x xs low (mid - 1)
  where
    mid = div (low + high) 2

countOccurrences :: (Ord a) => a -> [a] -> Int
countOccurrences _ [] = 0
countOccurrences x xs =
  if firstOccurrence == -1
    then 0
  else length $ takeWhile (== x) (drop firstOccurrence xs)
  where firstOccurrence = bSearch x xs 0 (length xs - 1)

scoreSimilarity :: Int -> [Int] -> Int
scoreSimilarity _ [] = 0
scoreSimilarity x ys = x * countOccurrences x ys

main :: IO ()
main = do
  (list1, list2) <- parseDay1 "./day1_input.txt"
  let l2' = qSort list2
  let score = sum (map (`scoreSimilarity` l2') list1)
  print score