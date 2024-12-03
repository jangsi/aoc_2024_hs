module Utils
( parseDay1
, qSort
, isMonotonic
, parseDay2
, isMostlyMonotonic
) where
import Data.Maybe (mapMaybe)

readLineDay1 :: String -> Maybe (Int, Int)
readLineDay1 line =
  if length ws == 2
    then Just (read (head ws), read (ws !! 1))
    else Nothing
  where ws = words line

parseDay1 :: FilePath -> IO ([Int], [Int])
parseDay1 filePath = do
  content <- readFile filePath
  let lists = mapMaybe readLineDay1 (lines content)
  let (list1, list2) = unzip lists
  return (list1, list2)

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) =
  let l = qSort [a | a <- xs, a <= x]
      r = qSort [a | a <- xs, a > x]
      in l ++ [x] ++ r

readLineDay2 :: String -> [Int]
readLineDay2 line = map read (words line)

isIncreasing :: Int -> [Int] -> Bool
isIncreasing tolerance xs = and [x < y && y - x <= tolerance | (x, y) <- zip xs (tail xs)]

isDecreasing :: Int -> [Int] -> Bool
isDecreasing tolerance xs = and [x > y && x - y <= tolerance | (x, y) <- zip xs (tail xs)]

isMonotonic :: Int -> [Int] -> Bool
isMonotonic tolerance xs = isIncreasing tolerance xs || isDecreasing tolerance xs

parseDay2 :: FilePath -> IO [[Int]]
parseDay2 filePath = do
  content <- readFile filePath
  let levels = map readLineDay2 (lines content)
  return levels

removeAt :: [a] -> Int -> [a]
removeAt xs i = take i xs ++ drop (i + 1) xs

isMostlyMonotonic :: Int -> [Int] -> Bool
isMostlyMonotonic tolerance xs =
  isMonotonic tolerance xs || any (isMonotonic tolerance . removeAt xs) [0..length xs - 1]