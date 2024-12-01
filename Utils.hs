module Utils
( readLine
, parse
, qSort
) where
import Data.Maybe (mapMaybe)

readLine :: String -> Maybe (Int, Int)
readLine line =
  if length ws == 2
    then Just (read (head ws), read (ws !! 1))
    else Nothing
  where ws = words line

parse :: FilePath -> IO ([Int], [Int])
parse filePath = do
  content <- readFile filePath
  let lists = mapMaybe readLine (lines content)
  let (list1, list2) = unzip lists
  return (list1, list2)

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) =
  let l = qSort [a | a <- xs, a <= x]
      r = qSort [a | a <- xs, a > x]
      in l ++ [x] ++ r