import System.IO
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