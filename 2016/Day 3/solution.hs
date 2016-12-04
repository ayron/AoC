import Data.List (transpose)
import Data.List.Split (chunksOf)

main = do
  input <- readFile "input.txt"
  let xs = map ((map read) . words) (lines input) :: [[Int]]
  print $ length $ filter is_triangle xs
  print $ length $ filter is_triangle $ chunksOf 3 $ concat $ transpose xs

is_triangle (a:b:c:_) = a + b > c && b + c > a && c + a > b
