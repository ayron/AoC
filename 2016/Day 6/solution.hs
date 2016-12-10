import Data.List (maximumBy, minimumBy, transpose, group, sort)
import Data.Function (on)
import Control.Monad (liftM)

main = do
  columns <- liftM (transpose . lines) (readFile "input.txt")
  putStrLn $ map mode columns
  putStrLn $ map lode columns

mode = head . (maximumBy $ on compare length) . group . sort
lode = head . (minimumBy $ on compare length) . group . sort
