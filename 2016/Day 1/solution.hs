import Control.Monad (liftM)
import Data.List.Split (splitOn)
import Data.List (unzip4, find)

type State = (Int, Int, Int, Int)   -- heading x, y & position x, y

main = do
  instructions <- liftM expand_instructions (readFile "input.txt")
  
  let x = apply_instructions initial_state instructions

  putStrLn $ show $ x
  putStrLn $ show $ part2 x

    where initial_state = (0, 1, 0, 0)

expand_instructions :: String -> String
expand_instructions s = concat $ map expand $ splitOn ", " s
  where expand (x:xs) = x : replicate (read xs - 1) 'F'

apply_instructions :: State -> [Char] -> [State]
apply_instructions = scanl move

move :: State -> Char -> State
move (a, b, c, d) x =
  case x of
    'L' -> (-b,   a,  c-b,   d+a)
    'R' -> ( b,  -a,  c+b,   d-a)
    'F' -> ( a,   b,  c+a,   d+b)

part2 ss = find is_repeated ps
  where (_, _, a, b) = unzip4 ss
        ps = zip a b
        is_repeated p = 1 < length (filter (p ==) ps)

-- Thoughts:
-- Perhaps a State monad version would be better:

--type Position = (Int, Int)
--type Heading  = (Int, Int)

--type PState a = State (Position, Heading) a

--move :: Char -> PState Position
--move c = ... TODO

