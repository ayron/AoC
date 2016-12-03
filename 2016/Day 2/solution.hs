import Data.Array

main = do
  input <- readFile "input.txt"
  print_code keypad1 rules1 start1 input
  print_code keypad2 rules2 start2 input

print_code keypad rules init input =
  print $ map (keypad !) $ tail $ scanl (foldl rules) init (lines input)


start1 = (0, 0)
keypad1 = listArray ((-1, -1), (1, 1)) 
          "123\
          \456\
          \789"

rules1 (y, x) c = 
  case c of
    'L' -> (y, max (x-1) (-1))
    'R' -> (y, min (x+1)   1)
    'U' -> (max (y-1) (-1), x)
    'D' -> (min (y+1)   1,  x)


start2 = (0, -2)
keypad2 = listArray ((-2, -2), (2, 2)) 
          "  1  \
          \ 234 \
          \56789\
          \ ABC \
          \  D  "

rules2 (y, x) c =    
  case c of
    'L' -> (y, max (x-1) (abs y - 2))
    'R' -> (y, min (x+1) (-abs y + 2))
    'U' -> (max (y-1) (abs x - 2), x)
    'D' -> (min (y+1) (-abs x + 2), x)
