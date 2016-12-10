import Data.Array
import Data.List (unlines)

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec

type Screen = Array (Int, Int) Bool
type Update = (Screen -> Screen)

blank :: Screen
w = 50
h = 6
blank = listArray ((0, 0), (h-1, w-1)) $ repeat False
rs = [0..h-1]
cs = [0..w-1]

main = do
  result <- parseFromFile parse_file "input.txt"
  case result of
    Left err      -> print err
    Right actions -> do
      print $ length $ filter id $ elems final_screen
      print_array final_screen
      where final_screen = foldl f blank actions

f :: Screen -> Update -> Screen
f s a = a s

-- parsing functions

parse_file :: Parser [Update]
parse_file = many $ parse_line <* newline

parse_line :: Parser Update
parse_line = try parse_rect <|> 
             try parse_rotr <|> 
             parse_rotc

parse_rect :: Parser Update
parse_rect = do
  string "rect"
  spaces
  width <- parse_uint
  char 'x'
  height <- parse_uint
  return $ rect width height

parse_rotr :: Parser Update
parse_rotr = do
  string "rotate"
  spaces
  string "row"
  spaces
  string "y="
  row <- parse_uint
  string " by "
  n <- parse_uint
  return $ rotate_r row n

parse_rotc :: Parser Update
parse_rotc = do
  string "rotate"
  spaces
  string "column"
  spaces
  string "x="
  col <- parse_uint
  string " by "
  n <- parse_uint
  return $ rotate_c col n
  
parse_uint :: Parser Int
parse_uint = read <$> many1 digit

-- Screen update functions

rect :: Int -> Int -> Screen -> Screen
rect w h a = a // [((r,c), True) | r <- [0..h-1], c <- [0..w-1]]
  
rotate_c :: Int -> Int -> Screen -> Screen
rotate_c c n a = a // (zip col (rotate n xs))
  where col = zip rs (repeat c)
        xs = [a ! e | e <- col]

rotate_r :: Int -> Int -> Screen -> Screen
rotate_r r n a = a // (zip row (rotate n xs))
  where row = zip (repeat r) cs
        xs = [a ! e | e <- row]
        
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = reverse $ zipWith const (drop n (cycle (reverse xs))) xs

-- Function below are to help debug, but not needed
print_array :: Screen -> IO ()
print_array a = putStrLn $ unlines $ chunksOf width $ map show' $ elems a
  where width = 1 + (snd $ snd $ bounds a)

show' :: Bool -> Char
show' True  = 'O'
show' False = ' '

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
