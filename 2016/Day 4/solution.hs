import Data.Char (isDigit, isLetter, ord, chr)
import Data.List (group, sort, sortOn, isInfixOf)

main = do
  input <- readFile "input.txt"
  let rooms      = map parse $ lines input
  let real_rooms = filter checksum_match rooms
  print $ sum $ map room_id real_rooms
  print $ filter (contains "north") $ map decrypt real_rooms

type Room = (String, Int, String)

room_id :: Room -> Int
room_id (_, n, _) = n

contains ss (s, _) = isInfixOf ss s

parse :: String -> Room
parse s = (name, id, cs)
  where (a, b) = break isDigit s
        (c, d) = break isLetter b
        name = init a
        id   = read $ init c
        cs   = init d

checksum_match :: Room -> Bool
checksum_match (s, _, cs) = cs == cs'
  where cs' = take 5 $ map head $ sortOn (negate . length) $ group $ sort $ filter isLetter s

decrypt :: Room -> (String, Int)
decrypt (s, n, _) = (map (shift n) s, n)

shift n '-' = ' '
shift n c   = to_char $ mod (to_int c + n) 26

to_int  c = ord c - 97
to_char x = chr $ 97 + x
