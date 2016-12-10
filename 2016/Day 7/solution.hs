import Control.Monad (liftM)
import Data.List (unfoldr, isInfixOf)
import Data.List.Split (splitOneOf)

main = do
  ips <- liftM lines $ readFile "input.txt"
  print $ length $ filter supports_tls ips
  print $ length $ filter supports_ssl ips

supports_tls :: String -> Bool
supports_tls s = has_abba address && not (has_abba hypernet)
  where (address, hypernet) = seperate $ splitOneOf "[]" s

seperate :: [String] -> ([String], [String])
seperate ss = (odds, evens)
  where odds  = map snd $ filter fst xs
        evens = map snd $ filter (not . fst) xs
        xs    = zip (cycle [True, False]) ss

has_abba :: [String] -> Bool
has_abba ss = any has_abba' ss

has_abba' :: String -> Bool
has_abba' (a:b:c:d:cs)
  | null cs = is_abba a b c d
  | otherwise = is_abba a b c d || has_abba' (b:c:d:cs)
has_abba' _ = False

is_abba a b c d = a == d && b == c && a /= b

supports_ssl :: String -> Bool
supports_ssl s = or [isInfixOf aba hn | aba <- abas, hn <- hypernet]
  where (address, hypernet) = seperate $ splitOneOf "[]" s
        abas = concat $ map get_abas address

get_abas :: String -> [String]
get_abas (a:b:c:cs)
 | is_aba a b c = [b, a, b] : get_abas (b:c:cs)
 | otherwise    = get_abas (b:c:cs)
get_abas _      = []

is_aba a b c = a == c && a /= b
