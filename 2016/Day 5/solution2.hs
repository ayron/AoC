{-# LANGUAGE OverloadedStrings #-}

{- Note: This was best best try, but it is terribly inefficent.
 - TODO: Make better -}

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List (sortOn)
import Crypto.Hash

md5 :: B.ByteString -> String
md5 bs = show (hash bs :: Digest MD5)

--doorid = "abc"
doorid = "abbhdwsy"

--main = print $ md5 doorid

main = print $ sortOn (\(a, b) -> a) $ take 16 $ xs
  where hashes = filter is_interesting $ map myhash [0..]
        ps     = map (!! 5) hashes
        cs     = map (!! 6) hashes
        xs     = filter is_valid $ zip ps cs

is_valid (p, _) = isDigit p && p /= '8' && p /= '9'

is_interesting :: String -> Bool
is_interesting s = take 5 s == "00000"
        
myhash :: Int -> String
myhash x = md5 $ B.pack $ doorid ++ show x
