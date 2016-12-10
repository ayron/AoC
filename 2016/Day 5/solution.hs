{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as B
--import Crypto.Hash.MD5 as MD5
import Data.Digest.Pure.MD5

--doorid = "abc"
doorid = "abbhdwsy"


main = do
  print  $ password 0 8
   
password :: Int -> Int -> String
password x n
  | n == 0  = ""
  | otherwise =
    case take 5 hash of
      "00000" -> hash !! 5 : password (x+1) (n-1)
      _       -> password (x+1) n
  where hash = show $ md5 $ B.pack $ doorid ++ show x
