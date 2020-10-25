module Ch09.S02.WritingCustomPrisms where

import Control.Lens
import Data.List (stripPrefix)

_Prefix :: String -> Prism' String String
_Prefix prefix = prism' embed match
  where
    match :: String -> Maybe String
    match s = stripPrefix prefix s
    embed :: String -> String
    embed s = prefix <> s

-- >>> "http://phishingscam.com" ^? _Prefix "https://"
-- Nothing

-- >>> "https://totallylegit.com" ^? _Prefix "https://"
-- Just "totallylegit.com"

-- We can even define new prisms using our existing one.
-- Only add our account number if the connection is secure!

-- This works in GHCi
-- >>> import Data.List (stripPrefix)
-- >>> _Prefix prefix = prism' (\s -> prefix <> s) (\s -> stripPrefix prefix s)
-- >>> _Secure = _Prefix "https://" :: Prism' String String
-- >>> "https://mybank.com" & _Secure <>~ "?accountNumber=12345"
-- "https://mybank.com?accountNumber=12345"
-- >>> "http://fakebank.com" & _Secure <>~ "?accountNumber=12345"
-- "http://fakebank.com"

-- | Cracking the coding interview: Prisms style!
_Factor :: Int -> Prism' Int Int
_Factor n = prism' embed match
  where
    embed :: Int -> Int
    embed i = i * n
    match :: Int -> Maybe Int
    match i
      | i `mod` n == 0 = Just (i `div` n)
      | otherwise = Nothing
