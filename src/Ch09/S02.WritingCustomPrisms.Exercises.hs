module Ch09.S02.WritingCustomPrisms.Exercises where

import Control.Lens

-- | 1. This Prism is not reversible (so not a Prism)

_Tail :: Prism' [a] [a]
_Tail = prism' embed match
  where
    embed :: [a] -> [a]
    embed = id -- Lossy
    match :: [a] -> Maybe [a] 
    match [] = Nothing
    match (_:xs) = Just xs
    
-- | 2. 

_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where
    embed :: (b, [b]) -> [b]
    embed (x, xs) = x : xs
    match :: [a] -> Either [b] (a, [a])
    match [] = Left []
    match (x:xs) = Right (x, xs)

-- | 3.

_Cycles :: forall a. Eq a => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where
    embed :: [a] -> [a]
    embed = concat . replicate n
    match :: [a] -> Maybe [a]
    match xs = case length xs `divMod` n of
      (m, 0) -> 
        if xs == concat (replicate n (take m xs))
          then Just (take m xs)
          else Nothing
      _ -> Nothing

