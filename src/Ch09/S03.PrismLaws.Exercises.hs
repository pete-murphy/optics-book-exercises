module Ch09.S03.PrismLaws.Exercises where

import qualified Data.Set as S
import Data.Set (Set)
import Control.Lens
import Data.Char

_Contains :: forall a. Ord a => a -> Prism' (Set a) (Set a)
_Contains x = prism' embed match
  where
    embed :: Set a -> Set a
    embed = S.insert x
    match :: Set a -> Maybe (Set a)
    match s 
      | x `S.member` s = Just (x `S.delete` s)
      | otherwise = Nothing

-- >>> import qualified Data.Set as S
-- >>> S.fromList [1, 2, 3] ^? _Contains 2 
-- Just (fromList [1,3])


-- This fails the first law
-- >>> import qualified Data.Set as S
-- >>> p = _Contains 2
-- >>> preview p (review p (S.fromList [1,2,3])) :: S.Set Int
-- Just (fromList [1,3])

-- | 2.
_Singleton :: forall a. Prism' [a] a
_Singleton = prism' embed match
  where
    match :: [a] -> Maybe a
    match [a] = Just a
    match _ = Nothing
    embed :: a -> [a]
    embed a = [a]

-- >>> p = _Singleton :: Prism' [Int] Int
-- >>> preview p (review p 3)
-- Just 3

-- | 3.
-- This fails the first law
_Upper :: Prism' String String
_Upper = prism' embed match
  where
    embed :: String -> String
    embed = map toUpper
    match :: String -> Maybe String
    match str = 
      if all isUpper str
        then Just str
        else Nothing
