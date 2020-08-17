{-# LANGUAGE RankNTypes #-}

module Ch06.S04.HigherOrderFolds.Exercises where

import Control.Lens
import Data.Char (isAlpha)

-- >>> "Here's looking at you, kid" ^.. ex1a 7 folded
-- "looking at you, kid"
ex1a :: Int -> Fold String Char -> Fold String Char
ex1a = dropping

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 ex1b
-- ["My","Hakuna","No"]
ex1b :: Fold String String
ex1b = worded

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. ex1c
-- ["My"]
ex1c :: Fold [String] String
ex1c = taking 1 (folded . worded)

-- No ex1d, errant copy-pasting

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . ex1e
-- "MyHakunaNo"
ex1e :: Fold String Char
ex1e = taking 1 worded . folded

-- >>> ex1f (10, 50, 100)
-- 60
ex1f :: (Int, Int, Int) -> Int
ex1f = sumOf (taking 2 each)

-- >>> ("stressed", "guns", "evil") ^.. ex1g each
-- ["evil","guns","stressed"]
ex1g :: Fold s a -> Fold s a
ex1g = backwards

-- >>> ("stressed", "guns", "evil") ^.. backwards each . to ex1h
-- ["live","snug","desserts"]
ex1h :: [a] -> [a]
ex1h = reverse

-- >>> import Data.Char (isAlpha)
-- >>> "blink182 k9 blazeit420" ^.. ex1i
-- "1829420"
ex1i :: Fold String Char
ex1i = worded . droppingWhile isAlpha folded

-- 2

sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

-- |
-- >>> ex2_1
-- 2
ex2_1 :: Int
ex2_1 = lengthOf (takingWhile (< 0) folded) sample

-- |
-- >>> ex2_2
-- Just 4
ex2_2 :: Maybe Int
ex2_2 = maximumOf (taking 4 folded) sample

-- |
-- >>> ex2_3
-- Just 3
ex2_3 :: Maybe Int
ex2_3 = sample ^? dropping 1 (droppingWhile (/= 4) folded)
-- (^?) = flip preview

-- Better way of doing this one??

-- |
-- >>> ex2_4
-- 2
ex2_4 :: Int
ex2_4 = lengthOf (takingWhile (< 0) (backwards folded)) sample

-- |
-- >>> ex2_5
-- [4,3,8,6]
ex2_5 :: [Int]
-- ex2_5 = sample ^.. takingWhile (>0) (droppingWhile (<0) folded)
-- ex2_5 = sample ^.. (takingWhile (>0) $ droppingWhile (<0) folded)
ex2_5 =
  sample ^.. do
    folded
      & droppingWhile (< 0)
      & takingWhile (> 0)

-- |
-- >>> ex2_bonus
-- [4,3,8,6,-2,3]
ex2_bonus :: [Int]
-- ex2_bonus = sample ^.. backwards (droppingWhile (< 0) (backwards (droppingWhile (< 0) folded)))
-- ^^^^ The example above seems gross to me. I also don't really like ($), hard to track where end of scope is.
-- I also imagine the below is sacrilege.
ex2_bonus = sample ^.. do
  folded
    & droppingWhile (< 0)
    & backwards
    & droppingWhile (< 0)
    & backwards
--  backwards (droppingWhile (< 0) (backwards (droppingWhile (< 0) folded)))

