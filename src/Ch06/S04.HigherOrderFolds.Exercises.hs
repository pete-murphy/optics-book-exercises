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

-- >>> ex2_1
-- 2
ex2_1 :: Int
ex2_1 = undefined

-- >>> ex2_2
-- Just 4
ex2_2 :: Maybe Int
ex2_2 = undefined

-- >>> ex2_3
-- Just 3
ex2_3 :: Maybe Int
ex2_3 = undefined

-- >>> ex2_4
-- 2
ex2_4 :: Int
ex2_4 = undefined

-- >>> ex2_5
-- [4, 3, 8, 6]
ex2_5 :: [Int]
ex2_5 = undefined

-- >>> ex2_bonus
-- [4, 3, 8, 6, -2, 3]
ex2_bonus :: [Int]
ex2_bonus = undefined
