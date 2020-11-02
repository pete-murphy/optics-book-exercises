module Ch10.S05.IntroToIsos.Exercises where

import Control.Lens
import Data.Char
import Numeric.Lens

-- | 1. For each of the following tasks, choose whether it’s best suited to a
-- Lens, Traversal, Prism, or Iso:

-- • Focus a Celsius temperature in Fahrenheit
--   Iso
-- • Focus the last element of a list
--   Traversal
-- • View a JSON object as its corresponding Haskell Record 
--   Prism?
-- • Rotate the elements of a three-tuple one to the right
--   Iso
-- • Focus on the ‘bits’ of an Int as Bools.
--   Iso? but I don't know much about computers
--   (I was wrong, either a Traversal or Prism)
-- • Focusing an IntSet from a Set Int
--   Iso

-- | 2. Fill in the blank

-- >>> ("Beauty", "Age") ^. ex1
-- ("Age","Beauty")
ex1 :: Iso' (a, a) (a, a)
ex1 = swapped

-- >>> import Numeric.Lens (adding)
-- >>> 50 ^. ex2 (adding 10)
-- 40
ex2 :: Iso' a b -> Iso' b a
ex2 = from

-- >>> import Numeric.Lens (multiplying)
-- >>> 0 & multiplying ex3 +~ 12
-- 3.0
ex3 :: Double
ex3 = 4

-- >>> import Numeric.Lens (adding, multiplying)
-- >>> 0 & adding 10 . multiplying 2 .~ ex4
-- 2.0
ex4 :: Num a => a
ex4 = 24

-- >>> [1,2,3] & reversed %~ ex5
-- [1,2]
ex5 :: [a] -> [a]
ex5 = drop 1

-- >>> (view ex6 (++)) [1, 2] [3, 4]
-- [3,4,1,2]
ex6 :: Iso' ([a] -> [a] -> [a]) ([a] -> [a] -> [a])
ex6 = flipped

-- >>> [1, 2, 3] `ex7` reversed
-- [3,2,1]
ex7 :: [a] -> Iso' [a] [a] -> [a]
ex7 = (^.)

-- | Bonus

-- >>> import Data.List (transpose)
-- >>> [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ ex8
-- [[2,3],[20,30]]
ex8 :: [[a]] -> [[a]]
ex8 = drop 1

-- >>> import Data.Char (isUpper, toUpper, toLower)
-- >>> switchCase c = if isUpper c then toLower c else toUpper c 
-- >>> (32, "Hi") & _2 . ex9 .~ ("hELLO" :: String)
-- (32,"Hello")
ex9 :: Iso' String String
ex9 = involuted (map switchCase)

switchCase c = if isUpper c then toLower c else toUpper c 

-- | 3. You can convert from Celsius to Fahrenheit using the following formula:

celsiusToF :: Double -> Double
celsiusToF c = (c * (9/5)) + 32

fahrenheit :: Iso' Double Double
fahrenheit = multiplying (9/5) . adding 32