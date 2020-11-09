module Ch10.S06.ProjectingIsos.Exercises where

import Control.Lens

-- | 1. Fill in the blank

-- >>> ("Beauty", "Age") ^. mapping reversed . swapped
-- ("egA","Beauty")
ex1 :: Iso' (a, b) (b, a)
ex1 = swapped

-- >>> [True, False, True] ^. mapping (ex2 not)
-- [False,True,False]
ex2 :: (Bool -> Bool) -> Iso' Bool Bool
ex2 = involuted

-- >>> [True, False, True] & ex3 %~ filter id
-- [False]
ex3 :: Iso' [Bool] [Bool]
ex3 = mapping (involuted not)

-- >>> (show ^. ex4) 1234
-- "4321"
ex4 :: Iso' (Int -> String) (Int -> String)
ex4 = mapping reversed

-- | 2. Using the enum iso provided by lens (type below):
-- enum :: Enum a => Iso' Int a
-- Implement the following intNot function, use dimapping in your implementation.

-- dimapping 
--   :: (Profunctor p)
--   => Iso' s a -> Iso' s' a'
--   -> Iso' (p a s') (p s a')

-- dimapping 
--   :: Iso' s a -> Iso' s' a'
--   -> Iso' (a -> s') (s -> a')

-- dimapping 
--   :: Iso' Int Bool -> Iso' Bool Int
--   -> Iso' (Bool -> Bool) (Int -> Int)

-- >>> intNot 0
-- 1
-- >>> intNot 1
-- 0
-- >>> intNot 2
-- Prelude.Enum.Bool.toEnum: bad argument

intNot :: Int -> Int
intNot = id ^. dimapping (enum . involuted not) (from enum)