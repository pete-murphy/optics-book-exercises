module Ch07.S06.TraversalLaws.Exercises where

import Data.List
import Control.Lens
import Data.Char

-- | 1
-- >>> ex1a
-- "H E L L O W O R L D"
-- >>> ex1b
-- "H\nE\nL\nL\nO W\nO\nR\nL\nD"
ex1a :: String
ex1a = 
  "Hello world" 
    & worded %~ intersperse '\n'
    & worded %~ map toUpper

ex1b :: String
ex1b = 
  "Hello world" 
    & worded %~ (intersperse '\n' . map toUpper)

-- (Also breaks first law)

-- | 2
everyOthered :: Traversal' [a] a
--           Applicative f =>
--           (a -> f a) ->
--           [a] ->
--           f [a]
everyOthered f = sequenceA 
                 . concatMap (\(i, x) -> if odd i then [f x] else replicate i (pure x)) 
                 . zip [(1 :: Int)..]

-- | 3
swapped' :: Traversal' (a, a) a
swapped' f (x,y) = (,) <$> f y <*> f x

-- >>> ex3
-- False
ex3 :: Bool
ex3 = a == b
  where
    a = x & swapped' %~ f & swapped' %~ g
    b = x & swapped' %~ f . g
    x = (1, 2) :: (Int, Int)
    f = (+ 1)
    g = (* 10)

-- | 4
-- `taking`: seems to be lawful
-- `beside`: seems to be lawful
-- >>> ex4_2
-- True
ex4_2 :: Bool
ex4_2 = a == b
  where
    a = traverseOf (beside id traversed) pure x
    b = pure x :: [(Int, [Int])]
    x = (1,[2]) :: (Int, [Int])

-- each is lawful
-- lined is NOT lawful
-- traversed is lawful
