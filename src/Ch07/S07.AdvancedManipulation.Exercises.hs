module Ch07.S07.AdvancedManipulation.Exercises where

import Control.Lens

-- >>> [1, 2, 3, 4] ^. partsOf (traversed . filtered even) == ex1
-- True
ex1 = [2,4]

-- >>> ex2 == ["Aardvark", "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed)
-- True
ex2 = "AarBanCap"

-- >>> import qualified Data.Map as M
-- >>> ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf ex3
-- [1,2,3,4]
ex3 :: (Traversable f, Traversable g) => Traversal' (f a, g a) a
ex3 = beside traversed traversed

-- >>> [1, 2, 3, 4] & partsOf (traversed . ex4) .~ [20, 40]
-- [1,20,3,40]
ex4 :: Traversal' Int Int
ex4 = filtered even

-- >>> import qualified Data.Map as M
-- >>> M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed %~ ex5
-- fromList [('a','b'),('b','c'),('c','a')]
ex5 :: [a] -> [a]
ex5 [a,b,c] = [b,c,a]

-- >>> ('a', 'b', 'c') & ex6 %~ reverse 
-- ('c','b','a')
ex6 :: Lens' (a, a, a) [a]
-- partsOf :: Traversal' s a -> Lens' s [a]
ex6 = partsOf each

-- >>> [1, 2, 3, 4, 5, 6] & partsOf ex7 %~ reverse
-- [3,2,1,4,5,6]
ex7 :: Traversal' [a] a
ex7 = taking 3 traversed
