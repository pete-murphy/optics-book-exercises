{-# LANGUAGE BlockArguments #-}

module Ch06.S02.CustomFolds where

import Control.Lens
import qualified Data.Set as S

-- Exercises

-- 1.
-- >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . _
-- "YerawizardHarry"
ex1a :: String
ex1a = ["Yer", "a", "wizard", "Harry"] ^.. folded . folded

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
-- [1, 2, 4, 5]
ex1b :: [Int]
ex1b = [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
-- [[1,2], [4,5]]
ex1c :: [[Int]]
ex1c = [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)

-- >>> ["bob", "otto", "hannah"] ^.. folded . _ reverse
-- ["bob", "otto", "hannah"]
ex1d :: [String]
ex1d = ["bob", "otto", "hannah"] ^.. folded . to reverse

-- >>> ("abc", "def") ^.. _ (\(a, b) -> [a, b]). _ reverse . _
-- "cbafed"
ex1e :: String
ex1e = ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded

-- 2.
-- >>> [1..5] ^.. _
-- [100,200,300,400,500]
ex2a :: [Int]
ex2a = [1..5] ^.. folded . to (* 100)

-- >>> (1, 2) ^.. _
-- [1, 2]
ex2b :: [Int]
ex2b = (1, 2) ^.. both

-- >>> [(1, "one"), (2, "two")] ^.. _
-- ["one", "two"]
ex2c :: [String]
ex2c = [(1 :: Int, "one"), (2, "two")] ^.. folded . folded

-- >>> (Just 1, Just 2, Just 3) ^.. _
-- [1, 2, 3]
ex2d :: [Int]
ex2d = (Just 1, Just 2, Just 3) ^.. each . folded

-- >>> [Left 1, Right 2, Left 3] ^.. _
-- [2]
ex2e :: [Int]
ex2e = [Left (1 :: Int), Right 2, Left 3] ^.. folded . folded
-- This one was interesting, first case of `folded` acting
-- differently from `map`

-- >>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. _
-- [1, 2, 3, 4, 5, 6, 7, 8]
ex2f :: [Int]
ex2f = [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . both . folded

-- >>> [1, 2, 3, 4] ^.. _
-- [Left 1, Right 2, Left 3, Right 4]
ex2g :: [Either Int Int]
ex2g =
  [1, 2, 3, 4]
    ^.. folded
    . to \x -> if even x then Right x else Left x

-- >>> [(1, (2, 3)), (4, (5, 6))] ^.. _
-- [1, 2, 3, 4, 5, 6]
ex2h :: [Int]
ex2h =
  [(1, (2, 3)), (4, (5, 6))]
    ^.. folded
    . folding \(x, (y, z)) -> [x, y, z]

-- >>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. _
-- [1, 2]
ex2i :: [Int]
ex2i = 
  [(Just 1, Left "one"), (Nothing, Right 2)]
    ^.. folded
    . folding (\(x, y) -> [x, hush y])
    . folded
  where
    hush :: Either a b -> Maybe b
    hush = either (\_ -> Nothing) Just

-- >>> [(1, "one"), (2, "two")] ^.. _
-- [Left 1, Right "one", Left 2, Right "two"]
ex2j :: [Either Int String]
ex2j = 
  [(1, "one"), (2, "two")]
    ^.. folded
    . folding \(x, y) -> [Left x, Right y]

-- >>> S.fromList ["apricots", "apples"] ^.. _
-- "selppastocirpa"
ex2k :: String
ex2k = 
  S.fromList ["apricots", "apples"]
    ^.. folded
    . folding reverse

-- >>> [(12, 45, 66), (91, 123, 87)] ^.. _
-- "54321"
ex2l :: String
ex2l =
  [(12, 45, 66), (91, 123, 87)]
    ^.. folded
    . _2
    . folding (reverse . show)

-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. _
-- ["b", "d"]
ex2m :: [String]
ex2m =
  [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
    ^.. folded . folding (\(n, s) -> if even n then [s] else [])

