module Ch06.S03.FoldActions.Exercises where

import Control.Lens
import Data.Ord

-- |
-- >>> ex1a
-- False
ex1a :: Bool
ex1a = has folded []

-- |
-- >>> ex1b
-- "YoAdrian!"
ex1b :: String
ex1b = foldOf both ("Yo", "Adrian!")

-- |
-- >>> ex1c
-- True
ex1c :: Bool
ex1c = elemOf each "phone" ("E.T.", "phone", "home")

-- |
-- >>> ex1d
-- Just 2
ex1d :: Maybe Int
ex1d = minimumOf folded [5, 7, 2, 3, 13, 17, 11]

-- 2.

-- |
-- >>> ex2a ["umbrella", "olives", "racecar", "hammer"]
-- Just "racecar"
ex2a :: [String] -> Maybe String
ex2a = findOf folded isPalindrome
  where
    isPalindrome = (==) <$> id <*> reverse

-- |
-- >>> ex2b (2, 4, 6)
-- True
ex2b :: (Int, Int, Int) -> Bool
ex2b = allOf each even

-- |
-- >>> ex2c [(2, "I'll"), (3, "Be"), (1, "Back")]
-- Just (3,"Be")
ex2c :: [(Int, String)] -> Maybe (Int, String)
ex2c = maximumByOf folded (comparing fst)

-- |
-- >>> ex2d (1, 2)
-- 3
ex2d :: (Int, Int) -> Int
ex2d = sumOf each

-- 3.

-- |
-- >>> ex3a "Do or do not, there is no try."
-- Just "there"
ex3a :: String -> Maybe String
ex3a = maximumByOf (folding words) (comparing (length . filter (`elem` "aeiou")))

-- |
-- >>> ex3b ["a", "b", "c"]
-- "cba"
ex3b :: [String] -> String
ex3b = foldlOf (folded . folded) (flip (:)) ""

-- |
-- >>> ex3c [(12, 45, 66), (91, 123, 87)]
-- "54321"
ex3c :: [(Int, Int, Int)] -> String
ex3c = foldMapOf (folded . _2) (reverse . show)

-- |
-- >>> ex3d [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
-- ["b","d"]
ex3d :: [(Int, String)] -> [String]
ex3d =
 foldMapOf folded (\(n,x) -> if even n then [x] else [])
