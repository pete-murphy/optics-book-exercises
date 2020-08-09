-- {-# LANGUAGE OverloadedStrings #-}

module Ch06.S01.SimpleFolds.Exercises where

import Control.Lens
import qualified Data.Text as T

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- | 1 What’s the result of each expression? Make sure to guess before trying it
-- out in the repl!
--
-- My guesses
-- a. (first guess)
--    ["Sirens", "Kraken", "Ogopogo"]
-- a. (after looking at other questions)
--    [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
--    (Same as original)
--
-- b. ["Sirens", "Kraken", "Ogopogo"]
--
-- c. "SirensKrakenOgopogo"

-- Wait... goddammit, amending guesses
-- a. >>> beastSizes ^.. folded
-- b. >>> beastSizes ^.. folded . folded
-- c. >>> beastSizes ^.. folded . folded . folded
-- a, b, & c are all the same, since they're already lists
--    [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
--
-- >>> beastSizes ^.. folded . _2
-- d. ["Sirens", "Kraken", "Ogopogo"]
--
-- >>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
-- Guess: [1..6]
--
-- >>> toListOf
--      (folded . folded)
--      (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
-- Guess: "CaptainFirst Mate"

-- >>> ("Hello", "It's me") ^.. both . folded
-- Guess: "HelloIt's me"

-- >>> ("Why", "So", "Serious?") ^.. each
-- Guess: ["Why", "So", "Serious?"]

-- quotes :: [(T.Text, T.Text, T.Text)]
-- quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

--- >>> quotes ^.. each . each . each
--- "WhySoSerious?ThisisSPARTA"

-- | 2.
-- Write out the `specialized` type for each of the requested combinators used
-- in each of the following expressions.

-- folded and _1
-- >>> toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]
-- [1, 2, 3]
-- <Answer>
-- folded :: Foldable f => Fold (f a) a
--        :: Fold [(Int, Char)] (Int, Char)
-- _1 :: Lens' (Int, Char) Int
--    :: Fold (Int, Char) Int

-- folded, _2, and toListOf
-- >>> toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])
-- ["one", "two", "three"]

-- <Answer>
-- folded :: Fold (Set String) String
-- _2 :: Fold (Bool, Set String) -> Set String
-- toListOf :: Fold s a -> s -> [a]
--          :: Fold (Bool, Set String) String -> (Bool, Set String) -> [String]

-- folded and folded and toListOf
-- >>> toListOf
--       (folded . folded)
-- (M.fromList [("Jack", "Captain"), ("Will", "First Mate")]) "CaptainFirst Mate"

-- <Answer>
-- folded :: Fold (Map String String) String
-- folded :: Fold String Char
-- toListOf :: Fold s a -> s -> [a]
--          :: Fold (Map String String) -> [Char]
--          :: Fold (Map String String) -> String

ex3a :: [Int]
ex3a = [1, 2, 3] ^.. folded

ex3b :: [String]
ex3b = ("Light", "Dark") ^.. _1

-- should be ["Light"]

ex3c :: [String]
ex3c = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . each

-- should be ["Light","Dark","Happy","Sad"]

ex3d :: [String]
ex3d = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1

-- should be ["Light", "Happy"]

ex3e :: String
ex3e = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . folded . folded

-- should be "DarkSad"
-- Having difficulty with this one (was because of OverloadedStrings)

ex3f :: [String]
ex3f = ("Bond", "James", "Bond") ^.. each
