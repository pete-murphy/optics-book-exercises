{-# LANGUAGE PartialTypeSignatures #-}

module Ch06.S03.FoldActions.Exercises where

import Control.Lens

-- |
-- >>> "Here's looking at you, kid" ^.. ex1a 7 folded
-- "looking at you, kid"
ex1a :: _
ex1a = undefined

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 ex1b
-- ["My","Hakuna","No"]
ex1b :: String
ex1b = foldOf both ("Yo", "Adrian!")

