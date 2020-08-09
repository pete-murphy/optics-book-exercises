{-# LANGUAGE PartialTypeSignatures #-}

module Ch06.S03.FoldActions.Exercises where

import Control.Lens

-- |
-- >>> "Here's looking at you, kid" ^.. ex1a 7 folded
-- "looking at you, kid"
ex1a :: Int -> Fold String String
ex1a = undefined

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 ex1b
-- ["My","Hakuna","No"]
ex1b :: Fold String String
ex1b = undefined

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. ex1c
-- ["My"]
ex1c :: Fold _ _
ex1c = undefined

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . ex1d
-- ["My"]
ex1d :: Fold _ _
ex1d = undefined

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . ex1e
-- "MyHakunaNo"
ex1e :: Fold _ _
ex1e = undefined

-- >>> ex1f (10, 50, 100)
-- 60
ex1f :: Fold _ _
ex1f = undefined

-- >>> ("stressed", "guns", "evil") ^.. ex1g each
-- ["evil","guns","stressed"]
ex1g :: Fold _ _
ex1g = undefined

-- >>> ("stressed", "guns", "evil") ^.. backwards each . to ex1h
-- ["live","snug","desserts"]
ex1h :: _
ex1h = undefined

-- >>> import Data.Char (isAlpha)
-- >>> "blink182 k9 blazeit420" ^.. ex1i
-- "1829420"
ex1i :: _
ex1i = undefined

