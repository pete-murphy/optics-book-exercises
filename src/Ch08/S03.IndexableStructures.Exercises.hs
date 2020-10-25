module Ch08.S03.IndexableStructures where

import Control.Lens
import Data.Map (Map)
import Data.Set (Set)

-- >>> ["Larry", "Curly", "Moe"] & ex1_1 1 .~ "Wiggly"
-- ["Larry","Wiggly","Moe"]
ex1_1 :: Ixed m => Index m -> Traversal' m (IxValue m)
ex1_1 = ix

-- >>> import qualified Data.Map as M
-- >>> heroesAndVillains = M.fromList [("Superman", "Lex1_"), ("Batman", "Joker")]
-- >>> heroesAndVillains & ex1_2 "Spiderman" .~ Just "Goblin"
-- fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex1_")]
ex1_2 :: String -> Traversal' (Map String String) (Maybe String)
ex1_2 = at

-- >>> import qualified Data.Map as M
-- >>> heroesAndVillains = M.fromList [("Superman", "Lex1_"), ("Batman", "Joker")]
-- >>> ex1_3 "Superman" heroesAndVillains
-- fromList [("Batman","Joker")]
ex1_3 :: String -> Map String String -> Map String String 
ex1_3 = sans

-- >>> import qualified Data.Set as S
-- >>> S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' `ex1_4` () & at 'i' .~ ex1_5
-- fromList "aeouy"
ex1_4 :: Traversal' (Set Char) (Maybe ()) -> () -> Set Char -> Set Char 
ex1_4 = (?~)

ex1_5 :: Maybe ()
ex1_5 = Nothing

-- | 2.
-- >>> import qualified Data.Map as M
-- >>> input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
-- >>> input & sans "gum" & at "ice cream" ?~ 5 & ix "soda" +~ 3
-- fromList [("candy bars",13),("ice cream",5),("soda",37)]

