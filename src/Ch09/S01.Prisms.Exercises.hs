
module Ch09.S01.Prisms.Exercises where

import Control.Lens

data ContactInfo = 
  Email String |
  Telephone Int |
  Address String String String

makePrisms ''ContactInfo

-- | 1.
-- _Email :: Prism' ContactInfo String
-- _Email = prism' Email \case Email s -> Just s; _ -> Nothing

-- _Telephone :: Prism' ContactInfo Int
-- _Address :: Prism' ContactInfo (String, String, String)

-- | 2.
-- >>> Right 35 & ex2_1 +~ 5 :: Either String Int
-- Right 40
ex2_1 :: forall a. Prism' (Either a Int) Int
ex2_1 = _Right

-- >>> xs = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
-- >>> xs ^.. folded . ex2_2
-- ["Mind","Power","Soul","Time"]
ex2_2 :: Prism' (Maybe String) String
ex2_2 = _Just

-- >>> xs = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
-- >>> xs & ex2_3 <>~ " Stone"
-- [Just "Mind Stone",Just "Power Stone",Nothing,Just "Soul Stone",Nothing,Just "Time Stone"]
ex2_3 :: Traversal' [Maybe String] String
ex2_3 = traversed . _Just

-- >>> Left (Right True, "Eureka!") & ex2_4 %~ not :: Either (Either Int Bool, String) Int
-- Left (Right False,"Eureka!")
ex2_4 :: Traversal' (Either (Either a Bool, String) b) Bool
ex2_4 = _Left . _1 . _Right
-- ^^^ Why can't this be a Prism'?

-- >>> _Cons `ex2_5` ("Do",["Re", "Mi"]) :: [String]
-- ["Do","Re","Mi"]
ex2_5 :: Prism' [String] (String, [String]) -> (String, [String]) -> [String]
ex2_5 = (#)

-- >>> isn't (_Show :: Prism' String Int) "not an int"
-- True

-- | 3.

-- >>> input = (Just 1, Nothing, Just 3)
-- >>> input ^.. each . _Just
-- [1,3]

-- >>> input = ('x', "yz")
-- >>> input & _2 %~ reverse & review _Cons
-- "xzy"

-- >>> input = "do the hokey pokey"
-- >>> (_Left . _Just . _Right) # input :: Either (Maybe (Either Int String)) Int
-- Left (Just (Right "do the hokey pokey"))
