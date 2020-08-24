module Ch07.S03.SimpleTraversals.Exercises where

import Control.Lens

-- >>> ("Jurassic", "Park") & ex2_1 .~ "N/A" 
-- ("N/A","N/A")
ex2_1 :: Traversal' (String, String) String
ex2_1 = both

-- >>> ("Jurassic", "Park") & both . ex2_2 .~ 'x' 
-- ("xxxxxxxx","xxxx")
ex2_2 :: Traversal' String Char
ex2_2 = traversed

-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & ex2_3 id traversed %~ take 3
-- ("Mal",["Kay","Ina","Jay"])
ex2_3 :: Traversal' s a -> Traversal' s' a -> Traversal' (s, s') a
ex2_3 = beside

-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . ex2_4 1 .~ "River"
-- ("Malcolm",["Kaylee","River","Jayne"])
ex2_4 :: Traversable t => Int -> Traversal' (t a) a
ex2_4 = element

-- | This one's tricky!
-- >>> ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . ex2_5 1 . traversed .~ 'x'
-- ["Die xxxxxxx Day","Live xxx Let Die","You xxxx Live Twice"]
ex2_5 :: Int -> Traversal' String String
ex2_5 = elementOf worded

-- | A bit tougher now!
-- >>> ((1, 2), (3, 4)) & ex2_6 +~ 1
-- ((2,3),(4,5))
ex2_6 :: Traversal' ((Int, Int), (Int, Int)) Int
ex2_6 = both . both

-- >>> (1, (2, [3, 4])) & ex2_7 +~ 1
-- (2,(3,[4,5]))
ex2_7 :: Traversal' (Int, (Int, [Int])) Int
ex2_7 = beside id (beside id traversed)

-- >>> import Data.Char (toUpper)
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & ex2_8 %~ toUpper
-- ((True,"STRAWberries"),(False,"Blueberries"),(True,"BLACKberries"))
ex2_8 :: Traversal' ((Bool, String), (Bool, String), (Bool, String)) Char
ex2_8 = each . filteredBy (_1 . only True) . traversed . taking 5 traversed

-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & ex2_9
-- ("Strawberries","Blueberries","Blackberries")
ex2_9 :: ((Bool, String), (Bool, String), (Bool, String)) -> (String, String, String)
ex2_9 = each %~ snd