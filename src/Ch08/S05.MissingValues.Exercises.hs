module Ch08.S05.MissingValues.Exercises where

import Data.Map (Map)
import Control.Lens

-- >>> import qualified Data.Map as M
-- >>> M.fromList [("first", False), ("second", False)] & ex1 .~ True
-- fromList [("first",True),("second",False)]
ex1 :: Traversal' (Map String Bool) Bool
ex1 = ix "first" `failing` ix "second"

-- >>> (1, 1) & ex2 *~ 10
-- (1,10)
-- >>> (2, 2) & ex2 *~ 10
-- (20,2)
ex2 :: Traversal' (Int, Int) Int
ex2 = _1 . filtered even `failing` _2

-- >>> [1..4] ^.. ex3
-- [2,4]
-- >>> [1, 3, 5] ^.. ex3
-- [1,3,5]
ex3 :: Traversal' [Int] Int
ex3 = traversed . filtered even `failing` traversed

-- >>> Nothing ^. non "default"
-- "default"

-- >>> Nothing & non 100 +~ 7
-- Just 107

-- >>> import qualified Data.Map as M
-- >>> M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)] ^. at "Broccoli" . non False
-- False

-- >>> import qualified Data.Map as M
-- >>> M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)] & at "Wario's Woods" . non 0 +~ 999
-- fromList [("Breath of the wild",22000000),("Odyssey",9070000),("Wario's Woods",999)]
