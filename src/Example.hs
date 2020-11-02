module Example where
  
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Lens

data MyData a = 
   B { unB :: a }
 | C { unC :: NonEmpty a}
 deriving (Show)

-- myLens :: Lens (MyData a) (MyData b) a b
myLens :: Lens' (MyData a) a
myLens = lens getter setter
  where
    getter = \case
      B x -> x
      C (x :| _) -> x
    setter arg h = case arg of
      B _ -> B h
      C _ -> C (h :| [])

-- >>> s = C (1 :| [2])
-- >>> set myLens (view myLens s) s
-- C {unC = 1 :| []}

-- >>> view myLens (B {unB = 0}) 
-- 0

-- >>> view myLens (C {unC = 0 :| [1]})
-- 0
