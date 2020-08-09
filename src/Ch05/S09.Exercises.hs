module Ch05.S09.Exercises where

import Control.Lens
import Data.Char (toUpper)

-- | 1.
data Gate
  = Gate
      { _open :: Bool,
        _oilTemp :: Float
      }
  deriving (Show)

makeLenses ''Gate

data Army
  = Army
      { _archers :: Int,
        _knights :: Int
      }
  deriving (Show)

makeLenses ''Army

data Kingdom
  = Kingdom
      { _name :: String,
        _army :: Army,
        _gate :: Gate
      }
  deriving (Show)

makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom "Duloc" (Army 22 14) (Gate True 10.0)

infix 2 \!~

(\!~) :: s -> Lens' s Bool -> s
s \!~ l = over l not s

goalA :: Kingdom
goalA =
  duloc
    & name <>~ ": a perfect place"
    & (\!~ gate . open)
    & army . knights +~ 28

goalB :: Kingdom
goalB =
  duloc
    & name <>~ "instein"
    & army . archers -~ 5
    & army . knights +~ 12
    & gate . oilTemp **~ 2

goalC :: (String, Kingdom)
goalC =
  duloc
    & name <>~ ": Home"
    & gate . oilTemp //~ 2
    & name <<<>~ " of the talking Donkeys"

-- | 2.
ex2a :: (Bool, String)
ex2a =
  (False, "opossums") & _1 ||~ True

ex2b :: ((Bool, String), Float)
ex2b =
  ((True, "Dudley"), 55.0)
    & _1 . _2 <>~ " - the worst"
    & _2 -~ 15
    & _2 //~ 2
    & _1 . _2 %~ map toUpper
    & _1 . _1 .~ False

-- | 3.
-- `view` only takes two arguments, `Lens s a` and `s`
-- (Actual: `s -> Getting a s a -> a`)

-- | 4.
-- What’s the type signature of %∼? Try to figure it without checking! Look at
-- the examples above if you have to.
--
-- First guess:
-- ```
-- (%~) :: Lens s a -> (a -> b) -> s -> t
-- ```
--
-- Actual
-- ```
-- (%~) :: ASetter s t a b -> (a -> b) -> s -> t
-- ```
