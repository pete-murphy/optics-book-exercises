module Ch06.S05.Filtered where

import Data.Lens

data Card = Card
  { _name :: String,
    _aura :: Aura,
    _holo :: Bool,
    _moves :: [Move] }
    deriving (Show, Eq)
