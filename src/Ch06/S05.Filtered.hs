module Ch06.S05.Filtered where

import Control.Lens

-- A data structure to represent a single card
data Card = Card
  { _name :: String,
    _aura :: Aura,
    _holo :: Bool,
    _moves :: [Move]
  }
  deriving (Show, Eq)

-- Each card has an aura type
data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Show, Eq)

data Move = Move
  { _moveName :: String,
    _movePower :: Int
  }
  deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck =
  [ Card "Skwortul" Wet False [Move "Squirt" 20],
    Card "Scorchander" Hot False [Move "Scorch" 20],
    Card "Seedasaur" Leafy False [Move "Allergize" 20],
    Card "Kapichu" Spark False [Move "Poke" 10, Move "Zap" 30],
    Card "Elecdude" Spark False [Move "Asplode" 50],
    Card "Garydose" Wet True [Move "Gary's move" 40],
    Card "Moisteon" Wet False [Move "Soggy" 3],
    Card "Grasseon" Leafy False [Move "Leaf Cut" 30],
    Card "Spicyeon" Hot False [Move "Capsaicisize" 40],
    Card "Sparkeon" Spark True [Move "Shock" 40, Move "Battery" 50]
  ]

