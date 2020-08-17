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


-- | List all the cards whose name starts with 'S'.
-- >>> ex1 deck
--
ex1 :: [Card]
ex1 = undefined

-- | What’s the lowest attack power of all moves?
-- >>> ex2 deck
--
ex2 :: Int
ex2 = undefined

-- | What’s the name of the first card which has more than one move?
-- >>> ex3 deck
--
ex3 :: String
ex3 = undefined

-- | Are there any Hot cards with a move with more than 30 attack power?
-- >>> ex4 deck
-- 
ex4 :: Bool
ex4 = undefined

-- | Are there any `Hot` cards with a move with more than 30 attack power?
-- >>> ex5 deck
--
ex5 :: Bool
ex5 = undefined

-- | List the names of all holographic cards with a `Wet` aura.
-- >>> ex6 deck
--
ex6 :: [String]
ex6 = undefined

-- | What’s the sum of all attack power for all moves belonging to non-Leafy cards?
-- >>> ex7 deck
--
ex7 :: Int
ex7 = undefined

