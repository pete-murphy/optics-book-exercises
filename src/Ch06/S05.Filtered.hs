{-# LANGUAGE TemplateHaskell #-}

module Ch06.S05.Filtered where

import Control.Lens

-- A data structure to represent a single card
data Card
  = Card
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

data Move
  = Move
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
-- >>> all ((== 'S') . head) (map _name ex1)
-- True
ex1 :: [Card]
ex1 =
  deck ^.. folded
    . filteredBy (taking 1 (name . folded) . only 'S')

-- What’s the lowest attack power of all moves?
-- >>> ex2
-- Just 3
ex2 :: Maybe Int
ex2 = minimumOf (folded . moves . folded . movePower) deck

-- What’s the name of the first card which has more than one move?
-- >>> ex3
-- Just "Kapichu"
ex3 :: Maybe String
ex3 = firstOf (folded . filteredBy (moves . to length . filtered (> 1)) . name) deck

-- Are there any Hot cards with a move with more than 30 attack power?
-- >>> ex4
-- [Card {_name = "Spicyeon", _aura = Hot, _holo = False, _moves = [Move {_moveName = "Capsaicisize", _movePower = 40}]}]
ex4 :: [Card]
ex4 =
  deck ^.. folded
    . filteredBy (aura . only Hot)
    . filteredBy (moves . folded . movePower . filtered (> 30))

-- List the names of all holographic cards with a `Wet` aura.
-- >>> ex5
-- ["Garydose"]
ex5 :: [String]
ex5 =
  deck ^.. folded
    . filteredBy (holo . only True)
    . filteredBy (aura . only Wet)
    . name

-- What’s the sum of all attack power for all moves belonging to non-Leafy cards?
-- >>> ex6
-- 303
ex6 :: Int
ex6 =
  deck
    & sumOf
      ( folded
          . filtered ((/= Leafy) . _aura)
          . moves
          . folded
          . movePower
      )
