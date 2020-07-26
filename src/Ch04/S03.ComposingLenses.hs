module Ch04.S03.ComposingLenses where

import Control.Lens

data Person
  = Person
      { _name :: String,
        _address :: Address
      }
  deriving (Show)

data Address
  = Address
      { _streetAddress :: StreetAddress,
        _city :: String,
        _country :: String
      }
  deriving (Show)

data StreetAddress
  = StreetAddress
      { _streetNumber :: String,
        _streetName :: String
      }
  deriving (Show)

makeLenses ''Person

makeLenses ''Address

makeLenses ''StreetAddress

sherlock :: Person
sherlock =
  Person
    { _name = "S. Holmes",
      _address =
        Address
          { _streetAddress =
              StreetAddress
                { _streetNumber = "221B",
                  _streetName = "Baker Street"
                },
            _city = "London",
            _country = "England"
          }
    }

setStreetNumber :: String -> Person -> Person
setStreetNumber = set do address . streetAddress . streetNumber

data Player = Player deriving (Show)

data Wool = Wool deriving (Show)

data Sweater = Sweater deriving (Show)

data Item a
  = Item
      { _material :: a,
        _amount :: Int
      }
  deriving (Show)

makeLenses ''Item

weave :: Wool -> Sweater
weave Wool = Sweater

gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

itemLens :: Lens (x, Item a) (x, Item b) a b
itemLens = _2 . material

-- | Exercises
-- 1
galileo = view solution ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
  where
    solution = _2 . _1 . _1

-- 2
dominoTrain :: Lens' Five Three
dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino
  where
    fiveEightDomino :: Lens' Five Eight
    fiveEightDomino = undefined
    mysteryDomino :: Lens' Eight Two -- Solution
    mysteryDomino = undefined
    twoThreeDomino :: Lens' Two Three
    twoThreeDomino = undefined

data Two

data Three

data Five

data Eight
-- 3
-- Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
-- Lens Armadillo Hedgehog Platypus BabySloth
-- I got this one wrong, should have been:
-- Lens Platypus BabySloth Armadillo Hedgehog
