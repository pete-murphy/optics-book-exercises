module Ch06.Ex where

import Control.Lens
import Control.Monad.State

newtype Game
  = Game
      { _player :: Player
      }
  deriving (Show)

newtype Player
  = Player
      { _score :: Int
      }
  deriving (Show)

makeLenses ''Game

makeLenses ''Player

p1 :: Player
p1 = Player 10

g1 :: Game
g1 = Game p1

-- (+=) = (+~)

loop :: State Game Game
loop = do
  game <- get
  put (game & player . score +~ 20)
  return game
