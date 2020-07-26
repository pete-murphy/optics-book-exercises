module Ch05.S02.View where

import Control.Lens

data Payload
  = Payload
      { _weightKilos :: Int,
        _cargo :: String
      }
  deriving (Show)

makeLenses ''Payload

newtype Ship
  = Ship
      { _payload :: Payload
      }
  deriving (Show)

makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")
