module Ch03.VirtualField where

import Control.Lens

data Temperature
  = Temperature
      { _location :: String,
        _celsius :: Float
      }
  deriving (Show)

makeLenses ''Temperature

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter = \temp f -> set celsius (fahrenheitToCelsius f) temp

-- | Exercises
data User
  = User
      { _firstName :: String,
        _lastName :: String,
        _email :: String
      }
  deriving (Show)

makeLenses ''User

username :: Lens' User String
username = email

-- where
--   getter = view email
--   setter = \user str -> set email str user

fullName :: Lens' User String
fullName = lens getter setter
  where
    getter = \user -> view firstName user <> " " <> view lastName user
    setter = \user str ->
      let (firstName' : rest) = words str
          lastName' = unwords rest
       in set firstName firstName' (set lastName lastName' user)
