module Ch03.LensesAndRecords where

import Control.Lens

data Ship
  = Ship
      { _name :: String,
        _numCrew :: Int
      }
  deriving (Show)

makeLenses ''Ship

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew' :: Ship -> Int -> Ship
setNumCrew' (Ship n _) = Ship n

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew = ship {_numCrew = newNumCrew}

numCrew' :: Lens' Ship Int
numCrew' = lens getNumCrew setNumCrew

getName :: Ship -> String
getName = _name

setName' :: Ship -> String -> Ship
setName' (Ship _ n) m = Ship m n

setName :: Ship -> String -> Ship
setName ship newName = ship {_name = newName}

name' :: Lens' Ship String
name' = lens getName setName

purplePearl :: Ship
purplePearl = Ship {_name = "Purple Pearl", _numCrew = 38}
