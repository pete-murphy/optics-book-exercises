{-# LANGUAGE InstanceSigs, TemplateHaskell #-}
module Ch08.S04.CustomIndexedDataStructures where

import Control.Lens

newtype Cycled a = Cycled [a]
  deriving Show


type instance Index (Cycled _) = Int
type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  ix :: Int -> Traversal' (Cycled a) a
  ix i handler (Cycled xs) =
    Cycled <$> traverseOf (ix (i `mod` length xs)) handler xs

data Address
  = Address
      { _buildingNumber :: Maybe String,
        _streetName :: Maybe String,
        _apartmentNumber :: Maybe String,
        _postalCode :: Maybe String
      }
  deriving (Show)

makeLenses ''Address


data AddressPiece
  = BuildingNumber
  | StreetName
  | ApartmentNumber
  | PostalCode
  deriving (Show)

-- type instance Index Address = ??
type instance Index Address = AddressPiece
type instance IxValue Address = String

instance Ixed Address

instance At Address where
  at :: AddressPiece -> Lens' Address (Maybe String)
  at BuildingNumber = buildingNumber
  at StreetName = streetName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode


