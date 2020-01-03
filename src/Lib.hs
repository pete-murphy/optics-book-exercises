module Lib
    ( module Control.Lens
    , module Control.Applicative
    , module Data.Char
    ) where

import Control.Lens
import Control.Applicative 
import Data.Char
import qualified Data.Map as M 
import qualified Data.Set as S 
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"
