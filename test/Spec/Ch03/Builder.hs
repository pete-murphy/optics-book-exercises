{-# LANGUAGE TemplateHaskell #-}

module Spec.Ch03.Builder where

import Ch03.Laws
import Control.Lens
import Hedgehog
  ( (===),
    Gen,
    Property,
    Range,
    checkParallel,
    discover,
    forAll,
    property,
  )
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import Spec.Lib

main :: IO ()
main =
  defaultMain
    [checkParallel $$(discover)]

instance Arg Char

instance Vary Char

builderGetSet :: Property
builderGetSet =
  property do
    str <- forAll do genString
    strs <- forAll do Gen.list (Range.linear 0 100) genString
    f <- forAllFn (fn genString)
    let b = Builder strs f
    -- Is this possible to implement?
    set builder (view builder b) b === undefined
