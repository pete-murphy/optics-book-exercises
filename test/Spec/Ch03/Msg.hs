{-# LANGUAGE TemplateHaskell #-}

module Spec.Ch03.Msg where

import Ch03.Msg
import Control.Lens
import Hedgehog
  ( (===),
    Gen,
    Property,
    checkParallel,
    discover,
    forAll,
    property,
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range

main :: IO ()
main =
  defaultMain
    [checkParallel $$(discover)]

prop_setGetErr :: Property
prop_setGetErr =
  property do
    e <- forAll genErr
    s <- forAll do Gen.string (Range.linear 0 100) Gen.alpha
    view msg (set msg s e) === s

genErr :: Gen Err
genErr =
  let s = Gen.string (Range.linear 0 100) Gen.alpha
      n = Gen.int (Range.linear 0 100)
   in Gen.choice [ReallyBadError <$> s, ExitCode <$> n]
