{-# LANGUAGE TemplateHaskell #-}

module Spec.Ch03.Msg where

import Ch03.Msg
import Hedgehog
  ( Gen,
    Property,
    checkParallel,
    discover,
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import Spec.Lib

main :: IO ()
main =
  defaultMain
    [checkParallel $$(discover)]

genErr :: Gen Err
genErr =
  let s = Gen.string (Range.linear 0 100) Gen.alpha
      n = Gen.int (Range.linear 0 100)
   in Gen.choice [ReallyBadError <$> s, ExitCode <$> n]

-- Fails
prop_setGetMsg :: Property
prop_setGetMsg = setGet genErr genString msg

-- Succeeds
prop_getSetMsg :: Property
prop_getSetMsg = getSet genErr msg

-- Succeeds
prop_setGetMsg' :: Property
prop_setGetMsg' = setGet genErr genString msg'

-- Fails
prop_getSetMsg' :: Property
prop_getSetMsg' = getSet genErr msg'
