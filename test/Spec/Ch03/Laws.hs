{-# LANGUAGE TemplateHaskell #-}

module Spec.Ch03.Laws where

import Ch03.Laws
import Hedgehog
  ( Gen,
    Property,
    checkParallel,
    discover,
  )
import Hedgehog.Main
import Spec.Lib

main :: IO ()
main =
  defaultMain
    [checkParallel $$(discover)]

genPair :: Gen (Int, String)
genPair =
  let s = genString
      n = genInt
   in (,) <$> n <*> s

-- Fails
prop_setGetPair :: Property
prop_setGetPair = setGet genPair genString unlawful3

prop_getSetPair :: Property
prop_getSetPair = getSet _
