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

gen_unlawful3 :: Gen (Int, String, String)
gen_unlawful3 =
  let s = genString
      n = genInt
   in (,,) <$> n <*> s <*> s

-- Fail
prop_unlawful1_getSet :: Property
prop_unlawful1_getSet = getSet gen unlawful1
  where
    gen = (,) <$> genString <*> genString

prop_unlawful1_setGet :: Property
prop_unlawful1_setGet = setGet gen' gen unlawful1
  where
    gen = (,) <$> genString <*> genString
    gen' = (,) <$> gen <*> gen

-- Fail
prop_unlawful2_getSet :: Property
prop_unlawful2_getSet = getSet ((,) <$> genString <*> genString) unlawful1

-- Fails
prop_unlawful3_setGet :: Property
prop_unlawful3_setGet = setGet gen_unlawful3 genString unlawful3

prop_unlawful3_getSet :: Property
prop_unlawful3_getSet = getSet gen_unlawful3 unlawful3

prop_unlawful3_setSet :: Property
prop_unlawful3_setSet = setSet gen_unlawful3 genString unlawful3
