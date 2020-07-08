module Spec.Lib where

import Control.Lens
import Hedgehog
  ( (===),
    Gen,
    Property,
    forAll,
    property,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

getSet ::
  (Show a, Show s, Eq s) =>
  Gen s ->
  Lens' s a ->
  Property
getSet gs l =
  property do
    s <- forAll gs
    set l (view l s) s === s

setGet ::
  (Show a, Show s, Eq a) =>
  Gen s ->
  Gen a ->
  Lens' s a ->
  Property
setGet gs ga l =
  property do
    s <- forAll gs
    a <- forAll ga
    view l (set l a s) === a

setSet ::
  (Show a, Show s, Eq s) =>
  Gen s ->
  Gen a ->
  Lens' s a ->
  Property
setSet gs ga l =
  property do
    s <- forAll gs
    a <- forAll ga
    set l a s === set l a (set l a s)

genString :: Gen String
genString = Gen.string (Range.linear 0 100) Gen.alpha

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 100)
