module Spec.Ch03.DataCorrection where

import Ch03.DataCorrection
import Control.Lens
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let prices = ProducePrices 1.5 1.48
  describe "setting limePrice" do
    it "should round up the lemon price when difference is more than 0.5" do
      set limePrice 2 prices
        `shouldBe` ProducePrices {_limePrice = 2, _lemonPrice = 1.5}
    it "should leave the lemon price as-is when difference is less than 0.5" do
      set limePrice 1.8 prices
        `shouldBe` ProducePrices {_limePrice = 1.8, _lemonPrice = 1.48}
    it "should leave the lemon price as-is when difference is less than 0.5" do
      set limePrice 1.63 prices
        `shouldBe` ProducePrices {_limePrice = 1.63, _lemonPrice = 1.48}
    it "should round up to 0.0 if setting negative lime price" do
      set limePrice (-1) prices
        `shouldBe` ProducePrices {_limePrice = 0, _lemonPrice = 0.5}
  describe "setting lemonPrice" do
    it "should round up the lime price when difference is more than 0.5" do
      set lemonPrice 2.2 prices
        `shouldBe` ProducePrices {_limePrice = 1.7, _lemonPrice = 2.2}
    it "should leave the lemon price as-is when difference is less than 0.5" do
      set lemonPrice 1.8 prices
        `shouldBe` ProducePrices {_limePrice = 1.5, _lemonPrice = 1.8}
    it "should round up to 0.0 if setting negative lime price" do
      over lemonPrice (subtract 21.0) prices
        `shouldBe` ProducePrices {_limePrice = 0.5, _lemonPrice = 0}
