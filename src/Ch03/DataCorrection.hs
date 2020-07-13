module Ch03.DataCorrection where

import Control.Lens

data Time
  = Time
      { _hours :: Int,
        _mins :: Int
      }
  deriving (Show)

clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal = min maxVal . min minVal

hours' :: Lens' Time Int
hours' = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (clamp 0 23 newHours) m

mins' :: Lens' Time Int
mins' = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMins = Time h (clamp 0 59 newMins)

hours :: Lens' Time Int
hours = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (newHours `mod` 24) m

mins :: Lens' Time Int
mins = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMins =
      let (h', m') = newMins `divMod` 60
          h'' = (h + h') `mod` 24
       in Time h'' m'

-- | Exercises
data ProducePrices
  = ProducePrices
      { _limePrice :: Float,
        _lemonPrice :: Float
      }
  deriving (Show, Eq)

-- | Initial pass
limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter = _limePrice
    setter = \ProducePrices {..} limePrice' ->
      let limePrice'' = max 0 limePrice'
          lemonPrice'
            | _lemonPrice > limePrice'' + 0.5 = limePrice'' + 0.5
            | _lemonPrice < limePrice'' - 0.5 = limePrice'' - 0.5
            | otherwise = _lemonPrice
       in ProducePrices limePrice'' lemonPrice'

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter = _lemonPrice
    setter = \ProducePrices {..} lemonPrice' ->
      let lemonPrice'' = max 0 lemonPrice'
          limePrice'
            | _limePrice > lemonPrice'' + 0.5 = lemonPrice'' + 0.5
            | _limePrice < lemonPrice'' - 0.5 = lemonPrice'' - 0.5
            | otherwise = _limePrice
       in ProducePrices limePrice' lemonPrice''
