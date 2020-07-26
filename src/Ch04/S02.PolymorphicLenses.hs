module Ch04.S02.PolymorphicLenses where

import Control.Arrow
import Control.Lens

data Preferences a
  = Preferences
      { _best :: a,
        _worst :: a
      }
  deriving (Show)

preferences :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
preferences = lens getter setter
  where
    getter :: Preferences a -> (a, a)
    getter = _best &&& _worst
    setter :: Preferences a -> (b, b) -> Preferences b
    setter _ (newBest, newWorst) =
      Preferences {_best = newBest, _worst = newWorst}

data Result e
  = Result
      { _lineNumber :: Int,
        _result :: Either e String
      }

result :: Lens (Result e) (Result e') (Either e String) (Either e' String)
result = lens getter setter
  where
    getter :: Result e -> Either e String
    getter = _result
    setter :: Result e -> Either e' String -> Result e'
    setter r e = r {_result = e}

-- 4. It’s thinking time! Is it possible to change more than one type variable
--    at a time using a polymorphic lens?
-- > Yes? Using the tuple trick? Could be a
data Foo a b = Foo a b

tupleLens :: Lens (Foo a b) (Foo c d) (a, b) (c, d)
tupleLens = lens getter setter
  where
    getter :: Foo a b -> (a, b)
    getter (Foo a b) = (a, b)
    setter :: Foo a b -> (c, d) -> Foo c d
    setter _ (c, d) = Foo c d

-- 5.

newtype Predicate a = Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where
    getter :: Predicate a -> (a -> Bool)
    getter (Predicate f) = f
    setter :: Predicate a -> (b -> Bool) -> Predicate b
    setter _ = Predicate
