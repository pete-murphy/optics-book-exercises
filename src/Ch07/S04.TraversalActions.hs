{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch07.S04.TraversalActions where

import Control.Applicative
import Prelude hiding (Traversable (..))

class Functor t => Traversable t where
  sequenceA :: Applicative f => t (f a) -> f (t a)
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  {-# MINIMAL sequenceA | traverse #-}
  sequenceA = traverse id
  traverse f = sequenceA . fmap f

instance Traversable [] where
  traverse ::
    forall f a b.
    Applicative f =>
    (a -> f b) ->
    [a] ->
    f [b]
  -- traverse _ [] = pure []
  -- traverse f (a:as) = (:) <$> f a <*> traverse f as
  traverse f = foldr (liftA2 (:) . f) (pure [])
-- Compare with:
-- traverse_ f = foldr ((*>) . f) (pure ())

instance Traversable Maybe where
  traverse ::
    forall f a b.
    Applicative f => (a -> f b) -> (Maybe a -> f (Maybe b))
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = Just <$> f x

