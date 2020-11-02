## Isos

### 10.6 Projecting Isos

> The strong guarantees which isos provide allow us to sneakily lift them into other structures.

```haskell
mapping' :: Functor f => Iso' s a -> Iso' (f s) (f a)
mapping' i = iso (fmap (view i)) (fmap (review i))

-- A more general version is provided in `lens`
mapping 
  :: (Functor f, Functor g)
  => Iso s t a b -> Iso (f s) (g t) (f a) (g b)
```
