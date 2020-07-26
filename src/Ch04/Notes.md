## 4.2 When do we need polymorphic lenses

### Changing type variables with polymorphic lenses

```haskell
data Promotion a =
  Promotion
    { _item :: a
    , _discountPercentage :: Double
    }
  deriving Show
```

```haskell
item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter :: Promotion a -> a
    getter = _item
    setter :: Promotion a -> b -> Promotion b
    setter promo newItem = promo{_item = newItem}
```

Exercises - Polymorphic Lenses

1. Write the type signature of the polymorphic lens which would allow changing a `Vorpal x` to a `Vorpal y`.

```haskell
vorpalLens :: Lens (Vorpal x) (Vorpal y) x y
```

2. Find one possible way to write a polymorphic lens which changes the type of the `best` and `worst` fields in the `Preferences` type above. You're allowed to change the type of the lenses or alter the type itself!

```haskell
preferences :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
preferences = lens getter setter
  where
    getter :: Preferences a -> (a, a)
    getter = _best &&& _worst
    setter :: Preferences a -> (b, b) -> Preferences b
    setter p (newBest, newWorst) = Preferences{_best = newBest, _worst = newWorst}
```

3. We can change type of more complex types too. What is the type of a lens which could change the type variable here:

```haskell
data Result e =
  Result
    { _lineNumber :: Int
    , _result :: Either e String
    }
```

### Composing Lenses

4. Find a way to compose ALL of the following lenses together into one big path using each exactly once. What’s the type of the resulting lens?

```haskell
spuzorktrowmble :: Lens Chumble Spuzz Gazork Trowlg
gazorlglesnatchka :: Lens Gazork Trowlg Bandersnatch Yakka
zinkattumblezz :: Lens Zink Wattoom Chumble Spuzz
gruggazinkoom :: Lens Grug Pubbawub Zink Wattoom
banderyakoobog :: Lens Bandersnatch Yakka Foob Mog
boowockugwup :: Lens Boojum Jabberwock Grug Pubbawub
snajubjumwock :: Lens Snark Jubjub Boojum Jabberwock
```

```haskell
snajubjumwock :: Lens Snark Jubjub Boojum Jabberwock
boowockugwup :: Lens Boojum Jabberwock Grug Pubbawub
gruggazinkoom :: Lens Grug Pubbawub Zink Wattoom
zinkattumblezz :: Lens Zink Wattoom Chumble Spuzz
spuzorktrowmble :: Lens Chumble Spuzz Gazork Trowlg
gazorlglesnatchka :: Lens Gazork Trowlg Bandersnatch Yakka
banderyakoobog :: Lens Bandersnatch Yakka Foob Mog
```
