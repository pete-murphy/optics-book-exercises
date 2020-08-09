# Folds

## 6.1 Introduction to Folds

- Lenses must focus **ONE** thing, Folds can focus **MANY** things
- Lenses can **get** and **set**, Folds can only **get**

The simplified type of `folded`

```haskell
folded :: Foldable f => Fold (f a) a
```

expanded thats

```haskell
folded :: (Foldable f, Contravariant g, Applicative g) =>
  (a -> g a) -> (f a -> g (f a))
```

(Nevermind, thats confusing, it seems we're supposed to think of `Fold` as kind of like `Lens'` but you can only `get` with it?)
(Yes, that makes sense now)

Simplified type of `toListOf`

```haskell
toListOf :: Fold s a -> s -> [a]
-- a.k.a.
(^..) :: s -> Fold s a -> [a]
```

### Using lenses as folds

> Ready for a mind-blower? Lenses can be used directly as folds!

Yes, this does blow my mind a bit.

> When we use a lens as a fold we can mentally substitute the types like this:
>
> ```haskell
> Lens' s a
> -- becomes
> Fold s a
> ```

### Foundational fold combinators

> `both` allows us to fold over both parameters _when the parameters are the same_

### (Questions)

How do you use/consume a fold? Is it always turning it into a list? (Or set?)

Author refers to folds as `arbitrary filters`??

## 6.2 Custom Folds

```haskell
folding :: Foldable f => (s -> f a) -> Fold s a
```

### (Questions)

Why use a fold?

### Mapping over folds

```haskell
to :: (s -> a) -> Fold s a
```

> Technically, `to` is a `Getter` rather than a fold, a `Getter` is just a fold which has this 1-to-1 mapping property, it's basically the "getter" half of a lens.

## 6.3 Fold Actions

_Queries_ (what author is calling queries—`findOf`, `elemOf`, `maximumByOf`, etc.) "behave as though you've collected your fold into a list and then run the respective operation; _but they tend to optimize performance a little better._ (Emphasis mine.)

> `firstOf`, `preview`, and `^?` are all effectively equivalent

Comparing `maximumBy` and `maximumByOf` types blew my mind:

```haskell
maximumBy   :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumByOf :: Fold s a   -> (a -> a -> Ordering) -> s   -> Maybe a
```

The substition of the `=>` for `->` reminded me of the dictionary-passing style that we use in TypeScript in lieu of type classes.
The author refers to this as "allow[ing] you to provide your folding behaviour _(sic)_ a'la carte as a fold."

> Optics are just values, we can pass them around to functions if we like. Let's see an Object Oriented language do THAT with dot-notation.

### Combining fold results

He presents the "two new actions for our folding toolbox"
{{{haskell
foldOf    :: Monoid a => Fold s a ->             s -> a
foldMapOf :: Monoid r => Fold s a -> (a -> r) -> s -> r

-- Their *real* signatures
foldOf    :: Getting a s a ->             s -> a
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
}}}

WTF? How do you not need a `Monoid` constraint on `a` and `r` (respectively) in "their *real* signatures"?
Looking at the docs for [Getting](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Combinators.html#t:Getting):
> If a function accepts a `Getting r s a`, then when `r` is a `Monoid`, then you can pass a `Fold` (or `Traversal`), otherwise you can only pass this a `Getter` or `Lens`.

How does this type inference/resolution work?

### Exercises

```haskell
>>> _ folded []
False
>>> has folded []

>>> _ both ("Yo", "Adrian!")
"YoAdrian!"
>>> foldOf both ("Yo", "Adrian!")

>>> _ each "phone" ("E.T.", "phone", "home")
True
>>> elemOf each "phone" ("E.T.", "phone", "home")

>>> _ folded [5, 7, 2, 3, 13, 17, 11]
Just 2
>>> minimumOf folded [5, 7, 2, 3, 13, 17, 11]

>>> _ folded [5, 7, 2, 3, 13, 17, 11]
Just 11
>>> lastOf folded [5, 7, 2, 3, 13, 17, 11]

>>> _ folded ((> 9) . length)
["Bulbasaur", "Charmander", "Squirtle"] True
>>> _ folded ((> 9) . length)

>>> _ folded even [11, 22, 3, 5, 6]
Just 22
```

## 6.4 Higher Order Folds

Scary type signatures:

```haskell
taking
  :: (Conjoined p, Applicative f)
  => Int
  -> Traversing p f s t a a
  -> Over p f s t a a

dropping
  :: (Conjoined p, Applicative f)
  => Int
  -> Over p (Control.Lens.Internal.Indexed.Indexing f) s t a a
  -> Over p f s t a a
```

Friendlier type signatures:

```haskell
taking
  :: Int
  -> Fold s a
  -> Fold s a

dropping
  :: Int
  -> Fold s a
  -> Fold s a
```

### Backwards

Another higher-order fold which is fun to use: `backwards`

```haskell
backwards :: Fold s a -> Fold s a
```

