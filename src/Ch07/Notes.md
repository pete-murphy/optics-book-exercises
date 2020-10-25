## Traversals

### 7.1 Introduction to Traversals

Expand on the capabilities of folds, can update or set values we focus _in-place_
Can perform _pure_ updates, but can also run _effectful_ update operations just like `traverse` can

Early versions of traversals were called _multilenses_

> In general we need to be more careful using a traversal because it may fail.

All lenses are valid traversals, and some folds are as well but not all.

### From fold to traversal
```haskell
-- Specialized to tuples:
both :: Traversal (a, a) (b, b) a b

-- Generally
both :: Bitraversable r => Traversal (r a a) (r b b) a b
```

> `filtered` is an extremely powerful tool, it alters the exact same elements which would be focused when you use it in a fold

## 7.2 Traversal Combinators

```haskell
-- Slightly simplified
traversed :: Traversable f => Traversal (f a) (f b) a b

-- Real type
traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b
```

Hmm, so `IndexedTraversal` is more general than `Traversal`?

### More Combinators

```haskell
worded :: Traversal' String String
lined :: Traversal' String String

-- *real types*
worded :: Applicative f => IndexedLensLike' Int f String String
lined  :: Applicative f => IndexedLensLike' Int f String String
```

### Traversing multiple paths at once

```haskell
beside :: Traversal s t a b
       -> Traversal s' t' a b
       -> Traversal (s,s') (t,t') a b

beside :: Lens s t a b -> Lens s' t' a b -> Traversal (s,s') (t,t') a b
beside :: Fold s a     -> Fold s' a      -> Fold (s,s') a
```

```haskell
both = beside id id
```

### Focusing a specific traversal element

```haskell
element :: Traversable f => Int -> Traversal' (f a) a
```

> Because it doesn't focus EVERY element of the container it's a **monomorphic** traversal. It can't change the type of the container's elements.

```haskell
>>> [0, 1, 2, 3, 4] ^? element 2
Just 2

>>> [0..4] & element 2 *~ 100
[0, 1, 200, 3, 4]
```

Author mentions "we'll learn an even better way to do this when we talk about **indexed optics**." I wonder what makes the latter "better"—what else would you do aside from what `element` does?

```haskell
elementOf :: Traversal' s a -> Int -> Traversal' s a
elementOf :: Fold s a       -> Int -> Fold s a
```

```haskell
>>> [0, 1, 2, 3, 4] ^? elementOf traversed 2
Just 2
```

### 7.3 Traversal Composition

1. Short answer questions:
• What type of optic do you get when you compose a traversal with a fold? 
> A fold (least upper bound)

• Which of the optics we’ve learned can act as a traversal?
> `filtered`, `traversed`

• Which of the optics we’ve learned can act as a fold?
> `folded` + all the ones from above :point-up:

### 7.4 Traversal Actions

```haskell
traverseOf  :: Traversal s t a b -> (a -> f b) -> s -> f t
sequenceAOf :: Traversal s t (f a) a           -> s -> f t
```

### 7.5 Custom Traversals

```haskell
type LensLike f s t a b = (a -> f b) -> (s -> f t)
```
> All of the optics that we've looked at so far can be written as some kind of `LensLike`.

```
traverse :: (Traversable g, Applicative f)
         => (a -> f b) -> (g a -> f (g b))
```

> By generalizing (`s = g a`) and (`t = g b`) then we get the type of a traversal

Do you lose information in "generalizing"? Not sure why this is a good thing.

```haskell
myTraversal :: (Applicative f)
            => (a -> f b) -> (s -> f t)
```

### 7.6 Traversal Laws

#### Law One: Respect Purity

```haskell
traverseOf myTraversal pure x == pure x
```

> This says that running the `pure` handler (which has no effects) using our traversal should be exactly the same as running `pure` on the original structure without using the traversal at all.

#### Law Two: Consistent Focuses

```haskell
import Data.Functor.Compose

fmap (traverseOf myTrav f) . traverseOf myTrav g $ x
==
getCompose . traverseOf myTrav (Compose . fmap f . g) $ x
```

> [This says] that running a handler over a traversal should **never change which elements are focused** in the traversal.

> The simpler subset of this law looks like this:

```haskell
x & myTraversal %~ f
  & myTraversal %~ g
==
x & myTraversal %~ (g . f)
```

> In essence this law states that the traversal should never **change which elements it focuses** due to alterations on those elements.

### 7.7 Advanced manipulation

```haskell
partsOf :: Traversal' s a -> Lens' s [a]
```

```haskell
unsafePartsOf :: Traversal s t a b -> Lens s t [a] [b]
```

