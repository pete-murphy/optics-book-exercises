## Indexable Structures

### 8.2 Accessing and updating values with `Ixed`

```haskell
class Ixed m where
  ix :: Index m -> Traversal' m (IxValue m)
```

> If you haven't seen type families before, they're really just functions that operate on types!

```haskell
-- The index for lists is `Int`
type instance Index [a] = Int
-- The index for Maps is the key type
type instance Index (Map k a) = k

type instance IxValue [a] = a
type instance IxValue (Map k a) = a
```

### 8.3 Inserting & Deleting with `At`

#### Map-like structures

> Now we can **access** or **alter** elements inside structures, but **inserting** and **deleting** elements are also very common operations.

```haskell
class At where
  at :: Index m -> Lens' m (Maybe (IxValue m))
```

> For comparison here's `ix` and `at` side-by-side
```haskell
ix :: Index m -> Traversal' m (IxValue m)
at :: Index m -> Lens'      m (Maybe (IxValue m))
```

```haskell
(?~) :: Traversal s t a (Maybe b) -> b -> s -> t
```

```haskell
sans :: At m => Index m -> m -> m
sans k = at k .~ Nothing
```

### 8.5 Handling missing values

> The use of folds, traversals, and prisms (which we'll learn about soon) includes with it an inherent possibility of **failure**, where **failure** in this case simply means that no elements were selected. Depending on the situation we'll want to handle these failures differently.

> \[H\]ow do we determine whether an update has succeeded?

```haskell
-- Actual signature
failover :: Alternative m => LensLike ((,) Any) s t a b -> (a -> b) -> s -> m t

-- Specialized
failover :: Traversal s t a b -> (a -> b) -> s -> Maybe t
```

#### Fallbacks with `failing`

```haskell
failing :: (Conjoined p, Applicative f)
        => Traversing p f s t a b
        -> Over p f s t a b
        -> Over p f s t a b

-- Specialized signatures
failing :: Fold s t a b -> Fold s t a b -> Fold s t a b
failing :: Traversal s t a b -> Traversal s t a b -> Traversal s t a b
```

#### Default elements

```haskell
non :: Eq a => a -> Iso' (Maybe a) a

-- For all intents and purposes we'll pretend it has this signature:
non :: Eq a => a -> Traversal' (Maybe a) a
```

`non` + `pre` is sort of like running `fromMaybe` on result of `preview`

```haskell
-- Real signature
pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)

-- Specialized
pre :: Fold s a -> Getter s (Maybe a)
```
