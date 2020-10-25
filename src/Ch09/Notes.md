## Prisms

### 9.1 Introduction to Prisms

#### How do Prisms fit into the hierarchy?

Like traversals, but can focus at most one value.
_**But,**_ they can also be run **backwards**.

#### Checking pattern matches with prisms

```haskell
has :: Fold s a -> s -> Bool
isn't :: Prism s t a b -> s -> Bool
```

#### Embedding values with prisms

> \[E\]very prism represents a pattern match which can be **reversed**.

> Even though **view**ing through a prism may fail if the pattern doesn't match, **embed**ding a value into a pattern using a prism **always** succeds!

```haskell
review :: Prism s t a b -> b -> t

-- Infix alias:
(#) :: Prism s t a b -> b -> t
```

```haskell
_Show :: (Read a, Show a) => Prism' String a
```

```haskell
-- Get a list of all Integers in a sentence
>>> "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show :: [Int]
[3, 5]
```

### 9.2 Writing Custom Prisms

```haskell
_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where
    match :: Maybe a -> Either (Maybe b) a
    match (Just a) = Right a
    match Nothing = Left Nothing
    embed :: b -> Maybe b
    embed b = Just b
```

```haskell
_Prefix :: String -> Prism' String String
```

### 9.3 Laws

> \[A\] prism is a **reversible pattern match**

#### Law One: Review-Preview

> This law entails the notion that `review` **embeds** values into the same pattern the prism matches.

```haskell
preview p (review p value) = Just value
```

#### Law Two: Prism Complement

> \[T\]he structure is completely described by the combination of the focus of the prism and the prism itself. 

The `_Show` prism is unlawful in this regard.

#### Law Three: Pass-through Reversion


