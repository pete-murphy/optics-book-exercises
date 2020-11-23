## Indexed Optics

### What are indexed optics?

> \[Indexed optics\] allow you to accumulate information about your current focus as you dive deeper into an optics path.

```haskell
itraversed :: TraversableWithIndex i t => IndexedTraversal i (t a) (t b) a b
```

```haskell
itoListOf :: IndexedFold i s a -> s -> [(i, a)]

-- As an operator
(^@..) :: s -> IndexedFold i s a -> [(i, a)]
```

