# Operators

| Action      | Operator | Type                                 |
| ----------- | -------- | ------------------------------------ |
| `flip view` | `^.`     | `s -> Lens' s a -> a`                |
| `set`       | `.~`     | `Lens s t a b -> b -> s -> t`        |
| `over`      | `%~`     | `Lens s t a b -> (a -> b) -> s -> t` |

## 5.4 Chaining many operations

The fixity of `(&)`:

> works out so that we can use it to chain many operations together.

## 5.6 Learning Hieroglyphics

> There's actually a sensible language to the whole thing which starts to make sense after a bit of practice. If you can learn the basics of this language you can usually _guess_ the name of a symbol which does what you need, and oftentimes it exists!

Towards the end, mentions this:

> Remember that `id` is a lens which focuses the full structure

but the following exercise does not seem to use `id`
