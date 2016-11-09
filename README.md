Haskell library to track quota usage. Still very WIP.

The core of the library is a type class, `MonadQuota`:

```haskell
class (Monad m) => MonadQuota m where
    recurse :: m a -> m a
    invoice :: Int -> m ()
````

A MonadQuota keeps track of two quotas:

1. Recursion limit
2. Total quota usage

`recurse` runs it's argument with a reduced recursion limit, and reduces
the total quota by 1. this is useful for avoiding blowing the call
stack. `invoice` permanently reduces the total quota by it's argument.
If either quota is violated, an error is signaled and the computation is
stopped.

This was originally developed for use with [haskell-capnp][1], which
needs to deal with potentially maliciously deep and/or cyclic inputs.

[1]: https://github.com/zenhack/haskell-capnp
