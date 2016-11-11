Haskell library to track quota usage.

# Why

Sometimes you need to process untrusted inputs in a way which if done
naively could consume unacceptably large amounts of resources. In this
case, you need a way to track and limit the resource usage.

# What

The core of the library is a type class, `MonadQuota`:

```haskell
class (Monad m) => MonadQuota m where
    recurse :: m a -> m a
    invoice :: Int -> m ()
````

...and a monad transformer `QuotaLimitT`, which implements MonadQuota on
top of MonadThrow and MonadState.

The QuotaLimitT keeps track of two quotas:

1. Recursion limit
2. Total quota usage

`recurse` runs it's argument with a reduced recursion limit, and reduces
the total quota by 1. this is useful for avoiding overflowing the call
stack. `invoice` permanently reduces the total quota by it's argument.
If either quota is violated, an error is signaled and the computation is
stopped.

This was originally developed for use with [haskell-capnp][1], which
needs to deal with inputs where naive traversal could cause a DoS
vulnerability.

# License

Apache 2.0

[1]: https://github.com/zenhack/haskell-capnp
