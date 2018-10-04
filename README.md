**Unmaintained**. This package was originally written for use with [haskell-capnp][1],
but the relevant functionality [has been folded into that package][2], so this is no
longer used.

Haskell library to track quota usage.

# Why

Sometimes you need to process untrusted inputs in a way which if done
naively could consume unacceptably large amounts of resources. In this
case, you need a way to track and limit the resource usage.

# What

The core of the library is a type class, `MonadQuota`:

```haskell
class MonadQuota m where
    invoice :: Int -> m ()
````

...and a monad transformer `QuotaT`, which implements MonadQuota on
top of MonadThrow.

The QuotaT keeps track of a quota, calling `throwM QuotaError` if it is
expended. `invoice` deducts its argument from the quota.

This was originally developed for use with [haskell-capnp][1], which
needs to deal with inputs where naive traversal could cause a DoS
vulnerability.

# License

Apache 2.0

[1]: https://github.com/zenhack/haskell-capnp
[2]: http://hackage.haskell.org/package/capnp-0.3.0.0/docs/Data-Capnp-TraversalLimit.html
