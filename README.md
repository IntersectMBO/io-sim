[![Haskell CI](https://img.shields.io/github/actions/workflow/status/intersectmbo/io-sim/haskell.yml?branch=main&label=Build&style=for-the-badge)](https://github.com/intersectmbo/io-sim/actions/workflows/haskell.yml)
[![handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational?style=for-the-badge)](https://intersectmbo.github.io/cardano-engineering-handbook)
[![Haddocks](https://img.shields.io/badge/documentation-Haddocks-pink?style=for-the-badge)](https://intersectmbo.github.io/io-sim)

# [`io-sim`]

[`IOSim`] is a simulator monad that supports:

* asynchronous exceptions
* simulated time
* timeout API
* software transaction memory (STM)
* concurrency: both low-level `forkIO` as well as `async` style
* strict STM
* access to lazy ST
* schedule discovery (see [IOSimPOR][io-sim-por-how-to])
* event log
* dynamic tracing
* tracing committed changes to `TVar`, `TMVar`s, etc.
* labeling of threads, `TVar`'s, etc.

[`io-classes:io-classes`] provides an interface, which allows writing code that
can be run in both real `IO` and [`IOSim`].  It is a drop-in replacement for
`IO`, and supports interfaces commonly known from `base`, `exceptions`, `stm`,
`async`, or `time` packages.

One of the principles of `io-classes:io-classes` was to stay as close to `IO`
as possible, thus most of the `IO` instances are directly referring to `base`
or `async` API.  However, we made some distinctions, which are reported below.

[`io-classes:io-classes`] supports a novel hierarchy for error-handling monads
as well as more familiar `exception` style.  The new hierarchy provides
`bracket` and `finally` functions in the `MonadThrow` class, while `catch`
style operators are provided by a super-class `MonadCatch`.  Both `bracket` and
`finally` are the most common functions used to write code with robust
exception handling, exposing them through the more basic `MonadThrow` class
informs the reader / reviewer that no tricky error handling is done in that
section of the code base.

[`IOSim`] exposes a detailed trace, which can be enhanced by labeling threads, or
mutable variables, tracing `Dynamic` values (which can be recovered from the
trace), or simple `String` based tracing.  Although it's agnostic concerning
the logging framework, it worked for us particularly well using
[contra-tracer][contra-tracer].  It has been used to develop, test, and debug
a complex, highly concurrent, distributed system
([ouroboros-network][ouroboros-network]), in particular

* write network simulations, to verify a complex networking stack;
* write disk IO simulations, to verify a database implementation.

## Supporting material

* [Philipp Kant (@kantp) at Bobconf 2022][bob-conf]
* [Armando Santos (@bolt12) at ZuriHac 2022][zuriHac-2022]
* [Marcin Szamotulski (@coot) IOSim and Partial Order Reduction][io-sim-por-presentation]


## Packages

* [`io-sim`]: provides two simulator interpreters: [`IOSim`] and
  `IOSimPOR` - an enhanced [`IOSim`] version with schedule discovery
  capabilities.
* [`io-classes:io-classes`]: class bases interface, which allows to to abstract over the
    monad
* [`io-classes:strict-stm`]: strict STM operations
* [`io-classes:si-timers`]: non-standard timers API

## Issues

New issues should be reported in [this][io-sim-issues] repository.

[`io-classes:io-classes`]: https://hackage.haskell.org/package/io-classes
[`io-classes:strict-stm`]:  https://hackage.haskell.org/package/io-classes
[`io-classes:si-timers`]:  https://hackage.haskell.org/package/io-classes
[`io-sim`]: https://hackage.haskell.org/package/io-sim

[contra-tracer]: https://hackage.haskell.org/package/contra-tracer
[io-sim-issues]: https://github.com/intersectmbo/io-sim/issues
[io-sim-por-how-to]: ./io-sim/how-to-use-IOSimPOR.md
[io-sim-por]: https://github.com/intersectmbo/io-sim/blob/main/io-sim/how-to-use-IOSimPOR.md
[ouroboros-network]: https://github.com/intersectmbo/ouroboros-network

[`IOSim`]: https://hackage.haskell.org/package/io-sim/docs/Control-Monad-IOSim.html#t:IOSim

[bob-conf]: https://youtu.be/uedUGeWN4ZM
[zuriHac-2022]: https://youtu.be/tKIYQgJnGkA
[io-sim-por-presentation]: https://coot.me/presentations/iosimpor.pdf
