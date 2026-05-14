# Building

The project is built with `cabal-install`.  You might need to run `cabal
update` after cloning the repository.

# Design Principles

We designed `io-classes` to be as close as possible to what `base` package
provides.  Almost all `IO` instances instantiate with API provided by one of
the core packages, see
[example](https://github.com/intersectmbo/io-sim/blob/main/io-classes/src/Control/Monad/Class/MonadSTM.hs?plain=1#L410-L446).
Please keep this in mind when adding new functionality.

# Roles and Responsibilities

Maintainers of each package are listed in the corresponding `*.cabal` file.

# Testing

The `typed-protocols-examples` package contains two simple protocols `PingPong`
& `ReqResp` which are used for illustration & testing.

```
cabal run typed-protocols-examples:test
```

If you contribute a more complex feature, it might be a good idea to run the
tests suites from the `ouroboros-network` repository, especially:

* `network-mux:test`
* `ouroboros-network-framework:test`
* `ouroboros-network:test`

If you've been working on `IOSimPOR`, please enable the `nightly` cabal flag in
the `ouroboros-network-testing` framework.  To configure `ouroboros-network`
repository with the right `io-sim` you can include the following snippet in
`cabal.project.local` file:
```
source-repository-package
  type: git
  location: <local .git directory; ssh or http url>
  tag: <hash of the commit, branch name, etc>
  subdir:
    io-classes
    io-classes-mtl
    io-sim
    si-timers
    strict-stm
    strict-mvar

package ouroboros-network-testing
  flags: +nightly
```

# Code Style

Please follow the local style.  For a more detailed style guide see
[link](https://github.com/intersectmbo/ouroboros-network/blob/master/docs/StyleGuide.md).

# Pull Requests

Each commit shall be small and preferably address one thing at a time.
Well-organised & documented commits make it much easier for the maintainers to
review them.  Hacking sessions are great, but please take your time to organise
your work, this usually improves the quality too!

New features should be well documented & tested, which means including new
tests as necessary.  You might be asked by the maintainers to write & include
additional tests.

Each commit should build & test, at least the package you are changing.  You
can update other packages from this repository in a subsequent commit.

Please use a draft PR if the work is still in progress.

If your pull requests resolve an existing issue, please link your PR to the
issue, see [GitHub documentation][gh-link-issue].

Please include your changes in the `CHANGELOG.md` files (per package).

We prefer to avoid merging commits, rebasing a well-organised PR is usually
quite simple.

## Code Style

Please follow the local style.  For a more detailed style guide see
[link](https://github.com/intersectmbo/ouroboros-network/blob/master/docs/StyleGuide.md).

## MonadSTM features

If you are adding new functionality to `MonadSTM`, don't forget to support it
in `strict-stm` package.

## CI

We run CI using [GitHub actions][ci].

# Tips

## `ppTrace` is strict

Both `ppTrace` and `ppTrace_` are strict.  They evaluate the trace before they
produce any result, thus they are not useful when your trace diverges.  This
can happen if the evaluation encounters an unhandled exception e.g. an assertion
fires (either internal or otherwise).  In that case, instead of `ppTrace` you
can use `Data.Trace.toList` and simply `traverse print` the list.  This will
give you the trace up to the point of failure.

## `IOSim` and `STMSim` monads are based on lazy `ST` monad

This means that any action is forced only when the result is needed.  This is
lazier than `IO` monad.  Thus if you want to use `Debug.Trace.traceM` inside
`schedule` function you need to:
```hs
    ...
    !_ <- Debug.Trace.traceM "hello"
    ...
```



[CHaP]: https://github.com/intersectmbo/cardano-haskell-packages/
[gh-link-issue]: https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue
[gh-signing-commits]: https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits
[ci]: https://github.com/intersectmbo/io-sim/actions

