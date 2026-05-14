# Revsion history of io-classes

## next

### Breaking changes

- Removed the deprecated `withLiftST` (deprecated since `io-classes-1.5.0.0`).

### Non-breaking changes

* Repository moved to https://github.com/IntersectMBO/io-sim

## 1.10.1.0

### Non-breaking changes

* Support `ghc-9.14`

## 1.10.0.0

### Breaking changes

* Added `flushEventLog` to `MonadEventLog` class.
* Added `Monad` constraint to `MonadEventlog` class.

### Non-breaking changes

## 1.9.0.0

### Breaking changes

* Changed `Time` show instance, which now is designed for pasting
*  counterexamples from terminal to an editor.

### Non-breaking changes

* Improved performance of `tryReadTBQueueDefault`.
* Added module `Control.Monad.Class.MonadUnique` generalising `Data.Unique`.
* mtl: Added module `Control.Monad.Class.MonadUnique.Trans` providing monad transformer instances for `MonadUnique`.
* Added `roundDiffTimeToMicroseconds` utility function to `si-timers` package (in the `MonadTimer.SI` module).

## 1.8.0.1

* Added support for `ghc-9.2`.

### 1.8.0.0

### Breaking changes

* Provided `MonadTraceMVar`
* Renamed `InspectMonad` to `InspectMonadSTM`
* Added `threadLabel` to `MonadThread`
* Added `MonadLabelledMVar` class.
* Added `labelMVar` to `Control.Concurrent.Class.MonadMVar.Strict`
* Added `debugTraceTVar`, `debugTraceTMVar`, `debugTraceTVarIO`, `debugTraceTMVarIO` for `Show`-based tracing.
* `MonadEvaluate` is not a supper-class of `MonadThrow` anymore.
* Moved all `MonadMaskingState` methods to `MonadMask`. `MonadMaskingState` is
  available but deprecated, it will be removed in one of the future releases.
* `io-classes:mtl` instances support the extended `MonadMask` instance.

### Non-breaking changes

* Added monad transformer instances for `MonadInspectSTM` & `MonadTraceSTM`
  type classes.
* Support ghc-9.12

## 1.7.0.0

### Breaking changes

* Renamed `io-classes:io-classes-mtl` as `io-classes:mtl`.

## 1.6.0.0

### Breaking changes

* `strict-stm`, `strict-mvar` and `si-timers` are now public sublibraries of `io-classes` package.
* Moved `io-classes-mtl` to `io-classes` as a public sublibrary.

## 1.5.0.0

### Breaking changes

* `MonadST` depends on `PrimMonad`.
* Provide a default implementation of `withLiftST`.
* Added `annotateIO` to `MonadThrow` (only supported for ghc-9.10 or newer).

### Non-breaking change

* Add `writeTMVar` to `MonadSTM`.

* `strict-stm`, `strict-mvar` and `si-timers` were moved to `io-classes` as
  **public sublibraries**.  You can import then in `*.cabal` files with the
  following syntax `io-classes:strict-stm` or `io-classes:{strict-stm,
  si-timers}`.  See the _Multiple public libraries_ subsection of [_Internal
  Libraries_][sublibs] section in the `cabal` documentation.

  Note: some time in the future we will deprecate `Hackage` packages
  `strict-stm`, `strict-mvar` and `si-timers`.  If one will want to use the
  namespace for a non `io-classes` related packages please contact the
  maintainer.

[sublibs]: https://cabal.readthedocs.io/en/stable/cabal-package.html#sublibs

## 1.4.1.0

### Non-breaking changes

* New dependency on `primitive` package
* New `stToIO` in `MonadST`, which is simpler to use than the existing
  `withLiftST`, and depends on the `primitive` package's `PrimState` (#141).

## 1.4.0.0

### Breaking changes

* Removed deprecated module `Control.Monad.Class.MonadVar`.

### Non-breaking changes

* Fixed some module haddock typos.

## 1.3.1.0

### Non-breaking changes

* Added `forkFinally` to `MonadFork`.

## 1.3.0.0

- `io-sim-1.3.0.0`.
* Support `ghc-9.8`.

## 1.2.0.0

### Non-breaking changes

* Add new `MonadInspectMVar` class with an `inspectMVar` function for accessing
  an `MVar` in an underlying monad (if applicable). This is mainly useful for
  `io-sim`, since the underlying monad is `ST`. `IO` has no underlying monad, so
  the provided instance for `IO` defaults `inspectMVar` to `tryReadMVar`.
* Add some Haddock documentation to `MonadDelay`

## 1.1.0.0

### Breaking changes

* `Control.Monad.Class.MonadMVar` is now deprecated in favour of
  `Control.Concurrent.Class.MonadMVar`.

### Non breaking changes

* Fixed building haddocks with `ghc-8.10`.

## 1.0.0.1

### Non breaking changes

* Support `ghc-9.6`.

## 1.0.0.0

### Breaking changes

* `MonadMonotonicTime` morphed into `MonadMonotonicTimeNSec` which supports
  `getMonotonicTimeNSec` from "base".  `MonadMonotonicTime` can be found in new
  package `si-timers`.
* A simplified `MonadTimer` which is using time in microseconds encoded as
  `Int` rather than `DiffTime`.  The previous interface can be found in the
  package `si-timers`.
* The non standard timer API is moved from `MonadTimer` to a `MonadTimerFancy`
  type class which can be imported from
  `Control.Monad.Class.MonadTimer.NonStandard` module.

### Non breaking changes

* Added `registerDelayCancellable` to `Control.Monad.Class.MonadTimer` module.

## 0.6.0.0

## 0.5.0.0

* fixed `tryTakeMVarDefault`, `takeMVarDefault`, `putMVarDefault` and
  `isEmptyMVarDefault`.
* Implemented the proper `readMVar` semantics.
* Added `tryReadMVar` to `MonadMVar` type class.
* Moved `MVarDefault` to `io-sim` package.

## 0.4.0.0

* support `ghc-9.4` (except on Windows input-output-hk/io-sim#51)

## 0.3.0.0

* added missing `MArray` instance for `WrappedSTM`
* added monoid instances for `WrappedSTM`
* added `threadStatus` method of `MonadFork` class
* removed `linktTo` and `linkToOnly`
* added `labelTChan`
* added `flushTQueue`
* split `MonadSTM` into submodules in a similar way that the `stm` package is.
* added `TChan`, `TSem` and `TArray`
* added `interruptible` and `allowInterrupt` to `MonadMaskingState` type class.
* added `unGetTQueue` & `unGetTBQueue`
* support `ghc-9.2` & `ghc-9.4`
* added `MonadMVar`

## 0.2.0.0

* First version published on Hackage.
* added `lengthTBQueue` to `MonadSTM`; `io-classes` now depends on `stm-2.5.0.0`.

## 0.1.0.0

* Initial version, not released on Hackage.
