# Revision history of io-sim

## next version

### Breaking changes

### Non-breaking changes

* Repository moved to https://github.com/IntersectMBO/io-sim

## 1.10.1.0

### Non-breaking changes

* Added `IOSimPOR` `QuickCheck` combinators:
  * `monadicIOSimPOR_`
  * `monadicIOSimPOR`
  * `runIOSimPORGen`
* Support ghc-9.14
* Support QuickCheck-2.18.0.0

## 1.10.0.0

### Breaking changes

* Added `EventEvaluationError`, `EventEvaluationSuccess`
* Added `EventSayEvaluationError`, `EventLogEvaluationError`
* Added `flushEventLog` to `MonadEventLog` instance.

### Non-breaking changes

* `ppSimEventType` (used by `Control.Monad.IOSim.ppTrace` and
  `Control.Monad.IOSim.ppTrace_`): does not fail if `EventThrow` or
  `EventThrowTo` contain a pure exception.  This supports laziness of `throwIO`
  and `throwTo`.
* `say`, `traceM` and `traceSTM` evaluate their arguments (first one to _NF_
  the other two to _WHNF_).  They throw an exception (within the simulator) if
  evaluation fails.  For `say` this makes it behave like `putStrLn` does.
  Previously all would throw a pure exception which would terminate the
  simulator prematurely.  If you want to verify that these calls do not fail,
  you can check for absence of `EventSayEvaluationError` or
  `EventLogEvaluationError`.
* Added `Data.List.Trace.last`
* Although `IOSim` and `IOSimPOR` are pure we use `evaluate` in a few places,
  none of them now catch asynchronous exceptions.
* Added `IOSimPOR` QuickCheck monadic combinators:
  `monadicIOSimPOR`, `monadicIOSimPOR_` and `runIOSimPORGen`.

## 1.9.1.0

### Non-breaking changes

* Reverted rounding of `si-timers` API to microsecond to match `IO` behaviour.
  It may cause surprising issues downstream when time is not advancing as
  expected because of rounding to `0`.

## 1.9.0.0

The version is bumped to `1.9` in sync with `io-classes-1.9`.

### Non-breaking changes

* Added support for unique symbol generation à la `Data.Unique`.
* Removed a misleading internal comment.
* Fixed error handling in `traceResult` so one can combine it (or ather APIs
  which are based on it: `runSim`, `runSimOrThrow`, or `runSimStrictShutdown`)
  with `within` or `discardAfter` from `QuickCheck`.  See the test suite how to
  use `discardAfter` with `IOSim`.
* Round `si-timers` API (`MonadDelay`, `MonadTimer`) to microsecond to match
  `IO` behaviour.

## 1.8.0.1

* Added support for `ghc-9.2`.

## 1.8.0.0

- Provided `MonadTraceMVar`
- Renamed `InspectMonad` to `InspectMonadSTM`
- Support `threadLabel` (`io-classes-1.8`)
- `IOSimPOR`'s `Effect` traces now will correctly show labels on read/written
  `TVars`.
- `Show` instance for `ScheduleMod` now prints `ThreadId`s in a slightly nicer
  way, matching the way those steps would be traced in the `SimTrace`.
- Implement `MonadLabelledMVar` instance for `(IOSim s)`
- `TVarId` is now a sum type with one constructor per `TVar` role, e.g. `TVar`,
  `TMVar`, `MVar` and a few others - except for `TChan`.
- A blocked `takeTVar` is now safe in the presence of exceptions. It will relay
  the value to other waiting threads.
- Faster handling of timeouts and timers by using a more efficient
  internal representation.
- The signature of:
  - `selectTraceEvents'`,
  - `selectTraceEventsDynamic'`,
  - `selectTraceEventsDynamicWithTime'`,
  - `selectTraceEventsSay'` and
  - `selectTraceEventsSayWithTime'`
  is more general.  These functions now accepts trace with any result, rather
  than one that finishes with `SimResult`.
- More polymorphic `ppTrace_` type signature.
- Fixed `tryReadTBQueue` when returning `Nothing`.
- Support ghc 9.12
- Export `Time` from `Control.Monad.IOSim`.

## 1.6.0.0

- Upgraded to `io-classes-1.6.0.0`

## 1.5.0.0

### Breaking changes

- Generalised the type of `traceSelectTraceEvents` & co.

### Non-breaking changes

- Added `writeTMVar` to `MonadSTM` instance for `(IOSim s)`.
- Fixes IOSimPOR test failure (see issue #154).
- Reverted commit 4534b6eae64072a87bd81584f479a123681358a3 which uses
  `unsafePerformIO` instead of ST, to regain lazyness on infinite simulations.
- Added a test to check for lazyness on infinite simulations

## 1.4.1.0

### Non-breaking changes

* QuickCheck monadic combinators: `monadicIOSim`, `monadicIOSim_` and `runIOSimGen` (#140).
* New dependency on `primitive`
* Provides an instance for `PrimMonad`, giving access to most functionality
  from the `primitive` package (#141).
* Prevented STM waking up threads blocked on `threadDelay` (#142).

## 1.4.0.0

### Breaking changes

* Removed `EventTimerUpdated` constructor (not used anymore).

### Non-breaking changes

* `Alternative` & `MonadPlus` instances for `IOSim`.
* Fixed `flushTQueue` implemetation.

## 1.3.1.0

### Non-breaking changes

* Optimised `io-sim` performance (improved memory footprint).
* Fixed a bug in `io-sim-por`: `execAtomically'` should not commit tvars.

## 1.3.0.0

### Breaking changes

* `MainReturn`, `MainException` and the pattern synonyms `TraceMainReturn`,
  `TraceMainException` changed their signature.  They will now also show the main thread id.
* Renamed `ThreadId` to `IOSimThreadId` to avoid a clash with `ThreadId`
  associated type family of `MonadFork`.  It makes it much simpler to paste
  failing `ScheduleControl` in `ghci` or tests.
* `BlockedReason` was modified: `BlockedOnOther` was removed, in favour of `BlockedOnDelay` and `BlockOnThrowTo`.
* The `Failure` type (for example returned by `runSim`) now also contains
  a constructor for internal failures.  This improved error reporting when
  there's a bug in `IOSimPOR`.  Currently it's only used by some of the
  assertions in `IOSimPOR`.

#### Non-breaking changes

* Refactored the internal API to avoid `unsafePerformIO`.
* Fixed bugs which lead to discovery of schedules which are impossible to run.
* Added haddocks, refactored the code base to improve readability.
* Fixed reported `step` in `EventTxWakup`
* Added debugging information schedule, (`explorationDebugLevel` option).
  Mostly useful for debugging `IOSimPOR` itself.  This information will
  contains `Effect`, discovered races and schedules.
* Addded or improved pretty printers for `SimTrace`.  Among other changes,
  a racy `StepId`: `(RacyThreadId [1,2], 2)`, is now pretty printed as `Thread
  {1,2}.2`, a non racy step is printed as `Thread [1,2].2`.
* Fixed trace of calls to the `deschedule` function.
* Exposed `Timeout` type as part of the `newTimeout` API.
* When `explorationDebugLevel` is set, avoid printing the same trace twice.
* Reimplemented `labelTVarIO` and `traceTVarIO` in `ST` monad, which simplifies
  trace of these calls.
* Fixed `traceTVar` for `TVar`'s created with `registerDelay`.
* Added pretty printer for `SimResult`, and other pretty printer improvements.
* Support `ghc-9.8`.

## 1.2.0.0

### Breaking changes

* `selectTraceEvents`, `selectTraceEvents'` catpure time of events.
* Added select function which capture the time of the trace events:
  - `selectTraceEventsDynamicWithTime`
  - `selectTraceEventsDynamicWithTime'`
  - `selectTraceEventsSayWithTime`
  - `selectTraceEventsSayWithTime'`

### Non-breaking changes

* Provide `MonadInspectMVar` instance for `IOSim`.
- Added NFData & NoThunks instances for `ThreadId`

## 1.1.0.0

### Non-breaking changes

* `io-classes-1.1.0.0`

## 1.0.0.1

### Non-breaking changes

* Support `ghc-9.6`.

## 1.0.0.0

### Breaking changes

* Support refactored `MonadTimer`, and new `MonadTimerFancy`, `MonadTimeNSec`
  monad classes.

## 0.6.0.0

### Breaking changes

* Added `TimeoutId` to `EventThreadDelay` and `EventThreadFired` events.

### Non-breaking changes

* Fixed `threadDelay` in presence of asynchronous exceptions (in `IOSim` and `IOSimPOR`) (#80).
* Fixed bug in `IOSim` & `IOSimPOR` which resulted in reusing existing
  `ThreadId`s (#80).

## 0.5.0.0

* `MVar` type family is not injective anymore.
* Removed default implementation of `readMVar` in the `MonadMVar` type class.
* Moved `MVarDefault` to `io-sim` package.

## 0.4.0.0

* support `ghc-9.4` (except on Windows input-output-hk/io-sim#51)
* `MonadCatch` instance for `STM (IOSim s)` monad
* fixed `isEmptyTBQueeuDefault` (thanks to @EMQ-YangM)
* refactored internal implementation of timeouts, changed `SimEventType`
  constructors

## 0.3.0.0

* added `Functor` instance for `SimResult`
* added `MonadFix` instance for `STM (IOSim s)`
* support `ghc-9.2` & `ghc-9.4`

## 0.2.0.0

* First version published on Hackage.
* Depends on `io-classes-0.2.0.0`.

## 0.1.0.0

* Initial version, not released on Hackage.
