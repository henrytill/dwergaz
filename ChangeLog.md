# Revision history for dwergaz

## 0.1.0.0  -- 2017-04-03

* First version. Released on an unsuspecting world.

## 0.1.0.1  -- 2017-04-09

* Updated a bunch of administrivia.

## 0.2.0.0  -- 2017-04-09

* Renamed `Dwergaz` module to `Test.Dwergaz`.

## 0.2.0.1  -- 2017-08-12

* Loosened constraints on `base` for compatibility with GHC 8.2.1.

## 0.2.0.2  -- 2018-06-23

* Loosened constraints on `base` for compatibility with GHC 8.4.3.

## 0.2.0.3  -- 2018-11-14

* Loosened constraints on `base` for compatibility with GHC 8.6.2.

## 0.2.0.4  -- 2020-05-05

* Loosened constraints on `base` for compatibility with GHC 8.8.x.

## 0.2.0.5  -- 2022-06-27

* Updated copyright dates.
* Moved source repository.

## 0.2.0.6  -- 2023-08-26

* Switched from GADTs to ExistentialQuantification, as we didn't need the full power of GADTs.
* Switched to Haskell98, as we didn't need the full power of Haskell2010.
* Updated copyright dates.

## 0.3.0.0  -- 2024-11-09

* Removed `Show` instance for `Result`.  `Result` output should now be acquired with `resultToString`.
* Renamed `isPassed` to `resultIsPassed` for consistency with `resultToString`.
* Removed (unnecessary) `Eq` constraints from `Test`.
* Added a second type variable `b` to `Expect`, allowing a wider variety of tests.
* Added `assertFailure`, `assertBool`, and `assertEqual` helper functions for creating tests.
* Simplified `Predicate` to only accept a boolean argument instead of separate predicate function and value arguments.

## 0.3.0.1  -- 2024-12-02

* Tweaked tests and formatting.

## 0.3.0.2  -- 2025-02-26

* Updated copyright dates.
* Tweaked documentation.

## 0.3.1.0  -- 2025-06-19

* Added a `Group` constructor and a corresponding `group` function for creating a group of tests.
