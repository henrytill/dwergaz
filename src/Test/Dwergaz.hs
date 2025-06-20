{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Test.Dwergaz
-- Description : A minimal testing library
-- Copyright   : (c) 2017-2025, Henry Till
-- License     : ISC
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- = Usage:
--
-- See the <https://github.com/henrytill/dwergaz/blob/master/test/Main.hs tests> for a usage example.
module Test.Dwergaz
  ( Test (..)
  , assertFailure
  , assertBool
  , assertEqual
  , group
  , Result
  , resultToString
  , resultIsPassed
  , runTest
  )
where

import Text.PrettyPrint
import Prelude hiding ((<>))

data Test
  = forall a b.
    (Show a, Show b) =>
    Expect
      -- | Test description
      String
      -- | Test function
      (a -> b -> Bool)
      -- | Expected value
      a
      -- | Actual value
      b
  | Predicate
      -- | Test description
      String
      -- | Condition
      Bool
  | Group
      -- | Group name
      String
      -- | Tests
      [Test]

assertFailure ::
  -- | Test description
  String ->
  Test
assertFailure = flip Predicate False

assertBool ::
  -- | Test description
  String ->
  -- | Condition
  Bool ->
  Test
assertBool = Predicate

assertEqual ::
  (Eq a, Show a) =>
  -- | Test description
  String ->
  -- | Expected value
  a ->
  -- | Actual value
  a ->
  Test
assertEqual desc = Expect desc (==)

group ::
  -- | Group name
  String ->
  -- | Tests
  [Test] ->
  Test
group = Group

data Result
  = forall a b. (Show a, Show b) => FailedExpect String a b
  | Failed String
  | Passed String
  | Multiple String [Result]

prettyResult :: Result -> Doc
prettyResult (FailedExpect n e a) =
  vcat
    [ text "FAILED:" <+> text n
    , nest 2 (text "EXPECTED:") <+> text (show e)
    , nest 2 (text "ACTUAL:") <+> text (show a)
    ]
prettyResult (Failed n) = text "FAILED:" <+> text n
prettyResult (Passed n) = text "PASSED:" <+> text n
prettyResult (Multiple n rs) =
  vcat
    [ text n <> colon
    , nest 2 . vcat . map prettyResult $ rs
    ]

resultToString :: Result -> String
resultToString = render . prettyResult

resultIsPassed :: Result -> Bool
resultIsPassed (Passed _) = True
resultIsPassed (Multiple _ rs) = all resultIsPassed rs
resultIsPassed _ = False

runTest :: Test -> Result
runTest (Expect n f e a)
  | f e a = Passed n
  | otherwise = FailedExpect n e a
runTest (Predicate n c)
  | c = Passed n
  | otherwise = Failed n
runTest (Group n ts) =
  Multiple n (map runTest ts)
