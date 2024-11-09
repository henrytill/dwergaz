{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Test.Dwergaz
-- Description : A minimal testing library
-- Copyright   : (c) 2017-2024, Henry Till
-- License     : ISC
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- = Usage:
--
-- See the <https://github.com/henrytill/dwergaz/blob/master/test/Main.hs tests> for a usage example.
module Test.Dwergaz
  ( Test (..),
    Result,
    resultToString,
    isPassed,
    runTest,
  )
where

import Text.PrettyPrint

data Test
  = forall a. (Eq a, Show a) => Predicate String (a -> Bool) a
  | forall a. (Eq a, Show a) => Expect String (a -> a -> Bool) a a

data Result
  = Passed String
  | forall a. (Show a) => Failed String a a

prettyResult :: Result -> Doc
prettyResult (Passed n) = text "PASSED:" <+> text n
prettyResult (Failed n e a) =
  vcat
    [ text "FAILED:" <+> text n,
      nest 2 (text "EXPECTED:") <+> text (show e),
      nest 2 (text "ACTUAL:") <+> text (show a)
    ]

resultToString :: Result -> String
resultToString = render . prettyResult

isPassed :: Result -> Bool
isPassed (Passed _) = True
isPassed _ = False

runTest :: Test -> Result
runTest (Predicate n p v)
  | p v = Passed n
  | otherwise = Failed n True False
runTest (Expect n f e a)
  | f e a = Passed n
  | otherwise = Failed n e a
