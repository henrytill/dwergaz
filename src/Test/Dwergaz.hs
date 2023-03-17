{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Test.Dwergaz
-- Description : A minimal testing library
-- Copyright   : (c) 2017-2022, Henry Till
-- License     : BSD3
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- = Usage:
--
-- See the <https://github.com/henrytill/dwergaz/blob/master/tests/Main.hs tests> for a usage example.
--
module Test.Dwergaz
  ( Test(..)
  , Result
  , isPassed
  , runTest
  ) where


data Test
  = forall a. (Eq a, Show a) => Predicate String (a -> Bool)      a
  | forall a. (Eq a, Show a) => Expect    String (a -> a -> Bool) a a

data Result
  = Passed String
  | forall a. (Show a) => Failed String a a

instance Show Result where
  show (Failed n e a) =
    "FAILED:   "     ++ n      ++
    "\nEXPECTED:   " ++ show e ++
    "\nACTUAL:     " ++ show a
  show (Passed n) =
    "PASSED:   "     ++ n

isPassed :: Result -> Bool
isPassed (Passed _) = True
isPassed _          = False

runTest :: Test -> Result
runTest (Predicate n p v) | p v       = Passed n
                          | otherwise = Failed n True False
runTest (Expect n f e a)  | f e a     = Passed n
                          | otherwise = Failed n e a
