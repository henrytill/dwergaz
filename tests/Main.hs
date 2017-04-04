module Main where

import Control.Monad (unless)
import Data.Either (isLeft, isRight)
import Dwergaz
import System.Exit (exitFailure)

testFun01 :: a -> String
testFun01 = const "quux"

testFun02 :: a -> Int
testFun02 = const 42

testFun03 :: a -> Either String String
testFun03 = const (Left "quux")

testFun04 :: a -> Either String Int
testFun04 = const (Right 42)

expectExample01, expectExample02 :: Test
expectExample01 = Expect "Strings are equal" (==) "quux" (testFun01 "quux")
expectExample02 = Expect "Ints are equal"    (==) 42     (testFun02 "quux")

predicateExample01, predicateExample02 :: Test
predicateExample01 = Predicate "Value is a Left"  isLeft  (testFun03 "quux")
predicateExample02 = Predicate "Value is a Right" isRight (testFun04 "quux")

exampleTests :: [Test]
exampleTests =
  [ expectExample01
  , expectExample02
  , predicateExample01
  , predicateExample02
  ]

results :: [Result]
results = fmap runTest exampleTests

main :: IO ()
main =  do
  _ <- mapM_ print results
  unless (all isPassed results) exitFailure
