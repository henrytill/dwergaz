module Main (main) where

import Control.Monad (when)
import Data.Either (isLeft, isRight)
import System.Exit (exitFailure)
import Test.Dwergaz

testFun01 :: a -> String
testFun01 = const "quux"

testFun02 :: a -> Int
testFun02 = const 43

testFun03 :: a -> Either String String
testFun03 = const (Left "quux")

testFun04 :: a -> Either String Int
testFun04 = const (Left "quux")

expectExample01, expectExample02 :: Test
expectExample01 = Expect "Strings are equal" (==) "quux" (testFun01 "quux")
expectExample02 = assertEqual "Ints are equal" 42 (testFun02 "quux")

predicateExample01, predicateExample02, predicateExample03 :: Test
predicateExample01 = Predicate "Value is a Left" (isLeft (testFun03 "quux"))
predicateExample02 = assertBool "Value is a Right" (isRight (testFun04 "quux"))
predicateExample03 = assertFailure "Just fail"

exampleTests :: [Test]
exampleTests =
  [ expectExample01,
    expectExample02,
    predicateExample01,
    predicateExample02,
    predicateExample03
  ]

step :: Result -> (ShowS, Bool) -> (ShowS, Bool)
step result (f, allPassed) =
  ( showString (resultToString result) . showChar '\n' . f,
    resultIsPassed result && allPassed
  )

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    (buildString, allPassed) = foldr (step . runTest) (id, True) exampleTests

main :: IO ()
main = do
  let (output, passed) = results
  putStr output
  -- The following should be 'Control.Monad.unless' in real usage.
  when passed exitFailure
