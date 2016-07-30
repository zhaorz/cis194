module Hw1Test
  ( tests
  ) where

import Hw1
import Test.HUnit

toDigitsTests :: Test
toDigitsTests =
  TestList [ TestCase (assertEqual "zero" [] (toDigits 0)),
             TestCase (assertEqual "neg"  [] (toDigits (-1))),
             TestCase (assertEqual "one"  [1] (toDigits 1)),
             TestCase (assertEqual "ten"  [1,0] (toDigits 10))
           ]

tests :: Test
tests = TestList [ TestLabel "toDigits" toDigitsTests
                 ]
