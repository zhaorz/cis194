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

doubleEveryOtherTests :: Test
doubleEveryOtherTests =
  TestList [ TestCase (assertEqual "nil" [] (doubleEveryOther [])),
             TestCase (assertEqual "one" [1] (doubleEveryOther [1])),
             TestCase (assertEqual "two" [2,1] (doubleEveryOther [1,1])),
             TestCase (assertEqual "three" [3,2,1] (doubleEveryOther [3,1,1])),
             TestCase (assertEqual "four" [4,3,2,1] (doubleEveryOther [2,3,1,1]))
           ]

sumDigitsTests :: Test
sumDigitsTests =
  TestList [ TestCase (assertEqual "zero" 0 (sumDigits [])),
             TestCase (assertEqual "zero" 0 (sumDigits [0])),
             TestCase (assertEqual "one" 1 (sumDigits [1])),
             TestCase (assertEqual "two" 2 (sumDigits [1,1])),
             TestCase (assertEqual "six" 6 (sumDigits [1,2,3])),
             TestCase (assertEqual "double digits" 7 (sumDigits [16])),
             TestCase (assertEqual "triple digits" 7 (sumDigits [124])),
             TestCase (assertEqual "triple digits" 16 (sumDigits [124, 9]))
           ]

validateTests :: Test
validateTests =
  TestList [ TestCase (assertEqual "valid" True (validate 4012888888881881)),
             TestCase (assertEqual "invalid" False (validate 4012888888881882))
           ]

hanoiTests :: Test
hanoiTests =
  TestList [ TestCase (assertEqual "zero" [] (hanoi 0 "a" "b" "c")),
             TestCase (assertEqual "one" [("a", "b")] (hanoi 1 "a" "b" "c")),
             TestCase (
               assertEqual "two"
               [("a","c"),("a","b"),("c","b")]
               (hanoi 2 "a" "b" "c")),
             TestCase (
               assertEqual "three"
               [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
               (hanoi 3 "a" "b" "c"))
           ]

tests :: Test
tests = TestList [ TestLabel "toDigits" toDigitsTests,
                   TestLabel "doubleEveryOther" doubleEveryOtherTests,
                   TestLabel "sumDigits" sumDigitsTests,
                   TestLabel "validate" validateTests,
                   TestLabel "hanoi" hanoiTests
                 ]
