module Hw3Test
  ( tests
  ) where

import Golf
import Test.HUnit

eachTests :: Test
eachTests =
  TestList [ TestCase (assertEqual "none" ([] :: [Int]) (each 1 [])),
             TestCase (assertEqual "one" "a" (each 1 "a")),
             TestCase (assertEqual "all" "abcde" (each 1 "abcde")),
             TestCase (assertEqual "second" "bd" (each 2 "abcde")),
             TestCase (assertEqual "third" "c" (each 3 "abcde")),
             TestCase (assertEqual "fourth" "d" (each 4 "abcde")),
             TestCase (assertEqual "fifth" "e" (each 5 "abcde"))
           ]

skipsTests :: Test
skipsTests =
  TestList [ TestCase (assertEqual "empty" ([] :: [[Int]]) (skips ([] :: [Int]))),
             TestCase (assertEqual "one" [[1]] (skips [1])),
             TestCase (assertEqual "two" [[True,False],[False]] (skips [True,False])),
             TestCase (assertEqual "abcd"  ["ABCD", "BD", "C", "D"] (skips "ABCD"))
           ]

localMaximaTests :: Test
localMaximaTests =
  TestList [ TestCase (assertEqual "empty" [] (localMaxima [])),
             TestCase (assertEqual "empty" [] (localMaxima [1])),
             TestCase (assertEqual "empty" [] (localMaxima [1,2])),
             TestCase (assertEqual "empty" [] (localMaxima [1,2,3])),
             TestCase (assertEqual "one" [4] (localMaxima [1,4,3])),
             TestCase (assertEqual "one" [4] (localMaxima [1,4,3,7])),
             TestCase (assertEqual "two" [4,7] (localMaxima [1,4,3,7,6]))
           ]

countTests :: Test
countTests =
  TestList [ TestCase (assertEqual "zero"
                       [0,0,0,0,0,0,0,0,0,0]
                       (count [])),
             TestCase (assertEqual "count should be correct"
                       [1,0,0,0,0,0,0,0,0,0]
                       (count [0])),
             TestCase (assertEqual "count should be correct"
                       [1,1,0,0,0,0,0,0,0,0]
                       (count [0,1])),
             TestCase (assertEqual "count should be correct"
                       [1,0,1,0,0,0,0,0,0,0]
                       (count [0,2])),
             TestCase (assertEqual "count should be correct"
                       [1,0,0,3,0,0,0,0,0,0]
                       (count [0,3,3,3])),
             TestCase (assertEqual "count should be correct"
                       [1,0,0,0,0,0,0,2,0,1]
                       (count [0,7,9,7]))
           ]

lineTests :: Test
lineTests =
  TestList [ TestCase (assertEqual "none"
                       "         "
                       (line [0,0,0,0,0,0,0,0,0] 1)),
             TestCase (assertEqual "line should be correct"
                       "*      * "
                       (line [1,0,0,0,0,0,0,1,0] 1)),
             TestCase (assertEqual "line should be correct"
                       "* **   * "
                       (line [1,0,2,3,0,0,0,1,0] 1)),
             TestCase (assertEqual "line should be correct"
                       "  **     "
                       (line [1,0,2,3,0,0,0,1,0] 2)),
             TestCase (assertEqual "line should be correct"
                       "   *     "
                       (line [1,0,2,3,0,0,0,1,0] 3)),
             TestCase (assertEqual "line should be correct"
                       "         "
                       (line [1,0,2,3,0,0,0,1,0] 4))
           ]

tests :: Test
tests = TestList [ TestLabel "each" eachTests,
                   TestLabel "skips" skipsTests,
                   TestLabel "localMaxima" localMaximaTests,
                   TestLabel "count" countTests,
                   TestLabel "line" lineTests
                 ]
