module Hw2Test
  ( tests
  ) where

import Log
import LogAnalysis
import Test.HUnit

parseMessageTests :: Test
parseMessageTests =
  TestList [ TestCase (assertEqual "unknown"
                       (Unknown "?")
                       (parseMessage "?")),
             TestCase (assertEqual "info"
                       (LogMessage Info 1 "info info")
                       (parseMessage "I 1 info info")),
             TestCase (assertEqual "warn"
                       (LogMessage Warning 1 "warn warn")
                       (parseMessage "W 1 warn warn")),
             TestCase (assertEqual "error"
                       (LogMessage (Error 2) 1 "error error")
                       (parseMessage "E 2 1 error error"))
           ]

tests :: Test
tests = TestList [ TestLabel "parseMessage" parseMessageTests
                 ]
