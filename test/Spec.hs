import Test.HUnit
import qualified Hw1Test
import qualified Hw2Test

tests :: Test
tests = TestList [
  Hw1Test.tests,
  Hw2Test.tests
  ]

main :: IO Counts
main = runTestTT tests
