import Test.HUnit
import qualified Hw1Test
import qualified Hw2Test
import qualified Hw3Test

tests :: Test
tests = TestList [
  Hw1Test.tests,
  Hw2Test.tests,
  Hw3Test.tests
  ]

main :: IO Counts
main = runTestTT tests
