import Test.HUnit
import qualified Hw1Test

tests :: Test
tests = TestList [Hw1Test.tests]

main :: IO Counts
main = runTestTT tests
