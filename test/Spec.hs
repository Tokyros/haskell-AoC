import Test.Tasty.HUnit
import Test.Tasty

testPass :: TestTree
testPass = testCase "1 + 1 should be 2" $ do
    (1 + 1) @?= 2

testFail :: TestTree
testFail = testCase "1 + 2 should be 3" $ do
	(1 + 2) @?= 4

tests :: TestTree
tests = testGroup "describe?" ([testPass, testFail])

main :: IO ()
main = do
	(defaultMain tests)
	return ()
