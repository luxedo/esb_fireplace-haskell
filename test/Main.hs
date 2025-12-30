{-# OPTIONS_GHC -Wno-x-partial #-}

import           Control.Exception  (bracket)
import           Fireplace
import           GHC.IO.Handle      (hDuplicate, hDuplicateTo)
import           System.Environment (withArgs)
import           System.IO
import           System.Process     (createPipe)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Regex.TDFA    ((=~))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

runWithMock :: [String] -> String -> IO () -> IO [String]
runWithMock mockArgs mockInput action = do
  (readIn, writeIn) <- createPipe
  (readOut, writeOut) <- createPipe

  hPutStr writeIn mockInput
  hClose writeIn

  withArgs mockArgs $ do
    bracket
      ( do
          oldIn <- hDuplicate stdin
          oldOut <- hDuplicate stdout
          hDuplicateTo readIn stdin
          hDuplicateTo writeOut stdout
          return (oldIn, oldOut)
      )
      ( \(oldIn, oldOut) -> do
          hDuplicateTo oldIn stdin
          hDuplicateTo oldOut stdout
          hClose readIn
          hClose writeOut
      )
      (const action)
  lines <$> hGetContents readOut

assertTiming :: String -> Assertion
assertTiming timing = do
  let match = "^RT [0-9]+ ns$"
      isValid = timing =~ match :: Bool
  assertBool ("Timing line '" ++ timing ++ "' didn't match regex") isValid

unitTests :: TestTree
unitTests =
  testGroup
    "v1Run Integration"
    [ testCase "Selects Part One and shows timing" $ do
        output@(ans : timing : _) <- runWithMock ["--part", "1"] "my input" (v1Run solvePt1 solvePt2)
        length output @?= 2
        ans @?= "25"
        assertTiming timing
    , testCase "Selects Part Two" $ do
        output@(ans : timing : _) <- runWithMock ["--part", "2", "--args", "Reindeer"] "December" (v1Run solvePt1 solvePt2)
        length output @?= 2
        ans @?= "December Reindeer"
        assertTiming timing
    ]

solvePt1 :: String -> [String] -> Int
solvePt1 _ _ = 25

solvePt2 :: String -> [String] -> String
solvePt2 input args = input ++ " " ++ concat args
