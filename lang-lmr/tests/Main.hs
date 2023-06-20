module Main where

import Test.HUnit

import Data.Either (isRight)
import AParser
import TypeCheck (runTC, runTCMod, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import Syntax
import Modules
import Debug.Trace (trace)

runTCTest :: LProg -> IO (Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: LProg -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e

-- Define your test cases like the following
testApplicationPlus :: IO ()
testApplicationPlus = do
  assertEqual "Incorrect type" 1 1

tests :: Test
tests = TestList [ "End to end tests" ~: e2e ]

trialAndError :: Test
trialAndError = TestList [ "Trial and error" ~: testeroo ]

testeroo :: IO ()
testeroo = runE2ETest "./aterm-res/lmr/modules/import-shadowing-twice.fn.aterm" runTCTest

e2e :: Test
e2e = TestList
  [ "./aterm-res/lmr/empty.aterm" ~: testE2E1
  , "./aterm-res/lmr/definitions/missing-def.no.aterm" ~: testE2E2
  , "./aterm-res/lmr/definitions/param-shadows-def.aterm" ~: testE2E3
  , "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" ~: testE2E4
  , "./aterm-res/lmr/definitions/rec-defs.aterm" ~: testE2E5
  , "./aterm-res/lmr/definitions/rec-function-def.aterm" ~: testE2E6
  , "./aterm-res/lmr/definitions/rec-function-letrec.aterm" ~: testE2E7
  , "./aterm-res/lmr/definitions/seq-defs.aterm" ~: testE2E8
  , "./aterm-res/lmr/definitions/type-mismatch.no.aterm" ~: testE2E9
  , "./aterm-res/lmr/modules/cycles-big.aterm" ~: testE2E10
  , "./aterm-res/lmr/modules/cycles-self.aterm" ~: testE2E11
  , "./aterm-res/lmr/modules/cycles-simple.aterm" ~: testE2E12
  , "./aterm-res/lmr/modules/cycles-with-defs.aterm" ~: testE2E13
  , "./aterm-res/lmr/modules/import-ambiguity.no.aterm" ~: testE2E14
  , "./aterm-res/lmr/modules/import-inner-then-outer.aterm" ~: testE2E15
  , "./aterm-res/lmr/modules/import-outer-inner.aterm" ~: testE2E16
  , "./aterm-res/lmr/modules/import-outer-then-inner.aterm" ~: testE2E17
  , "./aterm-res/lmr/modules/import-shadowing-twice.aterm" ~: testE2E18
  , "./aterm-res/lmr/modules/import-shadowing.aterm" ~: testE2E19
  , "./aterm-res/lmr/modules/import-sibling.aterm" ~: testE2E20
  , "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" ~: testE2E21
  , "./aterm-res/lmr/modules/inner-shadows-outer.aterm" ~: testE2E22
  , "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" ~: testE2E23
  , "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" ~: testE2E24
  , "./aterm-res/lmr/modules/qual-ref.aterm" ~: testE2E25
  , "./aterm-res/lmr/modules/two-level-qual-ref.aterm" ~: testE2E26 ]

testE2E1 :: IO ()
testE2E1 = runE2ETest "./aterm-res/lmr/empty.aterm" runTCTest

testE2E2 :: IO ()
testE2E2 = runE2ETest "./aterm-res/lmr/definitions/missing-def.no.aterm" runTCFail

testE2E3 :: IO ()
testE2E3 = runE2ETest "./aterm-res/lmr/definitions/param-shadows-def.aterm" runTCTest

testE2E4 :: IO ()
testE2E4 = runE2ETest "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" runTCFail

testE2E5 :: IO ()
testE2E5 = runE2ETest "./aterm-res/lmr/definitions/rec-defs.aterm" runTCTest

testE2E6 :: IO ()
testE2E6 = runE2ETest "./aterm-res/lmr/definitions/rec-function-def.aterm" runTCTest

testE2E7 :: IO ()
testE2E7 = runE2ETest "./aterm-res/lmr/definitions/rec-function-letrec.aterm" runTCTest

testE2E8 :: IO ()
testE2E8 = runE2ETest "./aterm-res/lmr/definitions/seq-defs.aterm" runTCTest

testE2E9 :: IO ()
testE2E9 = runE2ETest "./aterm-res/lmr/definitions/type-mismatch.no.aterm" runTCFail

testE2E10 :: IO ()
testE2E10 = runE2ETest "./aterm-res/lmr/modules/cycles-big.aterm" runTCTest

testE2E11 :: IO ()
testE2E11 = runE2ETest "./aterm-res/lmr/modules/cycles-self.aterm" runTCTest

testE2E12 :: IO ()
testE2E12 = runE2ETest "./aterm-res/lmr/modules/cycles-simple.aterm" runTCTest

testE2E13 :: IO ()
testE2E13 = runE2ETest "./aterm-res/lmr/modules/cycles-with-defs.aterm" runTCTest

testE2E14 :: IO ()
testE2E14 = runE2ETest "./aterm-res/lmr/modules/import-ambiguity.no.aterm" runTCFail

testE2E15 :: IO ()
testE2E15 = runE2ETest "./aterm-res/lmr/modules/import-inner-then-outer.aterm" runTCTest

testE2E16 :: IO ()
testE2E16 = runE2ETest "./aterm-res/lmr/modules/import-outer-inner.aterm" runTCTest

testE2E17 :: IO ()
testE2E17 = runE2ETest "./aterm-res/lmr/modules/import-outer-then-inner.aterm" runTCTest

testE2E18 :: IO ()
testE2E18 = runE2ETest "./aterm-res/lmr/modules/import-shadowing-twice.aterm" runTCTest

testE2E19 :: IO ()
testE2E19 = runE2ETest "./aterm-res/lmr/modules/import-shadowing.aterm" runTCTest

testE2E20 :: IO ()
testE2E20 = runE2ETest "./aterm-res/lmr/modules/import-sibling.aterm" runTCTest

testE2E21 :: IO ()
testE2E21 = runE2ETest "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" runTCFail

testE2E22 :: IO ()
testE2E22 = runE2ETest "./aterm-res/lmr/modules/inner-shadows-outer.aterm" runTCTest

testE2E23 :: IO ()
testE2E23 = runE2ETest "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" runTCTest

testE2E24 :: IO ()
testE2E24 = runE2ETest "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" runTCTest

testE2E25 :: IO ()
testE2E25 = runE2ETest "./aterm-res/lmr/modules/qual-ref.aterm" runTCTest

testE2E26 :: IO ()
testE2E26 = runE2ETest "./aterm-res/lmr/modules/two-level-qual-ref.aterm" runTCTest

runParseTest :: String -> IO ()
runParseTest s = do
  p <- parse s
  print p
  assertEqual "It parses" (isRight p) True

runBasisTest :: String -> Bool -> IO ()
runBasisTest s v = do
  p <- parse s
  case p of
    Left _ -> assertFailure "Failed to parse"
    Right p -> intentionalBehaviour "Type checks" v $! runTC p

runModuleTest :: String -> Bool -> IO ()
runModuleTest s v = do
  p <- parse s
  case p of
    Left _ -> assertFailure "Failed to parse"
    Right m -> intentionalBehaviour "Module works" v $! runTCMod $ createModuleTree m

intentionalBehaviour :: String -> Bool -> Either String (Graph Label Decl) -> IO ()
intentionalBehaviour message expected res = do
  trace "Inentional behaviour" $ print' $! res
  assertEqual message expected $ isRight res

runE2ETest :: String -> (LProg -> IO a) -> IO ()
runE2ETest s runner = do
  p <- parse s
  case p of
    Left _ -> assertFailure "Failed to parse"
    Right m -> do
      _ <- runner m
      return ()

print' :: Either String (Graph Label Decl) -> IO ()
print' (Right g) = print g
print' (Left e) = putStrLn $ "Received error message: " ++ e

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
