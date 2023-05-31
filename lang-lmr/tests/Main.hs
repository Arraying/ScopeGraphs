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
tests = TestList
  [ "Trial and error tests" ~: trialAndError
  , "End to end tests" ~: e2e
  , "Parsing tests" ~: parser
  , "Basis tests" ~: basis
  , "Modules tests" ~: modules ]

trialAndError :: Test
trialAndError = TestList [ "Trial and error" ~: testeroo ]

testeroo :: IO ()
testeroo = runE2ETest "./aterm-res/lmr/modules/import-shadowing.aterm" runTCTest

parser :: Test
parser = TestList
  [ "./aterm-res/lmr/empty.aterm" ~: testP1
  , "./aterm-res/lmr/definitions/missing-def.no.aterm" ~: testP2
  , "./aterm-res/lmr/definitions/param-shadows-def.aterm" ~: testP3
  , "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" ~: testP4
  , "./aterm-res/lmr/definitions/rec-defs.aterm" ~: testP5
  , "./aterm-res/lmr/definitions/rec-function-def.aterm" ~: testP6
  , "./aterm-res/lmr/definitions/rec-function-letrec.aterm" ~: testP7
  , "./aterm-res/lmr/definitions/seq-defs.aterm" ~: testP8
  , "./aterm-res/lmr/definitions/type-mismatch.no.aterm" ~: testP9
  , "./aterm-res/lmr/modules/import-inner-then-outer.aterm" ~: testP10
  , "./aterm-res/lmr/modules/import-outer-inner.aterm" ~: testP11
  , "./aterm-res/lmr/modules/import-outer-then-inner.aterm" ~: testP12
  , "./aterm-res/lmr/modules/import-shadowing.aterm" ~: testP13
  , "./aterm-res/lmr/modules/import-sibling.aterm" ~: testP14
  , "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" ~: testP15
  , "./aterm-res/lmr/modules/inner-shadows-outer.aterm" ~: testP16
  , "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" ~: testP17
  , "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" ~: testP18
  , "./aterm-res/lmr/modules/qual-ref.aterm" ~: testP19
  , "./aterm-res/lmr/modules/two-level-qual-ref.aterm" ~: testP20 ]

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
  , "./aterm-res/lmr/modules/import-inner-then-outer.aterm" ~: testE2E14
  , "./aterm-res/lmr/modules/import-outer-inner.aterm" ~: testE2E15
  , "./aterm-res/lmr/modules/import-outer-then-inner.aterm" ~: testE2E16
  , "./aterm-res/lmr/modules/import-shadowing.aterm" ~: testE2E17
  , "./aterm-res/lmr/modules/import-sibling.aterm" ~: testE2E18
  , "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" ~: testE2E19
  , "./aterm-res/lmr/modules/inner-shadows-outer.aterm" ~: testE2E20
  , "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" ~: testE2E21
  , "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" ~: testE2E22
  , "./aterm-res/lmr/modules/qual-ref.aterm" ~: testE2E23
  , "./aterm-res/lmr/modules/two-level-qual-ref.aterm" ~: testE2E24 ]

basis :: Test
basis = TestList
  [ "./aterm-res/lmr/definitions/missing-def.no.aterm" ~: testB1
  , "./aterm-res/lmr/definitions/param-shadows-def.aterm" ~: testB2
  , "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" ~: testB3
  , "./aterm-res/lmr/definitions/rec-defs.aterm" ~: testB4
  , "./aterm-res/lmr/definitions/rec-function-def.aterm" ~: testB5
  , "./aterm-res/lmr/definitions/rec-function-letrec.aterm" ~: testB6
  , "./aterm-res/lmr/definitions/seq-defs.aterm" ~: testB7
  , "./aterm-res/lmr/definitions/type-mismatch.no.aterm" ~: testB8 ]

modules :: Test
modules = TestList
  [ "./aterm-res/lmr/modules/cycles-big.aterm" ~: testM1
  , "./aterm-res/lmr/modules/cycles-self.aterm" ~: testM2
  , "./aterm-res/lmr/modules/cycles-simple.aterm" ~: testM3
  , "./aterm-res/lmr/modules/cycles-with-defs.aterm" ~: testM4
  , "./aterm-res/lmr/modules/import-inner-then-outer.aterm" ~: testM5
  , "./aterm-res/lmr/modules/import-outer-inner.aterm" ~: testM6
  , "./aterm-res/lmr/modules/import-outer-then-inner.aterm" ~: testM7
  , "./aterm-res/lmr/modules/import-shadowing.aterm" ~: testM8
  , "./aterm-res/lmr/modules/import-sibling.aterm" ~: testM9
  , "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" ~: testM10
  , "./aterm-res/lmr/modules/inner-shadows-outer.aterm" ~: testM11
  , "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" ~: testM12
  , "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" ~: testM13
  , "./aterm-res/lmr/modules/qual-ref.aterm" ~: testM14
  , "./aterm-res/lmr/modules/two-level-qual-ref.aterm" ~: testM15 ]

testP1 :: IO ()
testP1 = runParseTest "./aterm-res/lmr/empty.aterm"

testP2 :: IO ()
testP2 = runParseTest "./aterm-res/lmr/definitions/missing-def.no.aterm"

testP3 :: IO ()
testP3 = runParseTest "./aterm-res/lmr/definitions/param-shadows-def.aterm"

testP4 :: IO ()
testP4 = runParseTest "./aterm-res/lmr/definitions/param-shadows-def.no.aterm"

testP5 :: IO ()
testP5 = runParseTest "./aterm-res/lmr/definitions/rec-defs.aterm"

testP6 :: IO ()
testP6 = runParseTest "./aterm-res/lmr/definitions/rec-function-def.aterm"

testP7 :: IO ()
testP7 = runParseTest "./aterm-res/lmr/definitions/rec-function-letrec.aterm"

testP8 :: IO ()
testP8 = runParseTest "./aterm-res/lmr/definitions/seq-defs.aterm"

testP9 :: IO ()
testP9 = runParseTest "./aterm-res/lmr/definitions/type-mismatch.no.aterm"

testP10 :: IO ()
testP10 = runParseTest "./aterm-res/lmr/modules/import-inner-then-outer.aterm"

testP11 :: IO ()
testP11 = runParseTest "./aterm-res/lmr/modules/import-outer-inner.aterm"

testP12 :: IO ()
testP12 = runParseTest "./aterm-res/lmr/modules/import-outer-then-inner.aterm"

testP13 :: IO ()
testP13 = runParseTest "./aterm-res/lmr/modules/import-shadowing.aterm"

testP14 :: IO ()
testP14 = runParseTest "./aterm-res/lmr/modules/import-sibling.aterm"

testP15 :: IO ()
testP15 = runParseTest "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm"

testP16 :: IO ()
testP16 = runParseTest "./aterm-res/lmr/modules/inner-shadows-outer.aterm"

testP17 :: IO ()
testP17 = runParseTest "./aterm-res/lmr/modules/outer-visible-in-inner.aterm"

testP18 :: IO ()
testP18 = runParseTest "./aterm-res/lmr/modules/qual-ref-to-inner.aterm"

testP19 :: IO ()
testP19 = runParseTest "./aterm-res/lmr/modules/qual-ref.aterm"

testP20 :: IO ()
testP20 = runParseTest "./aterm-res/lmr/modules/two-level-qual-ref.aterm"

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
testE2E14 = runE2ETest "./aterm-res/lmr/modules/import-inner-then-outer.aterm" runTCTest

testE2E15 :: IO ()
testE2E15 = runE2ETest "./aterm-res/lmr/modules/import-outer-inner.aterm" runTCTest

testE2E16 :: IO ()
testE2E16 = runE2ETest "./aterm-res/lmr/modules/import-outer-then-inner.aterm" runTCTest

testE2E17 :: IO ()
testE2E17 = runE2ETest "./aterm-res/lmr/modules/import-shadowing.aterm" runTCTest

testE2E18 :: IO ()
testE2E18 = runE2ETest "./aterm-res/lmr/modules/import-sibling.aterm" runTCTest

testE2E19 :: IO ()
testE2E19 = runE2ETest "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" runTCFail

testE2E20 :: IO ()
testE2E20 = runE2ETest "./aterm-res/lmr/modules/inner-shadows-outer.aterm" runTCTest

testE2E21 :: IO ()
testE2E21 = runE2ETest "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" runTCTest

testE2E22 :: IO ()
testE2E22 = runE2ETest "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" runTCTest

testE2E23 :: IO ()
testE2E23 = runE2ETest "./aterm-res/lmr/modules/qual-ref.aterm" runTCTest

testE2E24 :: IO ()
testE2E24 = runE2ETest "./aterm-res/lmr/modules/two-level-qual-ref.aterm" runTCTest

testB1 :: IO ()
testB1 = runBasisTest "./aterm-res/lmr/definitions/missing-def.no.aterm" False

testB2 :: IO ()
testB2 = runBasisTest "./aterm-res/lmr/definitions/param-shadows-def.aterm" True

testB3 :: IO ()
testB3 = runBasisTest "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" False

testB4 :: IO ()
testB4 = runBasisTest "./aterm-res/lmr/definitions/rec-defs.aterm" True

testB5 :: IO ()
testB5 = runBasisTest "./aterm-res/lmr/definitions/rec-function-def.aterm" True

testB6 :: IO ()
testB6 = runBasisTest "./aterm-res/lmr/definitions/rec-function-letrec.aterm" True

testB7 :: IO ()
testB7 = runBasisTest "./aterm-res/lmr/definitions/seq-defs.aterm" True

testB8 :: IO ()
testB8 = runBasisTest "./aterm-res/lmr/definitions/type-mismatch.no.aterm" False

testM1 :: IO ()
testM1 = runModuleTest "./aterm-res/lmr/modules/cycles-big.aterm" True

testM2 :: IO ()
testM2 = runModuleTest "./aterm-res/lmr/modules/cycles-self.aterm" True

testM3 :: IO ()
testM3 = runModuleTest "./aterm-res/lmr/modules/cycles-simple.aterm" True

testM4 :: IO ()
testM4 = runModuleTest "./aterm-res/lmr/modules/cycles-with-defs.aterm" True

testM5 :: IO ()
testM5 = runModuleTest "./aterm-res/lmr/modules/import-inner-then-outer.aterm" True

testM6 :: IO ()
testM6 = runModuleTest "./aterm-res/lmr/modules/import-outer-inner.aterm" True

testM7 :: IO ()
testM7 = runModuleTest "./aterm-res/lmr/modules/import-outer-then-inner.aterm" True

testM8 :: IO ()
testM8 = runModuleTest "./aterm-res/lmr/modules/import-shadowing.aterm" True

testM9 :: IO ()
testM9 = runModuleTest "./aterm-res/lmr/modules/import-sibling.aterm" True

testM10 :: IO ()
testM10 = runModuleTest "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" True

testM11 :: IO ()
testM11 = runModuleTest "./aterm-res/lmr/modules/inner-shadows-outer.aterm" True

testM12 :: IO ()
testM12 = runModuleTest "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" True

testM13 :: IO ()
testM13 = runModuleTest "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" True

testM14 :: IO ()
testM14 = runModuleTest "./aterm-res/lmr/modules/qual-ref.aterm" True

testM15 :: IO ()
testM15 = runModuleTest "./aterm-res/lmr/modules/two-level-qual-ref.aterm" True

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
    result <- runTestTT trialAndError
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
