module Main where

import Test.HUnit

import Data.Either (isRight)
import AParser
import TypeCheck (runTC, runTCMod, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import Syntax
import Modules

-- Define your test cases like the following
testApplicationPlus :: IO ()
testApplicationPlus = do
  assertEqual "Incorrect type" 1 1

tests :: Test
tests = TestList
  [ "Parsing tests" ~: parser
  , "Basis tests" ~: basis 
  , "Modules tests" ~: modules ]

parser :: Test
parser = TestList []
  -- [ "./aterm-res/lmr/empty.aterm" ~: testP1
  -- , "./aterm-res/lmr/definitions/missing-def.no.aterm" ~: testP2
  -- , "./aterm-res/lmr/definitions/param-shadows-def.aterm" ~: testP3
  -- , "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" ~: testP4
  -- , "./aterm-res/lmr/definitions/rec-defs.aterm" ~: testP5
  -- , "./aterm-res/lmr/definitions/rec-function-def.aterm" ~: testP6
  -- , "./aterm-res/lmr/definitions/rec-function-letrec.aterm" ~: testP7
  -- , "./aterm-res/lmr/definitions/seq-defs.aterm" ~: testP8
  -- , "./aterm-res/lmr/definitions/type-mismatch.no.aterm" ~: testP9
  -- , "./aterm-res/lmr/modules/import-inner-then-outer.aterm" ~: testP10
  -- , "./aterm-res/lmr/modules/import-outer-inner.aterm" ~: testP11
  -- , "./aterm-res/lmr/modules/import-outer-then-inner.aterm" ~: testP12
  -- , "./aterm-res/lmr/modules/import-sibling.aterm" ~: testP13
  -- , "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" ~: testP14
  -- , "./aterm-res/lmr/modules/inner-shadows-outer.aterm" ~: testP15
  -- , "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" ~: testP16
  -- , "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" ~: testP17
  -- , "./aterm-res/lmr/modules/qual-ref.aterm" ~: testP18
  -- , "./aterm-res/lmr/modules/two-level-qual-ref.aterm" ~: testP19 ]

basis :: Test
basis = TestList
  [ "test" ~: testB1 ]

modules :: Test
modules = TestList []

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
testP13 = runParseTest "./aterm-res/lmr/modules/import-sibling.aterm"

testP14 :: IO ()
testP14 = runParseTest "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm"

testP15 :: IO ()
testP15 = runParseTest "./aterm-res/lmr/modules/inner-shadows-outer.aterm"

testP16 :: IO ()
testP16 = runParseTest "./aterm-res/lmr/modules/outer-visible-in-inner.aterm"

testP17 :: IO ()
testP17 = runParseTest "./aterm-res/lmr/modules/qual-ref-to-inner.aterm"

testP18 :: IO ()
testP18 = runParseTest "./aterm-res/lmr/modules/qual-ref.aterm"

testP19 :: IO ()
testP19 = runParseTest "./aterm-res/lmr/modules/two-level-qual-ref.aterm"

testB1 :: IO ()
testB1 = runBasisTest "./aterm-res/lmr/definitions/missing-def.no.aterm" False

testB2 :: IO ()
testB2 = runBasisTest "./aterm-res/lmr/definitions/param-shadows-def.aterm" True

testB3 :: IO ()
testB3 = runBasisTest "./aterm-res/lmr/definitions/param-shadows-def.no.aterm" False

testB4 :: IO ()
testB4 = runBasisTest "./aterm-res/lmr/definitions/rec-defs.aterm" False

testB5 :: IO ()
testB5 = runBasisTest "./aterm-res/lmr/definitions/rec-function-def.aterm" True

testB6 :: IO ()
testB6 = runBasisTest "./aterm-res/lmr/definitions/rec-function-letrec.aterm" True

testB7 :: IO ()
testB7 = runBasisTest "./aterm-res/lmr/definitions/seq-defs.aterm" True

testB8 :: IO ()
testB8 = runBasisTest "./aterm-res/lmr/definitions/type-mismatch.no.aterm" False

testM1 :: IO ()
testM1 = runModuleTest "./aterm-res/lmr/modules/import-inner-then-outer.aterm" True

testM2 :: IO ()
testM2 = runModuleTest "./aterm-res/lmr/modules/import-outer-inner.aterm" False

testM3 :: IO ()
testM3 = runModuleTest "./aterm-res/lmr/modules/import-outer-then-inner.aterm" True

testM4 :: IO ()
testM4 = runModuleTest "./aterm-res/lmr/modules/import-shadowing.aterm" False

testM5 :: IO ()
testM5 = runModuleTest "./aterm-res/lmr/modules/import-sibling.aterm" True

testM6 :: IO ()
testM6 = runModuleTest "./aterm-res/lmr/modules/inner-invisible-in-outer.no.aterm" False

testM7 :: IO ()
testM7 = runModuleTest "./aterm-res/lmr/modules/inner-shadows-outer.aterm" True

testM8 :: IO ()
testM8 = runModuleTest "./aterm-res/lmr/modules/outer-visible-in-inner.aterm" True

testM9 :: IO ()
testM9 = runModuleTest "./aterm-res/lmr/modules/qual-ref-to-inner.aterm" True

testM10 :: IO ()
testM10 = runModuleTest "./aterm-res/lmr/modules/qual-ref.aterm" True

testM11 :: IO ()
testM11 = runModuleTest "./aterm-res/lmr/modules/two-level-qual-ref.aterm" True

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
    Right p -> let res = runTC p in do
      seq res print "aaa?"
      _ <- print' res
      assertEqual "Correct typing behaviour" v (isRight res)

runModuleTest :: String -> Bool -> IO ()
runModuleTest s v = do
  p <- parse s
  case p of 
    Left _ -> assertFailure "Failed to parse"
    Right m -> let res = (runTCMod $ createModuleTree m) in do
      print' res
      assertEqual "Module works" v (isRight res)

print' :: Either String (Graph Label Decl) -> IO ()
print' (Right g) = print g
print' (Left e) = print e

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
