module Main where

import Test.HUnit

import Syntax
import TypeCheck (runTC, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)

runTCTest :: Prog -> IO ([[Type]], Graph Label Decl) 
runTCTest = either assertFailure return . runTC

testEmpty :: IO ()
testEmpty = do
  t <- runTCTest []
  assertEqual "Not empty" ([] :: [[Type]]) $ fst t

testBasicSingle :: IO ()
testBasicSingle = do
  t <- runTCTest [Module "foo" [] [Num 1, Num 2, Num 3]]
  assertEqual "Incorrect types" [[NumT, NumT, NumT]] $ fst t 

testBasicMultiple :: IO ()
testBasicMultiple = do
  t <- runTCTest [Module "foo" [] [Num 1, Num 2, Num 3], Module "bar" [] [Num 4]]
  assertEqual "Incorrect types" [[NumT, NumT, NumT], [NumT]] $ fst t 

testDeclSimple :: IO ()
testDeclSimple = do
  t <- runTCTest [Module "foo" [] [Num 1, VarDecl "x" $ Num 2]]
  assertEqual "Incorrect types" [[NumT, NumT]] $ fst t 

testModules :: IO ()
testModules = do
  t <- runTCTest example
  assertEqual "Incorrect types" [[NumT], [NumT]] $ fst t


tests :: Test
tests = TestList
    [ "testEmpty" ~: testEmpty
    , "testBasicSingle" ~: testBasicSingle
    , "testBasicMultiple" ~: testBasicSingle
    , "testDeclSimple" ~: testDeclSimple
    , "testModules" ~: testModules ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
