module Main where

import Test.HUnit

import Syntax
import TypeCheck (runTC, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)

runTCTest :: Prog -> IO ([[Type]], Graph Label Decl) 
runTCTest = either assertFailure return . runTC

-- Define your test cases like the following
testApplicationPlus :: IO ()
testApplicationPlus = do
  assertEqual "Test" 1 1

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "testApplicationPlus" ~: testApplicationPlus ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
