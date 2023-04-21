module Main where

import Test.HUnit

import Control.Exception (catch, ErrorCall)
import Syntax (Expr(..), Type(..))
import TypeCheck (runTC, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)

runTCTest :: Expr -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: Expr -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e

runTCFail' :: Expr -> IO ()
runTCFail' e = runTCFail e >> pure ()

-- Define your test cases like the following
testApplicationPlus :: IO ()
testApplicationPlus = do
  t <- runTCTest $ App (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (Num 21)
  assertEqual "Incorrect type" NumT $ fst t

testIfValid :: IO ()
testIfValid = do
  t <- runTCTest $ Conditional Tru (Num 1) (Num 2)
  assertEqual "Incorrect type" NumT $ fst t

testIfCond :: IO ()
testIfCond = runTCFail' $ Conditional (Num 3) (Num 1) (Num 2)

testIfBranch :: IO ()
testIfBranch = runTCFail' $ Conditional Fls (Num 1) (Tru)

testListValid :: IO ()
testListValid = do
  t <- runTCTest (Cons Tru (Cons Fls (Cons Tru (Nil BoolT))))
  assertEqual "Incorrect type" (ListT BoolT) $ fst t

testListInvalid :: IO ()
testListInvalid = runTCFail' $ Cons Tru Fls

testHeadValid :: IO ()
testHeadValid = do
  t <- runTCTest (Head (Cons Tru (Cons Fls (Cons Tru (Nil BoolT)))))
  assertEqual "Incorrect type" BoolT $ fst t

testHeadInvalid :: IO ()
testHeadInvalid = runTCFail' $ Head $ Num 1

testTailValid :: IO ()
testTailValid = do
  t <- runTCTest (Tail (Cons Tru (Cons Fls (Cons Tru (Nil BoolT)))))
  assertEqual "Incorrect type" (ListT BoolT) $ fst t

testTailInvalid :: IO ()
testTailInvalid = runTCFail' $ Tail $ Num 1

testTupleEmpty :: IO ()
testTupleEmpty = do
  t <- runTCTest (Tuple [])
  assertEqual "Incorrect type" (TupT []) $ fst t

testTupleMany :: IO ()
testTupleMany = do
  t <- runTCTest (Tuple [Num 1, Tru, Fls, Tuple []])
  assertEqual "Incorrect type" (TupT [NumT, BoolT, BoolT, TupT []]) $ fst t

testLetSingle :: IO ()
testLetSingle = do
  t <- runTCTest $ Let ("x", NumT, Num 1) $ Plus (Num 2) (Ident "x")
  assertEqual "Incorrect type" NumT $ fst t

testLetFreeIdent :: IO ()
testLetFreeIdent = runTCFail' $ Let ("x", NumT, Num 1) $ Ident "y"

testLetShadowing :: IO ()
testLetShadowing = do
  t <- runTCTest $ Let ("x", NumT, Num 1) $ Let ("x", BoolT, Tru) $ Ident "x"
  assertEqual "Incorrect type" BoolT $ fst t

testLetPropagation :: IO ()
testLetPropagation = do
  t <- runTCTest $ Let ("x", NumT, Num 1) $ Let ("y", BoolT, Tru) $ Ident "x"
  assertEqual "Incorrect type" NumT $ fst t

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "testApplicationPlus" ~: testApplicationPlus
    , "testIfValid" ~: testIfValid
    , "testIfCond" ~: testIfCond
    , "testListValid" ~: testListValid
    , "testHeadValid" ~: testHeadValid
    , "testHeadInvalid" ~: testHeadInvalid
    , "testTailValid" ~: testTailValid
    , "testTailInvalid" ~: testTailInvalid
    , "testTupleEmpty" ~: testTupleEmpty
    , "testTupleMany" ~: testTupleMany
    , "testIndexValid" ~: testIndexValid
    , "testIndexInvalid" ~: testIndexInvalid
    , "testLetSingle" ~: testLetSingle
    , "testLetFreeIdent" ~: testLetFreeIdent
    , "testLetShadowing" ~: testLetShadowing
    , "testLetPropagation" ~: testLetPropagation ]

testIndexValid :: IO ()
testIndexValid = do
  t <- runTCTest $ Index 0 $ Tuple [Num 1, Tru, Fls, Tuple []]
  assertEqual "Incorrect type" NumT $ fst t

testIndexInvalid :: IO ()
testIndexInvalid = runTCFail' $ Index 1000 $ Tuple [Num 1, Tru, Fls, Tuple []]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
