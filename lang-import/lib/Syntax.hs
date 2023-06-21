module Syntax where

import Free.Scope (Sc)

data Type
  = ModuleT
  | NumT
  deriving Eq
-- To use inference, replace `Type` with
-- type Ty = Term Int
-- (Term imported from Data.Term)
-- See also `lang-hm/Syntax` for an example.

type Prog = [Module]
type Prog' = [(Module, Sc)]

data Module
  = Module String [String] [Expr]

data Expr
  = Num Int
  | Ident String
  | VarDecl String Expr
  deriving (Eq, Show)

instance Show Type where
  show ModuleT = "module"
  show NumT = "num"

example :: Prog
example = 
  [ Module "a" [] [VarDecl "x" $ Num 1]
  , Module "b" ["a"] [VarDecl "y" $ Num 2] ]
