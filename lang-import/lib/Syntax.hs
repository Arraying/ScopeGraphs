module Syntax where

data Type
  = UnitT
  | ModuleT
  | NumT
  deriving Eq
-- To use inference, replace `Type` with
-- type Ty = Term Int
-- (Term imported from Data.Term)
-- See also `lang-hm/Syntax` for an example.

type Prog = [Module]

data Module
  = Module String [String] [Expr]

data Expr
  = Num Int
  | Ident String
  | VarDecl String Expr
  deriving (Eq, Show)

instance Show Type where
  show UnitT = "void"
  show ModuleT = "module type"
  show NumT = "num"

example :: Prog
example = 
  [ Module "a" [] [VarDecl "x" $ Num 1]
  , Module "b" ["a"] [VarDecl "y" $ Ident "x"] ]
