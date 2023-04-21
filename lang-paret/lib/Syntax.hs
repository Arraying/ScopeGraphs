module Syntax where
import Data.List (intercalate)

data Type
  = NumT
  | BoolT
  | FunT Type Type
  | ListT Type
  | TupT [Type]
  deriving Eq

data Expr
  = Num Int
  | Tru
  | Fls
  | Plus Expr Expr
  | Conditional Expr Expr Expr
  | Nil Type -- No inferrence yet.
  | Cons Expr Expr
  | Head Expr
  | Tail Expr
  | Tuple [Expr]
  | Index Int Expr
  | Let (String, Type, Expr) Expr
  | App Expr Expr
  | Ident String
  | Abs String Type Expr
  deriving (Eq, Show)

instance Show Type where
  show NumT = "num"
  show BoolT = "bool"
  show (FunT ti to) = "(" ++ show ti ++ " -> " ++ show to ++ ")"
  show (ListT i) = "[" ++ show i ++ "]"
  show (TupT t) = "(" ++ intercalate ", " (map show t) ++ ")"

example :: Expr
example = App (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (Num 21)
