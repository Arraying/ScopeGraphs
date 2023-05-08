{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Data.Term

-- Current types:
-- * Numbers
-- * Booleans
-- * Some named type.
-- * Functions from A -> B
type Ty = Term Int

instance Show Ty where
  show (Const i) = "α" ++ show i
  show (Var i) = "α" ++ show i
  show (Term "->" [t1, t2]) = show t1 ++ " -> " ++ show t2
  show (Term "Num" []) = "__Num__"
  show (Term "Bool" []) = "__Bool__"
  show _ = "unknown construct"

numT :: Term c
numT = Term "Num" []
boolT :: Term c
boolT = Term "Bool" []
funT :: Term c -> Term c -> Term c
funT f t = Term "->" [f, t]


-- Mostly from: https://dl-acm-org.tudelft.idm.oclc.org/doi/pdf/10.1145/2847538.2847543 
type LProg = [LDecl]
data LDecl
  = LMod String [LDecl]
  | LImport LModule
  | LDef (String, LExp)
  -- | Record String [LFDecl] We do not care about records for the time being.
  deriving (Eq, Show)
data LModule
  = LMLiteral String 
  | LMNested LModule String
  deriving (Eq, Show)
data LFDecl = LFDecl String LType deriving (Eq, Show)
data LType
  = LInt
  | LBool
  | LFn LType LType
  deriving (Eq, Show)
data LExp -- Do not prefix for brevity.
  = Num Int
  | Tru
  | Fls
  | Id LIdent
  | Plus LExp LExp
  | Minus LExp LExp
  | Mult LExp LExp
  | Eql LExp LExp
  | If LExp LExp LExp
  | Fn (String, LType) LExp
  | App LExp LExp
  | LetRec (String, LExp) LExp
  -- | New String [LFBind] Skipped, for now.
  -- | With LExp LExp Skipepd, for now.
  -- | Ref LExp String Skipped, for now.
  deriving (Eq, Show)
data LIdent
  = LILiteral String
  | LINested LModule String
  deriving (Eq, Show)
data LFBind = LFBind String LExp deriving (Eq, Show)

toTy :: LType -> Ty
toTy LInt = numT
toTy LBool = boolT
toTy (LFn f t) = funT (toTy f) (toTy t)

example :: LExp
example = Num 1
