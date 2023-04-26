module TypeCheck where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax

data Label
  = P -- Lexical parent.
  | I -- Import.
  | M -- Module.
  | V -- Variable.
  deriving (Show, Eq)

data Decl
  = Decl String Type -- Variable declaration.
  | Modl String Sc -- Module declaration.
  deriving (Eq)


instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t
  show (Modl x s) = "module " ++ x ++ " @ " ++ show s

projTy :: Decl -> Type
projTy (Decl _ t) = t
projTy (Modl _ _) = ModuleT

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom V

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (Modl x' _) = x == x'

-- Phase 1: create graph of modules and imports.
phase1 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Prog -> Sc -> Free f ()
phase1 _ g = undefined

-- Phase 2: perform type checking of all expressions.
phase2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Expr -> Sc -> Free f Type
phase2 _ g = undefined

-- Perform the actual type checker.
tc :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Prog -> Sc -> Free f [[Type]]
tc p g = do
  -- Construct the graph, it will be handled by the monad, we can ignore unit here.
  _ <- phase1 p g
  -- Now we need to recursively typecheck.
  mapM checkModule p
  where
    checkModule :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Module -> Free f [Type]
    checkModule (Module _ _ es) = mapM (`phase2` 0) es

-- Tie it all together
runTC :: Prog -> Either String ([[Type]], Graph Label Decl)
runTC p = un
        $ handle hErr
        $ handle_ hScope (tc p 0) emptyGraph
