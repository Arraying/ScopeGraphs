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
  = Var String Type -- Variable declaration.
  | Modl String Sc -- Module declaration.
  deriving (Eq)


instance Show Decl where
  show (Var x t) = x ++ " : " ++ show t
  show (Modl x s) = "module " ++ x ++ " @ " ++ show s

projTy :: Decl -> Type
projTy (Var _ t) = t
projTy (Modl _ _) = ModuleT

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression (P|I)*V
re :: RE Label
re = Dot (Star $ Pipe (Atom P) (Atom I)) $ Atom V

-- Regular expression P*Mod
re' :: RE Label
re' = Dot (Star $ Atom P) $ Atom M

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Var x' _) = x == x'
matchDecl x (Modl x' _) = x == x'

-- Phase 1: create graph of modules.
phase1 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Prog -> Sc -> Free f [Sc]
phase1 mods g = mapM modl mods
  where
    -- This will return the scope of every module.
    modl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Module -> Free f Sc
    modl (Module x _ _) = do
      -- Create the new scope for this particular module, link it to the parent.
      g' <- new
      edge g' P g
      -- Now "register" the module with the parent.
      sink g M $ Modl x g'
      return g'

-- Phase 2: add import edges to module graph.
phase2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Prog' -> Free f ()
phase2 = mapM_ resolve
  where
    -- Resolves all imports of a module
    resolve :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (Module, Sc) -> Free f ()
    resolve (Module _ is _, g') = mapM_ (`impt` g') is -- Import individually.
    -- This will attempt to create a singular import edge given a specific "current" scope.
    impt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => String -> Sc -> Free f ()
    impt name g' = do
      -- Let's get all the search results.
      ds <- query g' re' pShortest (matchDecl name)
      -- We need to match to determine ambiguity.
      case ds of
        [] -> err $ "A module by the name of '" ++ name ++ "' does not exist"
        [Modl _ g''] -> do
          -- Draw an edge from our current module g' to the imported g''.
          edge g' I g''
        _ -> err $ "Multiple modules by the name of '" ++ name ++ "' exist"

-- Phase 3: perform type checking of all expressions.
phase3 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Expr -> Sc -> Free f Type
phase3 (Num _) _ = return NumT
phase3 (Ident x) g = do
  ds <- query g re pShortest (matchDecl x) <&> map projTy
  case ds of
    []  -> err $ "No matching declarations found for '" ++ x ++ "'"
    [t] -> return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC
phase3 (VarDecl k v) g = do
  -- First resolve the type (spoiler alert, number).
  t <- phase3 v g
  -- Add the declaration to the current scope. Do NOT start a new scope.
  sink g V $ Var k t
  -- Type!
  return t

-- Perform the actual type checker.
tc :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Prog -> Sc -> Free f [[Type]]
tc p g = do
  -- Construct the graph, it will be handled by the monad, we can ignore unit here.
  scopes <- phase1 p g
  let p' = zip p scopes
  _ <- phase2 p'
  -- Now we need to recursively typecheck.
  mapM checkModule p'
  where
    checkModule :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (Module, Sc) -> Free f [Type]
    checkModule (Module _ _ es, g) = mapM (`phase3` g) es

-- Tie it all together
runTC :: Prog -> Either String ([[Type]], Graph Label Decl)
runTC p = un
        $ handle hErr
        $ handle_ hScope (tc p 0) emptyGraph

-- For debugging!
runPhase1 :: Prog -> Either String (Graph Label Decl)
runPhase1 p = fmap snd
        $ un
        $ handle hErr
        $ handle_ hScope (phase1 p 0) emptyGraph

-- For debugging!
runPhase2 :: Prog -> Either String (Graph Label Decl)
runPhase2 p = fmap snd
        $ un
        $ handle hErr
        $ handle_ hScope (phase1 p 0 >>= (phase2 . zip p)) emptyGraph