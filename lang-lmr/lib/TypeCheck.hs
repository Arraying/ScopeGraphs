module TypeCheck where

import Data.Functor
import qualified Data.Map as Map
import Data.Regex
import qualified Data.Term as T

import Free hiding (R)
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals
import Syntax
import Modules
import Data.List
import Debug.Trace
import Data.Maybe (catMaybes)

data Label
  = P -- Lexical parent.
  | I -- Import.
  | M -- Module.
  | V -- Variable.
  deriving (Show, Eq)

data Decl
  = Var String Ty -- Variable declaration.
  | Modl String Sc -- Module declaration.
  deriving (Eq)

instance Show Decl where
  show (Var x t) = x ++ " : " ++ show t
  show (Modl x s) = "module " ++ x ++ " @ " ++ show s

projTy :: Decl -> Ty
projTy (Var _ t) = t
projTy (Modl _ _) = error "Critical runtime error: projecting module"

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

-- Regular expression (P|I)*M
re' :: RE Label
re' = Dot (Star $ Pipe (Atom P) (Atom I)) $ Atom M

re'' :: RE Label
re'' = Dot (Star $ Atom P) $ Atom M

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Var x' _) = x == x'
matchDecl x (Modl x' _) = x == x'

------------------
-- Scope Graphs --
------------------

-- Registers all the modules in the scope graph in a hierarchical fashion.
constrHierarchy :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ModTree -> Sc -> Free f AnnotatedModTree
-- In the root level, we just take the root scope and add any information.
constrHierarchy (Anon imports children decls) g = do
  children' <- mapM (`constrHierarchy` g) children
  return $ AAnon g imports children' decls
-- In any other case, we recursively create the tree and annotate it.
constrHierarchy (Named name imports children decls) g = do
  -- Create a new scope for this module, it can be nested.
  g' <- new
  edge g' P g
  -- Register the module with its parent scope.
  sink g M $ Modl name g'
  -- Now we recursively create and annotate all the children.
  children' <- mapM (`constrHierarchy` g') children
  -- Return the annotated tree with the scope.
  return $ ANamed g' name imports children' decls

-- Create all declarations.
constrDecls :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => AnnotatedModTree -> Free f [(Sc, Ty, LExp)]
constrDecls (AAnon g _ children decls) = constrDecls' g children decls
constrDecls (ANamed g _ _ children decls) = constrDecls' g children decls

-- Specifically, create declarations of current module and recurse to child modules.
constrDecls' :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => Sc -> [AnnotatedModTree] -> [LDecl] -> Free f [(Sc, Ty, LExp)]
constrDecls' g children decls = do
  curr <- catMaybes <$> mapM (make g) decls
  rest <- concat <$> mapM constrDecls children
  return $ curr ++ rest
  where
    make g (LDef (s, e)) = do
      t <- exists
      sink g V $ Var s t
      return $ Just (g, t, e)
    make _ _ = return Nothing

resImports :: (Functor f, Error String < f, Scope Sc Label Decl < f) => AnnotatedModTree -> [ModSummary] -> Free f ()
resImports (AAnon g i children _) m = do
  trace "DOING IMPORTS FOR ANONYMOUS" $ resImport (g, i) m
  mapM_ (`resImports` m) children
resImports (ANamed g _ i children _) m = do
  resImport (g, i) m
  mapM_ (`resImports` m) children

resImport :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (Sc, [LModule]) -> [ModSummary] -> Free f ()
resImport (g, rawImports) m = do
  -- Get the correct order going.
  let imports = sortModules (map fst m) rawImports
  -- Run the algorithm.
  algorithm imports [g]
  where
    algorithm :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [LModule] -> [Sc] -> Free f ()
    algorithm [] _ = return ()
    algorithm xs@(_:_) [] = err $ "Could not resolve imports " ++ intercalate ", " (map show xs)
    algorithm importsLeft (f:fs) = do
      found <- trace ("LEFT " ++ show importsLeft ++ " FRONTIER " ++ show (f:fs)) $ mapM (`algorithm'` f) importsLeft
      -- Flatten.
      let found' = catMaybes found
      -- Now we draw all the edges.
      trace ("ALGORITHM RESPONSE IS " ++ show found) $ mapM_ (edge g I . snd) found'
      -- Update imports.
      let importsLeft' = importsLeft \\ map fst found'
      -- Now recursively call it again.
      algorithm importsLeft' $ fs ++ map snd found'
    algorithm' :: (Functor f, Error String < f, Scope Sc Label Decl < f) => LModule -> Sc -> Free f (Maybe (LModule, Sc))
    algorithm' imp from = do
      -- We try to query the particular import via P*M.
      res <- trace ("HOP RESOLVING " ++ show imp) hop from imp
      case res of
        Just g' -> return $ Just (imp, g')
        Nothing -> return Nothing
    hop from (LMLiteral s) = singularResolve from s
    hop from (LMNested s s') = do
      recursive <- hop from s
      case recursive of
        Nothing -> return Nothing
        Just x -> singularResolve x s'
    singularResolve from to = do
      res <- trace ("TRYING TO RESOLVE " ++ to) query from re'' pShortest $ matchDecl to
      case trace ("RES IS " ++ show res) res of
        [] -> return Nothing
        [Modl _ g'] -> return $ Just g'
        _ -> err $ "Found multiple ocurrences of " ++ to

------------------
-- Type Checker --
------------------

tc :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => LExp -> Sc -> Ty -> Free f ()
tc (Num _) _ t = equals t numT
tc Tru _ t = equals t boolT
tc Fls _ t = equals t boolT
tc (Syntax.Id ident) g t = do
  ds <- getQuery ident <&> map projTy
  case ds of
    []  -> err "No matching declarations found"
    [ty] -> equals t ty
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC
  where
    getQuery (LILiteral x) = query g re pShortest (matchDecl x)
    getQuery _ = err "Unsupported querying"
tc (Plus l r) g t = tcBinop l r numT numT g t
tc (Minus l r) g t = tcBinop l r numT numT g t
tc (Mult l r) g t = tcBinop l r numT numT g t
tc (Eql l r) g t = tcBinop l r numT boolT g t
tc (If c i f) g t = do
  c' <- exists
  tc c g c'
  equals c' boolT
  i' <- exists
  f' <- exists
  tc i g i'
  tc f g f'
  equals i' f'
  equals t i'
tc _ _ _ = err "Currently unsuppoted operator"

tcBinop :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => LExp -> LExp -> Ty -> Ty -> Sc -> Ty -> Free f ()
tcBinop l r wantInput wantOutput g t = do
  equals wantOutput t
  tc l g wantInput
  tc r g wantInput

-- Typechecks the module system.
tcMod :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ModTree -> Sc -> Free f AnnotatedModTree
tcMod modl g = do
  annotated <- constrHierarchy modl g
  resImports annotated $ createModuleOrdering annotated
  return annotated

tcAll :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => LProg -> Sc -> Free f ()
tcAll e g = do
  annotatedModTree <- tcMod (createModuleTree e) g
  decls <- constrDecls annotatedModTree
  mapM_ (\(g, t, e) -> tc e g t) decls

-- Tie it all together
runTC :: LProg -> Either String (Graph Label Decl)
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ handle_ hExists (tcAll e 0
        :: Free ( Exists Ty
                + Equals Ty
                + Scope Sc Label Decl
                + Error String
                + Nop )
                ()
        ) 0
  in case x of
    Left err -> Left err
    Right (Left (UnificationError t1 t2), _)  -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (_, _), sg)                  -> Right sg

-- For debugging.
runTCMod :: ModTree -> Either String (Graph Label Decl)
runTCMod t = fmap snd
        $ un
        $ handle hErr
        $ handle_ hScope (tcMod t 0) emptyGraph
