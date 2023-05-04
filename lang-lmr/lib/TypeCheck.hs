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

tcExp :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => LExp -> Sc -> Free f Ty
tcExp (Num _) _ = return numT
tcExp Tru _ = return boolT
tcExp Fls _ = return boolT
tcExp (Syntax.Id ident) g = do
  ds <- getQuery ident <&> map projTy
  case ds of
    []  -> err "No matching declarations found"
    [t] -> return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC
  where
    getQuery (LILiteral x) = query g re pShortest (matchDecl x)
    getQuery _ = err "Unsupported querying"
tcExp (Plus l r) g = binop l r numT numT g
tcExp (Minus l r) g = binop l r numT numT g
tcExp (Mult l r) g = binop l r numT numT g
tcExp (Eql l r) g = binop l r numT boolT g
tcExp (If c t e) g = do
  c' <- tcExp c g
  t' <- tcExp t g
  f' <- tcExp e g
  if c' == boolT then
    if t' == f' then return t' else err "If branches should be the same"
  else err "If expects boolean condition"
tcExp (Fn (k, t) b) g = do
  let t' = toTy t
  g' <- new
  edge g' P g
  sink g' V $ Var k t'
  t'' <- tcExp b g'
  return $ funT t' t''
tcExp (App f a) g = do
  f' <- tcExp f g
  a' <- tcExp a g
  case f' of
    (T.Term "->" [from, to]) | from == a' -> return to
    (T.Term "->" _) -> err "Application argument mismatch"
    _ -> err "Application expects function"
tcExp (LetRec (k, v) b) g = do
  v' <- tcExp v g
  g' <- new
  edge g' P g
  sink g' V $ Var k v'
  tcExp b g'

binop :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => LExp -> LExp -> Ty -> Ty -> Sc -> Free f Ty
binop l r input output g = do
  l' <- tcExp l g
  r' <- tcExp r g
  if l' == input && r' == input then
    return output
  else
    err "Binop type error"

-- Function that handles each language construct
tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f
      , Scope Sc Label Decl < f
      )
   => LProg -> Sc -> Free f Ty
tc _ _ = undefined

-- Typechecks the module system.
tcMod :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ModTree -> Sc -> Free f AnnotatedModTree
tcMod modl g = do
  annotated <- constrHierarchy modl g
  resImports annotated $ createModuleOrdering annotated
  return annotated

-- Tie it all together
runTC :: LProg -> Either String (Ty, Graph Label Decl)
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ handle_ hExists
        (tc e 0
        :: Free ( Exists Ty
                + Equals Ty
                + Scope Sc Label Decl
                + Error String
                + Nop )
                Ty
        ) 0
  in case x of
    Left err -> Left err
    Right (Left (UnificationError t1 t2), _)  -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (t, u), sg)                  ->
      let t' = explicate u t in
        Right (t', sg)

-- For debugging.
runTCExp :: LExp -> Either String (Ty, Graph Label Decl)
runTCExp e =
  let x = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ handle_ hExists
        (tcExp e 0
        :: Free ( Exists Ty
                + Equals Ty
                + Scope Sc Label Decl
                + Error String
                + Nop )
                Ty
        ) 0
  in case x of
    Left err -> Left err
    Right (Left (UnificationError t1 t2), _)  -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (t, u), sg)                  ->
      let t' = explicate u t in
        Right (t', sg)

-- For debugging.
runTCMod :: ModTree -> Either String (Graph Label Decl)
runTCMod t = fmap snd
        $ un
        $ handle hErr
        $ handle_ hScope (tcMod t 0) emptyGraph
