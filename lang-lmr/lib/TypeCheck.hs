module TypeCheck where

import qualified Data.Map as Map
import Data.Regex

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

-- Regular expression P*V TODO Set to P*I?V
re :: RE Label
re = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom I)) $ Atom V

-- Regular expression P*I?M
re' :: RE Label
re' = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom I)) $ Atom M

re'' :: RE Label
re'' = Dot (Star $ Atom P) $ Atom M

-- Path order based on Ministatix priorities.
pPriority :: PathOrder Label Decl
pPriority p1 p2 = val == LT || val == EQ
  where
    val = pPriority' p1 p2

pPriority' :: ResolvedPath Label Decl -> ResolvedPath Label Decl -> Ordering
pPriority' (ResolvedPath p1 _ _) (ResolvedPath p2 _ _) = comparePaths (extractPath p1) (extractPath p2)
  where
    comparePaths [] [] = EQ
    comparePaths (_:_) [] = GT
    comparePaths [] (_:_) = LT
    comparePaths (x:xs) (y:ys) = case compareLabel x y of
      Just r -> r
      Nothing -> comparePaths xs ys
    compareLabel M P = Just LT
    compareLabel P M = Just GT
    compareLabel M I = Just LT
    compareLabel I M = Just GT
    compareLabel V P = Just LT
    compareLabel P V = Just GT
    compareLabel V I = Just LT
    compareLabel I V = Just GT
    compareLabel I P = Just LT
    compareLabel P I = Just GT
    compareLabel _ _ = Nothing
    extractPath (Start _) = []
    extractPath (Step p l _) = extractPath p ++ [l]

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
    make g (LDef s e) = do
      t <- exists
      sink g V $ Var s t
      return $ Just (g, t, e)
    make _ _ = return Nothing

resImports :: (Functor f, Error String < f, Scope Sc Label Decl < f) => AnnotatedModTree -> [ModSummary] -> Free f ()
resImports (AAnon g i children _) m = do
  trace ("DOING IMPORTS FOR ANONYMOUS WITH " ++ show m) $ resImport (g, i) m
  mapM_ (`resImports` m) children
resImports (ANamed g _ i children _) m = do
  resImport (g, i) m
  mapM_ (`resImports` m) children

resImport :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (Sc, [LModule]) -> [ModSummary] -> Free f ()
resImport (g, rawImports) m = do
  -- Get the correct order going.
  let imports = sortModules (map fst m) rawImports
  -- Run the algorithm.
  algorithm m imports [g]
  where
    algorithm :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [ModSummary] -> [LModule] -> [Sc] -> Free f ()
    algorithm _ [] _ = return ()
    algorithm _ xs@(_:_) [] = err $ "Could not resolve imports " ++ intercalate ", " (map show xs)
    algorithm m importsLeft (f:fs) = do
      -- First we split into non-shadow imports and shadow imports.
      let (normalImportsLeft, shadowedImportsLeft) = trace ("LEFT " ++ show importsLeft ++ " FRONTIER " ++ show (f:fs)) $ createShadowSplit importsLeft (map fst m)
      -- The first part is to try and resolve all the non-shadowing imports.
      normalFound <- trace ("NORMAL ARE " ++ show normalImportsLeft ++ ", SHADOW ARE " ++ show shadowedImportsLeft) $ mapM (`algorithm'` f) normalImportsLeft
      let normalFound' = catMaybes normalFound
      -- Now we take the results and search from those and the frontier for ambiguous imports.
      shadowedFound <- mapM (\imp -> algorithm'' imp f (map snd normalFound')) shadowedImportsLeft
      let shadowedFound' = catMaybes shadowedFound
      -- We've resolved as many imports as possible.
      let found = normalFound' ++ shadowedFound'
      -- Draw all the edges.
      trace ("ALGORITHM RESPONSE IS " ++ show found) $ mapM_ (edge g I . snd) found
      -- Update imports.
      let importsLeft' = importsLeft \\ map fst found
      -- Now recursively call it again.
      algorithm m importsLeft' $ fs ++ map snd found
    algorithm' :: (Functor f, Error String < f, Scope Sc Label Decl < f) => LModule -> Sc -> Free f (Maybe (LModule, Sc))
    algorithm' imp from = do
      -- We try to query the particular import via P*M.
      res <- trace ("HOP RESOLVING " ++ show imp) hop singularResolve from imp
      case res of
        Just (g', _) -> return $ Just (imp, g')
        Nothing -> return Nothing
    algorithm'' :: (Functor f, Error String < f, Scope Sc Label Decl < f) => LModule -> Sc -> [Sc] -> Free f (Maybe (LModule, Sc))
    algorithm'' imp f fs = do
      -- We try to query the potential paths via P*M.
      fromOriginal <- hop multipleResolve f imp
      fromDiscovered <- mapM (\from -> hop multipleResolve from imp) fs
      -- Need to add the virtual import edge to make path comparison fair.
      let fromDiscovered' = map (fmap (\(g', ResolvedPath p l d) -> (g', ResolvedPath (addFakeImportEdgeToPath f p) l d))) fromDiscovered
      let paths = catMaybes $ fromOriginal : fromDiscovered'
      -- Return the most suitable path.
      let ordered = sortBy (\(_, p1) (_, p2) -> pPriority' p1 p2) paths
      case trace ("POSSIBLE ORDERED PATHS ARE " ++ show ordered) ordered of
        -- Nothing found, this is fine too.
        [] -> return Nothing
        -- Find minimum by LMR priority rules.
        _ -> return $ trace ("RETURNING ID " ++ show (fst $ head ordered)) $ Just (imp, fst $ head ordered)
    hop :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (Sc -> String -> Free f (Maybe (Sc, a))) -> Sc -> LModule -> Free f (Maybe (Sc, a))
    hop resolver from (LMLiteral s) = resolver from s
    hop resolver from (LMNested s s') = do
      recursive <- hop resolver from s
      case recursive of
        Nothing -> return Nothing
        Just (x, _) -> resolver x s'
    singularResolve :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> String -> Free f (Maybe (Sc, ()))
    singularResolve from to = do
      res <- trace ("SINGULAR TRYING TO RESOLVE " ++ to) query from re'' pPriority $ matchDecl to
      case trace ("SINGULAR RES IS " ++ show res) res of
        [] -> return Nothing
        [Modl _ g'] -> return $ Just (g', ())
        _ -> err $ "Found multiple ocurrences of " ++ to
    multipleResolve :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> String -> Free f (Maybe (Sc, ResolvedPath Label Decl))
    multipleResolve from to = do
      res <- trace ("MULTIPLE TRYING TO RESOLVE " ++ to) queryWithPath from re'' pPriority $ matchDecl to
      case trace ("MULTIPLE RES IS " ++ show res) res of
        [] -> return Nothing
        [ResolvedPath p l d@(Modl _ g)] -> return $ Just (g, ResolvedPath p l d)
        [ResolvedPath {}] -> err "Internal error resolving module lookups"
        _ -> err $ "Found multiple ocurrences of " ++ to
    addFakeImportEdgeToPath trueStart (Start fakeStart) = Step (Start trueStart) I fakeStart
    addFakeImportEdgeToPath trueStart (Step p l s) = Step (addFakeImportEdgeToPath trueStart p) l s

------------------
-- Type Checker --
------------------

tc :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => LExp -> Sc -> Ty -> Free f ()
tc (Num _) _ t = equals t numT
tc Tru _ t = equals t boolT
tc Fls _ t = equals t boolT
tc (Syntax.Id ident) g t = do
  t' <- tcLookup g (traceHops ident)
  equals t t'
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
tc (Fn (p, pt) b) g t = do
  let paramType = toTy pt
  t' <- exists
  g' <- new
  edge g' P g
  sink g' V (Var p paramType)
  tc b g' t'
  equals t (funT paramType t')
tc (App fn a) g t = do
  argType <- exists
  tc fn g (funT argType t)
  tc a g argType
tc (LetRec (v, e) b) g t = do
  t' <- exists
  g' <- new
  edge g' P g
  sink g' V (Var v t')
  tc e g' t'
  tc b g' t

tcLookup :: (Functor f, Exists Ty < f, Equals Ty < f, Error String < f, Scope Sc Label Decl < f) => Sc -> [String] -> Free f Ty
tcLookup g [x] = do
  ds <- query g re pPriority (matchDecl x)
  case ds of 
    [] -> err $ "Variable " ++ show x ++ " could not be resolved from scope " ++ show g
    [Var _ ty] -> return ty
    _   -> err $ "Variable " ++ show x ++ " could not be resolved to a single variable "
tcLookup g (x:xs) = do
  -- We need to make a hop from g to x.
  ds <- query g re' pPriority (matchDecl x)
  case ds of
    [] -> err $ "Module " ++ show x ++ " could not be resolved from scope " ++ show g
    [Modl _ g'] -> tcLookup g' xs
    _ -> err $ "Module " ++ show x ++ " could not be resolved to a single module"
tcLookup _ _ = err "Critical internal lookup error"

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
  decls <- trace "CONSTRUCTING DECLS" $ constrDecls annotatedModTree
  mapM_ (\(g, t, e) -> tc e g t) $ trace "TYPE CHECKING DECLS NOW" decls

-- Tie it all together
runTC :: LProg -> Either String (Graph Label Decl)
runTC e = trace "RUNNING TC " $
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
