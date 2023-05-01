module TypeCheck where

import Data.Functor
import qualified Data.Map as Map
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals
import Syntax

data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  deriving (Show, Eq)

data Decl
  = Decl String Ty   -- Variable declaration
  deriving (Eq)

instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t

projTy :: Decl -> Ty
projTy (Decl _ t) = t

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom D

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'

------------------
-- Type Checker --
------------------

-- Function that handles each language construct
tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f
      , Scope Sc Label Decl < f
      )
   => LProg -> Sc -> Free f Ty
tc _ _ = undefined


-- Tie it all together
runTC :: LProg -> Either String (Ty, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ handle_ hScope (tc e 0) emptyGraph