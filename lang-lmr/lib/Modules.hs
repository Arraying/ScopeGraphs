module Modules where

import Syntax
import Free.Scope
import Data.List
import Data.Maybe

data ModTree
  = Anon [LModule] [ModTree] [LDecl]
  | Named String [LModule] [ModTree] [LDecl]
  deriving (Eq, Show)

data AnnotatedModTree
  = AAnon Sc [LModule] [AnnotatedModTree] [LDecl]
  | ANamed Sc String [LModule] [AnnotatedModTree] [LDecl]
  deriving (Eq, Show)

type ModWrapped = (String, [LDecl])
type ModSummary = (String, Sc)

createModuleTree :: LProg -> ModTree
createModuleTree xs = let (is, ms, ls) = extract xs in Anon is (map traverseModule ms) ls

createModuleOrdering :: AnnotatedModTree -> [ModSummary]
createModuleOrdering a = bfs [a]
  where
    bfs [] = []
    bfs ((AAnon _ _ children _):xs) = bfs $ xs ++ children
    bfs ((ANamed g n _ children _):xs) = (:) (n, g) $ bfs $ xs ++ children

createModuleHops :: LModule -> [String]
createModuleHops (LMLiteral s) = [s]
createModuleHops (LMNested r s) = createModuleHops r ++ [s]

createShadowSplit :: [LModule] -> [String] -> ([LModule], [LModule])
createShadowSplit rawImports modNames = (filter (shouldBeDuplicate False) rawImports, filter (shouldBeDuplicate True) rawImports)
  where
    shouldBeDuplicate b i = (moduleHead i `elem` duplicateNames) == b
    duplicateNames = modNames \\ nub modNames

isEverythingImported :: AnnotatedModTree -> Bool
isEverythingImported (AAnon _ imports children _) = null imports && all isEverythingImported children
isEverythingImported (ANamed _ _ imports children _) = null imports && all isEverythingImported children

traceUnimported :: AnnotatedModTree -> [String]
traceUnimported (AAnon _ imports children _) = map (pr Nothing) imports ++ concatMap traceUnimported children
traceUnimported (ANamed _ n imports children _) = map (pr $ Just n) imports ++ concatMap traceUnimported children

moduleHead :: LModule -> String
moduleHead (LMLiteral s) = s
moduleHead (LMNested s _) = moduleHead s

sortModules :: [String] -> [LModule] -> [LModule]
sortModules ms = sortBy sorter
  where
    sorter l r = compare (fromMaybe (-1) $ elemIndex (moduleHead l) ms) (fromMaybe (-1) $ elemIndex (moduleHead r) ms)

pr :: Maybe String -> LModule -> String
pr q s = par q ++ intercalate "." (createModuleHops s)
  where
    par Nothing = ""
    par (Just s) = s ++ ": "

traverseModule :: ModWrapped -> ModTree
traverseModule (name, xs) = let (is, ms, ls) = extract xs in Named name is (map traverseModule ms) ls

extract :: [LDecl] -> ([LModule], [ModWrapped], [LDecl])
extract = extract' ([], [], [])

extract' :: ([LModule], [ModWrapped], [LDecl]) -> [LDecl] -> ([LModule], [ModWrapped], [LDecl])
extract' s [] = s
extract' (is, ms, ls) ((LImport x):xs) = extract' (x:is, ms, ls) xs
extract' (is, ms, ls) ((LMod n ds):xs) = extract' (is, (n, ds) : ms, ls) xs
extract' (is, ms, ls) (l:xs) = extract' (is, ms, l : ls) xs
