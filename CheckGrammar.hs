module CheckGrammar where

import AbsContextFree

import Data.Set

data Error = 
    NoSuchRule String
  | ManyStarts [String]
  | NoStart

check :: Grammar -> Maybe [Error]
check (Grammar stmts) = 
    case getStarts stmts of
      [] -> Left NoStart
      s@(_:_:_) -> Left (ManyStarts s)
      _ -> case concatMap (checkStmtCalls ruleNames) stmts of
	     [] -> Right Gr
	     s  -> Left (map NoSuchRule s)
  where ruleNames = getRuleNames stmts

getRuleNames :: [Stmt] -> Set String
getRuleNames = foldr union empty
  where getName (Rule name _) = singleton name
	getName (Start _)     = empty

checkStmtCalls :: Stmt -> Set String -> [String]
checkStmtCalls (Start _) _ = []
checkStmtCalls (Rule _ calls) names = concatMap (checkCall names) calls
checkCall :: Call -> Set String -> [String]
checkCall (Call name _) names | member name names = []
			      | otherwise         = [name]


getStarts :: [Stmt] -> [String]
getStarts = concatMap getStartName
  where getStartName (Rule _ _) = []
	getStartName (Start name) = [name]
