module TransformGrammar where

import qualified AbsContextFree as G
import qualified Drawing as D

import qualified Data.Map as Map
import Data.Array
import Data.List
import Data.Char
import System.Random

type Gr = Rule

-- This type should perhaps be a little more abstract. Like this maybe:
-- StdGen -> [Figure]
-- type Rule = Array Int [Figure]
type Rule = StdGen -> [Figure]

data Figure = Rule Rule D.Transform
	    | Circle D.Transform
	    | Square D.Transform

transformGrammar :: G.Grammar -> Gr
transformGrammar (G.Grammar grammar) 
    = Map.findWithDefault (error $ "Wrong start name \"" ++ start ++ 
			   "\" in transformGrammar")
                          start ruleNameMap
  where (G.Start (G.Ident start)
	 : sortedGrammar) = sortBy ruleName grammar
	partitionedGrammar = map mkRuleTpl $ groupBy eqRuleName sortedGrammar

	mkRuleTpl rules@(G.Rule (G.Ident name) w _ : _) 
	    = (map toLower name
	      ,map stripRuleConstructor rules)
	stripRuleConstructor (G.Rule _ w calls) = (w,calls)
	stripRuleConstructor _ 
	    = error "Odd constructor in TransformGrammar.stripRuleConstructor"
	-- Maps a rule name to the possible choices in the rule.
	-- It is recursively defined to that we get sharing in the grammar
	ruleNameMap = Map.fromList $
		      map mkRule $
		      map (translateCalls ruleNameMap) $
		      partitionedGrammar
	mkRule (name,choices) = (name
			        ,ruleFunction)
          where weights = map fst choices
                increasingWeights = scanl1 (+) weights
                choiceList = zip increasingWeights (map snd choices)
                totalWeight = sum weights
                ruleFunction rnd = go choiceList
                  where go ((w,c):cs) | r < w = c
                                      | otherwise = go cs
                        (r,_) = randomR (0,totalWeight) rnd

translateCalls nameMap (name,calls) 
  = (name, map (\ (w,c) -> (w,map (translateCall nameMap) c)) calls)
translateCall nameMap (G.Call (G.Ident ruleName) specs)
    = case map toLower ruleName of
       "circle" -> Circle trans
       "square" -> Square trans
       name -> Rule rule trans
  where rule = Map.findWithDefault (error $ "unknown rule name " ++ ruleName ++
				    " in TransformGrammar.translateCall")
	                           ruleName nameMap
	trans = specsToTransform specs

specsToTransform = foldr updateTransformField D.identityTransform

updateTransformField (G.Y y) trans = trans { D.dy = y }
updateTransformField (G.X x) trans = trans { D.dx = x }
updateTransformField (G.Size s) trans = trans { D.dsize = s }
updateTransformField (G.Rotate r) trans = trans { D.drotate = r }
updateTransformField (G.Bright b) trans = trans { D.dbright = b }

ruleName (G.Start _) _ = LT
ruleName _ (G.Start _) = GT
ruleName (G.Rule n1 _ _) (G.Rule n2 _ _) = compare n1 n2

eqRuleName (G.Rule n1 _ _) (G.Rule n2 _ _) = n1 == n2
eqRuleName _ _ = error "Not a rule in TransformGrammar.eqRuleName"
