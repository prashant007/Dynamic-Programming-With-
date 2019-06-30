module Examples.ShortestPath where

import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring
import Control.Monad.Identity

import Semiring.Derivation
import Semiring.BestDerivations
import Semiring.MinPlus
import Data.List 
import Semiring.AlternateDer

type Node = Int
type Edge = (Node,Node)
type Graph l = [(Edge,l)] 

[a,b,c,d,e] = [1..5]

graph2 :: Graph Decomp 
graph2 = map g ls
  where
    g (x,y,z) = ((x,y),Decompose z)
    ls = [(a,b,[10,3,1,0]),(a,c,[14,9,2,1]),
          (b,d,[13,2,1,2]),(b,e,[6,0,2,1]),
          (c,d,[3,1,1,0]),
          (e,c,[3,0,0,0]),(e,d,[4,1,1,1])]



graph :: Graph Double 
graph = map g ls
  where
    g (x,y,z) = ((x,y),z)
    ls = [(a,b,14),(a,c,26),(b,d,18),(b,e,9),
          (c,d,5),(e,c,3),(e,d,7)]


graph1 :: Graph Double   
graph1 = map g ls 
  where
    g (x,y,z) = ((x,y),z)
    ls = [(1,2,75),(2,3,60),(3,4,104),(4,8,75),
          (1,5,70),(5,6,90),(6,7,94),(7,8,70)]



nodes :: Graph c -> [Node]
nodes = nub . concatMap (\((p,q),_) -> [p,q])



-- -- ==================================================================
type LabeledPath = (Large Double,[Edge])

-- sp :: Graph Double -> Node -> DP (Node,Int) LabeledPath
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = memoize(v,i-1) <+> 
--                sconcat[memoize(u,i-1) <.> (constant.lpath) el | el@((u,v'),_) <- g, v' == v]

-- lpath :: (Edge,Double) -> LabeledPath
-- lpath (e,l) = (Finite l,[e])

-- shortestPath :: Graph Double -> Node -> Node -> LabeledPath
-- shortestPath g s t = runDP (sp g s) (t,length (nodes g)-1)

type Decomp = Decompose Double
type LabeledPath_D = (Large Decomp,[Edge])

sp :: Graph Decomp -> Node -> DP (Node,Int) LabeledPath_D 
sp g s (v,0) = if s==v then one else zero 
sp g s (v,i) = memoize(v,i-1) <+> 
                 sconcat[memoize(u,i-1) <.> (constant.lpath) el | el@((u,v'),_) <- g, v' == v]

lpath :: (Edge,Decomp) -> LabeledPath_D
lpath (e,l) = (Finite l,[e])

shortestPath :: Graph Decomp -> Node -> Node -> LabeledPath_D 
shortestPath g s t = runDP (sp g s) (t,length (nodes g)-1)



-- ===========================================================
type BestDers   = Min10Best LabeledPath

-- sp :: Graph Double -> Node -> DP (Node,Int) BestDers  
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = memoize(v,i-1) <+> 
--                sconcat[memoize(u,i-1) <.> (constant.semiBestDers) e | e@((u,v'),w) <- g, v' == v] 

-- semiBestDers :: (Edge,Double) -> BestDers
-- semiBestDers (e,l) = Min10Best [Surface (Finite l) [e]]

-- shortestPath :: Graph Double -> Node -> Node -> BestDers
-- shortestPath g s t = runDP (sp g s) (t,length (nodes g)-1)

-- =============================================================

-- type BestDers_D   = Min10Best LabeledPath_D

-- sp :: Graph Decomp -> Node -> DP (Node,Int) BestDers_D  
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = memoize(v,i-1) <+> 
--                sconcat[memoize(u,i-1) <.> (constant.semiBestDers) e | e@((u,v'),w) <- g, v' == v] 

-- semiBestDers :: (Edge,Decomp) -> BestDers_D
-- semiBestDers (e,l) = Min10Best [Surface (Finite l) [e]]

-- shortestPath :: Graph Decomp -> Node -> Node -> BestDers_D
-- shortestPath g s t = runDP (sp g s) (t,length (nodes g)-1)

-- =============================================================



-- sp :: Graph Double -> Node -> DP (Node,Int) LabeledPath
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = memoize(v,i-1) <+> 
--                  sconcat [memoize(u,i-1) <.> (constant.lpath) el | el@((u,v'),_) <- g, v' == v]

-- lpath :: (Edge,Double) -> LabeledPath
-- lpath (e,l) = Surface (Finite l) [e]

-- shortestPath :: Graph Double -> Node -> Node -> LabeledPath
-- shortestPath g s t = runDP (sp g s) (t,length (nodes g)-1)

--runSthg = shortestPath graph 1 4

runSthg = 2

-- index of the feature starting from 1. More natural
type FeatIndex = Int   


-- type Decomp = Decompose Double
-- type LabeledPath_D = (Large Decomp,[Edge])


-- This function takes a decomposed value, and an index of a primary feature in that decomposition as 
-- the inputs. The function returns a decomposed value such that every component except the primary feature, whose
-- index is provided in the input, is forgotten.   
forget :: (Num a) => Decompose a -> FeatIndex -> Decompose a
forget (Decompose xs) i = Decompose (zs1 ++ (xs !! (i-1)):tail zs2)
  where
    zs = replicate (length xs) 0
    (zs1,zs2) = splitAt (i-1) zs 

getAllGraphs :: Graph Decomp -> [FeatIndex] -> [Graph Decomp]
getAllGraphs gs is = map (getGraph gs) is 
    where
      getGraph :: Graph Decomp -> FeatIndex -> Graph Decomp
      getGraph gs i = map (\(e,g) -> (e,forget g i)) gs   


interestingCases :: Graph Decomp -> Node -> Node -> [FeatIndex] -> [(FeatIndex,[Edge])]
interestingCases g s t is = map (\(i,(pLen,p)) -> (i,p)) intSols 
  where
    intSols = filter (\(_,(pLen,p)) -> p /= bPath) allSols -- solutions of interest
    (bLen,bPath) = shortestPath g s t -- best solution 
    allSols = zipWith (\i g1 -> (i,shortestPath g1 s t)) is gs -- allSolutions 
    gs = getAllGraphs g is -- all generated graphs 

totalD :: (Num a) => Decompose a -> a 
totalD (Decompose d) = total d 

--total :: Num a => Decomposed a -> a
total :: Num a => [a] -> a 
total = foldr (+) 0
