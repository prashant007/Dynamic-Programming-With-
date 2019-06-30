module Examples.FinalShortestPath where

import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring
import Control.Monad.Identity

import Semiring.Derivation
import Semiring.BestDerivations
import Semiring.MinPlus
import Data.List 


type Node = Int
type Edge = (Node,Node)
type Graph l = [(Edge,l)] 
type DecompRew = Decompose Double

[a,b,c,d,e] = [1..5]

graph2 :: Graph Time_D 
graph2 = map g ls
  where
    g (x,y,z) = ((x,y),Decompose z)
    ls = [(a,b,[10,3,1,0]),(a,c,[14,9,2,1]),
          (b,d,[13,2,1,2]),(b,e,[6,0,2,1]),
          (c,d,[3,1,1,0]),
          (e,c,[3,0,0,0]),(e,d,[4,1,1,1])]

type Time   = Double
type Time_D = Decompose Time

type MinTime    = Large Time 
type MinTime_D  = Large Time_D

graph :: Graph Time 
graph = map g ls
  where
    g (x,y,z) = ((x,y),z)
    ls = [(a,b,14),(a,c,26),(b,d,18),(b,e,9),
          (c,d,5),(e,c,3),(e,d,7)]


graph1 :: Graph Time   
graph1 = map g ls 
  where
    g (x,y,z) = ((x,y),z)
    ls = [(1,2,75),(2,3,60),(3,4,104),(4,8,75),
          (1,5,70),(5,6,90),(6,7,94),(7,8,70)]



nodes :: Graph c -> [Node]
nodes = nub . concatMap (\((p,q),_) -> [p,q])

-- sp :: Graph Time -> Int -> DP (Int,Int) MinTime
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = f(v,i-1) <+> rem 
--   where
--     rem = sconcat[f(u,i-1) <.> (constant.semiCount) w|((u,v'),w) <- g, v' == v]

-- semiCount :: Time -> MinTime
-- semiCount = Finite   

-- shortestPath :: Graph Time -> Node -> Node -> MinTime
-- shortestPath g s t = runDP (t,length (nodes g)-1) (sp g s) 

runSthg = shortestPath graph 1 4 


-- -- ==================================================================


-- -- ==================================================================
-- type Time   = Double
-- type MinTime    = Large Time 
type Path = MultiDerivation [Edge]
type LenNPath = MinBest(Large Double,Path)

sp :: Graph Time -> Int -> DP (Int,Int) LenNPath
sp g s (v,0) = if s==v then one else zero 
sp g s (v,i) = memoize(v,i-1) <+> rem 
  where
    rem = sconcat[memoize(u,i-1) <.> (constant.semiLenNPath) e 
                 |e@((u,v'),w) <- g, v' == v]

semiLenNPath :: (Edge,Time) -> LenNPath
semiLenNPath (e,t) = MinBest(Finite t,MultiDerivation [[e]])

shortestPath :: Graph Time -> Node -> Node -> LenNPath
shortestPath g s t = runDP (t,length (nodes g)-1) (sp g s) 

-- -- ========================================================================
sp :: Graph Double -> Int -> DP (Int,Int) LenNPath
sp g s (v,0) = if s==v then one else zero 
sp g s (v,i) = memoize(v,i-1) <+> rem 
  where
    rem = sconcat[memoize(u,i-1) <.> (constant.semiLenNPath) e 
                 |e@((u,v'),w) <- g, v' == v]

semiLenNPath :: (Edge,Time) -> LenNPath
semiLenNPath (e,t) = MinBest(Finite t,MultiDerivation [[e]])

shortestPath :: Graph Time -> Node -> Node -> LenNPath
shortestPath g s t = runDP (t,length (nodes g)-1) (sp g s) 
-- ===========================================================================
type BestDers   = Min10Best (Large Double,Path)

-- sp :: Graph Double -> Int -> DP (Node,Int) BestDers  
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = f(v,i-1) <+> 
--                sconcat[memoize(u,i-1) <.> (constant.semiBestDers) e | e@((u,v'),w) <- g, v' == v] 

-- semiBestDers :: (Edge,Double) -> BestDers
-- semiBestDers (e,t) = Min10Best [(Finite t,mkDerivation [e])]

-- shortestPath :: Graph Double -> Node -> Node -> BestDers
-- shortestPath g s t = runDP (t,length (nodes g)-1) (sp g s) 

-- showAllPaths :: Show a => Min10Best (Large a,Path) -> String
-- showAllPaths (Min10Best m) = concatMap showPath m 

-- showPath :: Show a => (Large a,Path) -> String 
-- showPath (Finite cost,Derivation d) = (show d ++ " : " ++ show cost ++ " ")


-- runSthg = shortestPath graph1 1 8
-- -- ========================================================================
-- type Decomp     = Decompose Double 
-- type LenNPath_D = (Large Decomp,Path) 
-- type BestDers_D =  Min10Best LenNPath_D

-- sp :: Graph Decomp -> Node -> DP (Node,Int) BestDers_D  
-- sp g s (v,0) = if s==v then one else zero 
-- sp g s (v,i) = memoize(v,i-1) <+> 
--                sconcat[memoize(u,i-1) <.> (constant.semiBestDers) e | e@((u,v'),w) <- g, v' == v]

-- semiBestDers :: (Edge,Decomp) -> BestDers_D
-- semiBestDers (e,t) = Min10Best [(Finite t,mkDerivation [e])]

-- shortestPath :: Graph Decomp -> Node -> Node -> BestDers_D
-- shortestPath g s t = runDP (t,length (nodes g)-1) (sp g s) 


--runSthg = shortestPath graph 1 4
