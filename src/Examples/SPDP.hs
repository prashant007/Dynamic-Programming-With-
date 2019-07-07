{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Examples.SPDP where


-- Haskell imports
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Monad.Identity

-- DP imports
import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring

-- Local imports
import Semiring.Large
import Semiring.View

import Explanation.Decomposed
--import Explanation.Labeled
import Explanation.Dominators

import Data.List (sortBy)
import Data.Function (on)




-- Graph Representation
--
type Node = Int
type Edge = (Node,Node)
type Graph l = [(Edge,l)]

nodes :: Graph l -> [Node]
nodes = nub . concatMap (\((p,q),_) -> [p,q])

noNodes :: Graph l -> Int
noNodes = length . nodes


-- Shortest Path Algorithms
--
-- l : type of graph labels
-- r : type of result
--
class Semiring r => SP l r where
  result :: (Edge,l) -> r

  sp :: Graph l -> Node -> DP (Node,Int) r
  sp g s (v,0) = if s==v then one else zero
  sp g s (v,i) = memoize (v,i-1) <+>
                  sconcat [memoize (u,i-1) <.>
                    (constant.result) e | e@((u,v'),_) <- g, v' == v]

  shortestPath :: Graph l -> Node -> Node -> r
  shortestPath g s t = runDP (sp g s) (t,noNodes g-1)


-- Instances
--
-- (1) length, non-decomposed
-- (2) path, non-decomposed
-- (3) length, decomposed
-- (4) path, decomposed

-- Label and path types
type DOUBLE = Large Double
type Doubles = Decomposed Double
type DOUBLES = Large Doubles -- Large (Decomposed Double)
-- type Path l = (l,[Edge])
type Path l = View l [Edge]


-- (1) length, non-decomposed
--
instance SP Double (Large Double) where
  result (_,l) = Finite l

-- (2) path, non-decomposed
--
instance SP Double (Path (Large Double)) where
  result (e,l) = V (Finite l) [e]

-- (3) length, decomposed
--
-- instance SP [Double] DOUBLES where
instance SP [Double] (Large (Decomposed Double)) where
  result (_,l) = Finite (Values l)

-- THIS IS INCORECT!
-- Because it lets the semiring operations work on the component,
-- when they should work on the sums of the componets!
--
-- instance SP [Double] (Decomposed (Large Double)) where
--   result (_,l) = (Values (map Finite l))

-- (4) path, decomposed
--
-- instance SP [Double] (Path DOUBLES) where
instance SP [Double] (Path (Large (Decomposed Double))) where
  result (e,l) = V (Finite (Values l)) [e]

-- Examples
--
[a,b,c,d,e] = [1..5]

es :: [(Node,Node,[Double])]
es = [(a,b,[10,3,1,0]),(a,c,[14,9,2,1]),
      (b,d,[13,2,1,2]),(b,e,[6,0,2,1]),
      (c,d,[3,1,1,0]),
      (e,c,[3,0,0,0]),(e,d,[4,1,1,1])]

gd :: Graph [Double]
gd = map (\(u,v,l)->((u,v),l)) es

g :: Graph Double
g = map (\(u,v,l)->((u,v),sum l)) es

-- sp1 :: DOUBLE
sp1 = shortestPath g 1 4 :: Large Double

-- sp2 :: Path DOUBLE
sp2 = shortestPath g 1 4 :: Path (Large Double)

-- sp3 :: DOUBLES
sp3 = shortestPath gd 1 4 :: Large (Decomposed Double)
-- NOTE: This is incorrect, since lists are not semirings.
-- sp3 = shortestPath gd 1 4 :: Decomposed (Large Double)

-- sp4 :: Path DOUBLES
sp4 = shortestPath gd 1 4 :: Path (Large (Decomposed Double))

sps = (sp1,sp2,sp3,sp4)


-- Connetic the tropical semiring with the number type of decomposed values
--
instance Decompose (Large (Decomposed Double)) Double where
  dec Infinity = Values []
  dec (Finite vs) = vs
  -- lift = Finite
  supportive _ x = x<0


-- Auxiliary functions for computing path lengths
--
pathLength :: Num a => Graph a -> [Node] -> a
pathLength = pathAgg (+)

pathAgg :: (a -> a -> a) -> Graph a -> [Node] -> a
pathAgg f g vs@(v:_) = path' g vs
     where path' g [v,w]    = fromJust (lookup (v,w) g)
           path' g (v:w:vs) = fromJust (lookup (v,w) g) `f` path' g (w:vs)


-- mds for paths as computed, i.e., with semiring type
--
-- ps: shortest
-- pa: alternative
--
p = sp3
p' = Finite $ Values [17.0,10,3,1]
-- dsa = (shortestPath gd 1 4 :: Large (Decomposed Double)) - Finite (Values [17.0,10,3,1])
diff = p - p'
dom = mds p p'

-- mds for paths with labeled components
--
categories = ["Distance","Traffic","Weather","Construction"]

domX = explainWith categories p p'
-- explainWith ["Distance","Traffic","Weather","Construction"] p p'
