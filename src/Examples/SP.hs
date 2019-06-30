module Examples.SP where

import Data.Maybe (fromJust)
import Data.List (nub)

import Semiring.MinPlus


-- Large numbers
--
-- data Large a = Finite a | Infinity deriving (Eq,Ord)
type LDouble = Large Double

smallest :: (Ord a,Num a) => [Large a] -> Large a
smallest [] = Infinity
smallest xs = minimum xs


-- Graph Representation
--
type Node = Int
type Graph l = [((Node,Node),l)]

nodes :: Graph l -> [Node]
nodes = nub . concatMap (\((p,q),_) -> [p,q])

noNodes :: Graph l -> Int
noNodes = length . nodes

mapLabel :: (a -> b) -> Graph a -> Graph b
mapLabel f = map (\(e,l)->(e,f l))


-- Shortest Path Algorithm
--
type Cost = Large Double

spRec :: Graph Cost -> Node -> (Node,Int) -> Cost
spRec g s (v,0) = if s==v then Finite 0 else Infinity
spRec g s (v,i) = smallest (spRec g s (v,i-1):
                         [(spRec g s (u,i-1))+l | ((u,w),l) <- g, w==v])

spDist :: Graph Cost -> Node -> Node -> Cost
spDist g s t = spRec g s (t,noNodes g-1)

-- type Distances = [(Node,Node,LDouble)]
type Distances = [(Node,LDouble)]

from :: Graph Cost -> Node -> Distances
from g s = [(t,spDist g s t) | t <- nodes g]

to :: Graph Cost -> Node -> Distances
to g t = [(s,spDist g s t) | s <- nodes g]

track :: Graph Cost -> Node -> Node -> [Node]
track g s t | s==t = [t]
track g s t = s:track g w t
              where
                w:_ = [w | ((v,w),l) <- g, v==s, spDist g v t==spDist g w t+l]

sp :: Graph Cost -> Node -> Node -> ([Node],Cost)
sp g s t = (track g s t,spDist g s t)


-- Examples
--
[a,b,c,d,e] = [1..5]

g :: Graph [Double]
g = map (\(u,v,l)->((u,v),l)) es
    where
     es = [(a,b,[10,3,1,0]),(a,c,[14,9,2,1]),
           (b,d,[13,2,1,2]),(b,e,[6,0,2,1]),
           (c,d,[3,1,1,0]),
           (e,c,[3,0,0,0]),(e,d,[4,1,1,1])]

gc :: Graph Cost
gc = mapLabel (Finite . sum) g




forget xs i = let
                zs = replicate (length xs) 0
                (zs1,zs2) = splitAt (i-1) zs   
              in
                (zs1 ++ (xs !! (i-1)):tail zs2)


-- viewHelp :: Int -> ([Node],Cost) -> [([Node],Cost)] -> Graph Cost -> [([Node],Cost)]
-- viewHelp n p ps g = 


-- sumView :: [Node] -> Graph Double -> Double
-- sumView (n1:n2:ns) g = vn + sumView (n2:ns) g    
--   where
--     vn = (removeJust.lookup (n1,n2)) g
--     removeJust (Just x) = x

-- sumView _ g = 0

-- genGraph :: Int -> Graph [Double] -> Graph Double
-- genGraph n g = map (\(p,l) -> (p,l!!(n-1))) g

-- costForViewN :: [Node] -> Graph [Double] -> Int -> Double
-- costForViewN ns g n = sumView ns (genGraph n g)

-- costForAllViews :: [Node] -> Graph [Double] -> [Double]
-- costForAllViews ns g = map (costForViewN ns g) [1..noOfComps]
--   where
--     noOfComps = ((\(p,z) -> length z).head) g

-- -- this function takes two lists of costs. If the element in the first list is
-- -- smaller than the corresponsing element in the second list then its index is 
-- -- recorded.
-- compViewsH :: [Double] -> [Double] -> Int -> [Int]
-- compViewsH []     []     _ = []
-- compViewsH (x:xs) (y:ys) n
--   | x < y     = n:compViewsH xs ys (n+1)
--   | otherwise = compViewsH xs ys (n+1) 

-- -- first argument is the longer path 
-- compViews :: [Double] -> [Double] -> ([Double],[Int],Bool)
-- compViews xs ys = (ys,ls,c)
--   where
--     c  = ls /= [] 
--     ls = compViewsH xs ys 0 

-- compareWithPath :: [Double] -> Graph [Double] -> [Node] -> ([Node],([Double],[Int],Bool))
-- compareWithPath vwst g ns = (ns,compViews vwst vws) 
--   where
--     vws  = costForAllViews ns g

-- compAllPaths :: [Node] -> [[Node]] -> Graph [Double] -> [([Node],([Double],[Int],Bool))]
-- compAllPaths nst nss g = map (compareWithPath vwst g) nss 
--   where
--     vwst = costForAllViews nst g

-- handleAllNodes :: [] -> Graph [Double] -> Node -> Node ->
--                   [([Node],([Double],[Int],Bool))]
-- handleAllNodes ns g s t = filter (\(_,(_,_,d)) -> d) ls 
--   where
--     (spL,spN) = sp gc s t 
--     ps = map (\n -> costForAllViews n g) ns 
--     ls = map (\(x,y) -> (x,compViews y spath)) ps  

-- generateExpls :: ([Node],Cost) -> [([Node],Cost)] -> Graph Cost -> [([Node],Cost)]
-- generateExpls p (x:xs) g = 
--   where
--     