module Examples.ValueIteration where 

import DPSolver.DP
import DPSolver.Solvers
import Text.Printf
import Semiring.Semiring
import Semiring.MaxPlus

import Data.Matrix 



data Action =  UP 
             | DOWN 
             | LEFT  
             | RIGHT  

type RewMatrix  = Matrix MaxPlus
type UtilMatrix = Matrix MaxPlus
type State = (Int,Int)
type DiscountFact = Double     


rewMat :: RewMatrix
rewMat = fromLists ls 
    where
      nval = -(0.04)
      rval = replicate 4 (MaxP nval)
      ls   = replicate 3 rval

getPos :: State -> Action -> State 
getPos p@(x,y) act = 
  case act of 
    UP    -> handleWall (x-1,y)  
    DOWN  -> handleWall (x+1,y) 
    LEFT  -> handleWall (x,y-1)
    RIGHT -> handleWall (x,y+1)
  where
    cond ns@(nx,ny) 
      = or [ns == (2,2),nx < 1,nx > 3,ny > 4,ny < 1]
    handleWall np = if cond np then p else np   

getAllPos :: State -> Action -> [State]
getAllPos p act =
    case act of
      UP    -> g [UP,LEFT,RIGHT]
      DOWN  -> g [DOWN,RIGHT,LEFT]
      LEFT  -> g [LEFT,DOWN,UP]
      RIGHT -> g [RIGHT,UP,DOWN]
  where
    g = map (getPos p)

calcRew :: State -> Action -> UtilMatrix -> MaxPlus 
calcRew p a um =  g p1 0.8 <.> g p2 0.1 <.> g p3 0.1 
  where 
    g (x,y) w = let 
                  MaxP v = getElem x y um
                in MaxP (w*v) 
    [p1,p2,p3]  = getAllPos p a 

calcUtil :: State -> RewMatrix -> UtilMatrix -> 
            DiscountFact -> MaxPlus 
calcUtil p@(x,y) rm um gamma = 
    case elem p ls of
      True  -> 
        getElem x y um 
      False ->
        rval <.> MaxP (gamma * bval)
  where
    ls = [(2,2),(1,4),(2,4)]  
    rval = getElem x y rm 
    MaxP bval = sconcat allVals
    allVals   = [calcRew p a um | a <- [UP,DOWN,LEFT,RIGHT]]



valuIter :: UtilMatrix -> RewMatrix -> DiscountFact -> UtilMatrix 
valuIter um rm gamma = fromList 3 4 uls   
    where 
      uls = map (\s -> calcUtil s rm um gamma) ls   
      ls  = [(x,y)| x <- [1..3], y <- [1..4]]  


someFun 0 um _ _ = um 
someFun n um rm gamma = someFun (n-1) um' rm gamma  
  where
    um' = valuIter um rm gamma

initUMat = fromLists [r1,r2,r3]
    where
      r1 = replicate 3 (MaxP 0.0) ++ [MaxP (1.0)]    
      r2 = replicate 3 (MaxP 0.0) ++ [MaxP (-1.0)]  
      r3 = replicate 4 (MaxP 0.0)  
