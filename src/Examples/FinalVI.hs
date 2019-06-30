module Examples.FinalVI where 

import DPSolver.DP
import DPSolver.Solvers
import Data.List 
import Semiring.Semiring
import Semiring.MaxPlus
import Semiring.Derivation
import qualified Semiring.BestDerivations as V

import Data.Matrix 

data Action =  UP 
             | DOWN 
             | RIGHT    
             | LEFT  
             deriving(Eq,Ord,Show) 

type RewMatrix  = Matrix (NegInf Decomp)
type UtilMatrix = Matrix (NegInf Decomp)
type State      = (Int,Int)
type DiscountFact = Double  
type Decomp = Decompose Double  

finDecomp = Fin' . Decompose
internalStates = [(2,2),(1,4),(2,4)]
allStates      = [(x,y)| x <- [1..3], y <- [1..4]]   
gamePlayStates = allStates \\ internalStates
allowedActions = [UP,DOWN,LEFT,RIGHT]

rewMat :: RewMatrix
rewMat = fromLists ls 
    where
      nval = Decompose[0,-(0.04)]
      rval = replicate 4 (Fin' nval)
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



calcRew :: State -> Action -> UtilMatrix -> RewMatrix -> NegInf Decomp
calcRew p@(px,py) a um rm = 
    case elem p internalStates of 
      True  -> getElem px py rm   
      False -> g 0.8 p1 <.> g 0.1 p2 <.> g 0.1 p3 
  where 
    g w (x,y)  = let xyVal = getElem x y um
                 in xyVal * finDecomp [w,w]
    [p1,p2,p3] = getAllPos p a 

calcUtil :: State -> RewMatrix -> UtilMatrix -> DiscountFact -> NegInf Decomp
calcUtil p@(x,y) rm um gamma = 
    case elem p internalStates of
      True  ->  getElem x y um 
      False ->  rval <.>  (finDecomp [gamma,gamma] * svals)
  where
    rval    = getElem x y rm 
    svals   = sconcat allVals
    allVals = [calcRew p a um rm | a <- allowedActions]

valuIter :: UtilMatrix -> RewMatrix -> DiscountFact -> UtilMatrix 
valuIter um rm gamma = fromList 3 4 uls   
    where 
      uls = map (\s -> calcUtil s rm um gamma) allStates 

    
policyExtract :: UtilMatrix -> RewMatrix -> Result Decomp
policyExtract um rm = saveQVals ra   
  where
    g s = [(calcRew s a um rm,a,s) | a <- allowedActions]
    ra  = [g l  | l <- gamePlayStates]

type Result a = [(State,Action,[(Action,a)])]

saveQVals :: [[(NegInf Decomp, Action, State)]] -> Result Decomp
saveQVals  = map formattoResult 
  
formattoResult :: [(NegInf Decomp,Action,State)] -> (State,Action,[(Action,Decomp)])
formattoResult ls = (hs,ha,ls3)  
  where
    ls1 = (reverse.sortBy (\(a,_,_) (b,_,_) -> compare a b)) ls 
    ls2 = map (\(Fin' x,y,z) -> (x,y,z)) ls1 
    ls3 = map (\(x,y,z) -> (y,x)) ls2 
    (hd,ha,hs) = head ls2  


someFun 0 um rm _ = policyExtract um rm 
someFun n um rm gamma = someFun (n-1) um' rm gamma  
  where
    um' = valuIter um rm gamma

initUMat :: Matrix (NegInf (Decompose Double))
initUMat = fromLists [r1,r2,r3]
    where
      r1 = replicate 3 (finDecomp [0.0,0.0]) ++ [finDecomp [1.0,0.0]]    
      r2 = replicate 3 (finDecomp [0.0,0.0]) ++ [finDecomp [0.0,-(1.0)]]  
      r3 = replicate 4 (finDecomp [0.0,0.0])  

