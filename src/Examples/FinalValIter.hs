module Examples.FinalValIter where 

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
internalStates = [(1,5),(2,5),(3,2),(3,3),(3,4),(3,5)]
allStates      = [(x,y)| x <- [1..4], y <- [1..5]]   
gamePlayStates = allStates \\ internalStates
allowedActions = [UP,DOWN,LEFT,RIGHT]

rewMat :: RewMatrix
rewMat = fromLists ls 
    where
      nval = Decompose[0,0,0,0]
      rval = replicate 5 (Fin' nval)
      ls   = replicate 4 rval

getPos :: State -> Action -> State 
getPos p@(x,y) act = 
  case act of 
    UP    -> handleWall (x-1,y)  
    DOWN  -> handleWall (x+1,y) 
    LEFT  -> handleWall (x,y-1)
    RIGHT -> handleWall (x,y+1)
  where
    cond ns@(nx,ny) 
      = or [nx < 1,nx > 4,ny > 5,ny < 1]
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
      False -> g 0.9 p1 <.> g 0.05 p2 <.> g 0.05 p3 
  where 
    g w (x,y)  = let xyVal = getElem x y um
                 in xyVal * finDecomp [w,w,w,w]
    [p1,p2,p3] = getAllPos p a 

calcUtil :: State -> RewMatrix -> UtilMatrix -> DiscountFact -> NegInf Decomp
calcUtil p@(x,y) rm um gamma = 
    case elem p internalStates of
      True  ->  getElem x y um 
      False ->  rval <.>  (finDecomp [gamma,gamma,gamma,gamma] * svals)
  where
    rval    = getElem x y rm 
    svals   = sconcat allVals
    allVals = [calcRew p a um rm | a <- allowedActions]

valuIter :: UtilMatrix -> RewMatrix -> DiscountFact -> UtilMatrix 
valuIter um rm gamma = fromList 4 5 uls   
    where 
      uls = map (\s -> calcUtil s rm um gamma) allStates 

    
policyExtract :: UtilMatrix -> RewMatrix -> [(State,Action)] 
policyExtract um rm = justActions $ saveQVals ra   
  where
    g s = [(calcRew s a um rm,a,s) | a <- allowedActions]
    ra  = [g l  | l <- gamePlayStates]

policyExtractD :: UtilMatrix -> RewMatrix -> Result Decomp
policyExtractD um rm =  saveQVals ra   
  where
    g s = [(calcRew s a um rm,a,s) | a <- allowedActions]
    ra  = [g l  | l <- gamePlayStates]


type Result a = [(State,Action,[(Action,a)])]


justActions :: Result Decomp -> [(State,Action)] 
justActions = map (\(x,y,z) -> (x,y))

saveQVals :: [[(NegInf Decomp, Action, State)]] -> Result Decomp
saveQVals  = map formattoResult 
  
formattoResult :: [(NegInf Decomp,Action,State)] -> (State,Action,[(Action,Decomp)])
formattoResult ls = (hs,ha,ls3)  
  where
    ls1 = (reverse.sortBy (\(a,_,_) (b,_,_) -> compare a b)) ls 
    ls2 = map (\(Fin' x,y,z) -> (x,y,z)) ls1 
    ls3 = map (\(x,y,z) -> (y,x)) ls2 
    (hd,ha,hs) = head ls2  


runValueIter :: Int -> UtilMatrix -> RewMatrix -> DiscountFact -> [(State,Action)]
runValueIter 0 um rm _ = policyExtract um rm 
runValueIter n um rm gamma = runValueIter (n-1) um' rm gamma  
  where
    um' = valuIter um rm gamma

runValueIterD :: Int -> UtilMatrix -> RewMatrix -> DiscountFact -> Result Decomp
runValueIterD 0 um rm _ = policyExtractD um rm 
runValueIterD n um rm gamma = runValueIterD (n-1) um' rm gamma  
  where
    um' = valuIter um rm gamma


-- runValueIter 100  initUMat rewMat 0.9 

initUMat :: Matrix (NegInf (Decompose Double))
initUMat = fromLists [fstRow,sndRow,trdRow,fthRow]
  where
    allZs     = finDecomp [0,0,0,0]
    cliffGold = finDecomp [-10,0,0,10]
    justCliff = finDecomp [-10,0,0,0]
    succ1     = finDecomp [0,1,0,0] 
    succ15    = finDecomp [0,15,0,0]
    fail      = finDecomp [0,0,-20,0]
    fourZs    = replicate 4 allZs
    fstRow = fourZs ++ [succ1]
    sndRow = fourZs ++ [fail]
    trdRow = [allZs,justCliff,cliffGold,justCliff,succ15]
    fthRow = fourZs ++ [allZs]

-- [cliff,goal state +, end game - , gold]
-- *Examples.FinalValIter> runValueIter 100 initUMat rewMat 0.9
-- 