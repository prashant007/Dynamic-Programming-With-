{-# LANGUAGE FlexibleInstances #-}
module Examples.ValueIteration2 where 

import DPSolver.DP
import DPSolver.Solvers
import Data.List 
import Semiring.Semiring
import Semiring.MaxPlus
import Semiring.Derivation
import qualified Semiring.BestDerivations as V

import Data.Matrix 

instance FeatureType(Double,Double) where
  funit = (0,0) 
  combine f (a,b) (x,y) = (f a x, f b y) 
  normalize (x,y) = x + y 
  decompose (x,y) = [("Gold",x),("Cliff",y)]


data Action =  UP 
             | DOWN 
             | RIGHT    
             | LEFT  
             deriving(Eq,Ord,Show) 

type RewMatrix  = Matrix (MaxPlusD(Double,Double))
type UtilMatrix = Matrix (MaxPlusD(Double,Double))
type State = (Int,Int)
type DiscountFact = Double    

type PairL = [(MaxPlusD(Double,Double),Action)] 

rewMat :: RewMatrix
rewMat = fromLists ls 
    where
      nval = (0,-(0.04))
      rval = replicate 4 (MaxPD nval)
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

calcRew :: State -> Action -> UtilMatrix -> MaxPlusD(Double,Double) 
calcRew p a um = g p1 0.8 <.> g p2 0.1 <.> g p3 0.1 
  where 
    g (x,y) w = let 
                  MaxPD (v1,v2) = getElem x y um
                in MaxPD (w*v1,w*v2) 
    [p1,p2,p3]= getAllPos p a 

calcUtil :: State -> RewMatrix -> UtilMatrix -> 
            DiscountFact -> MaxPlusD(Double,Double) 
calcUtil p@(x,y) rm um gamma = 
    case elem p ls of
      True  -> 
        getElem x y um 
      False ->
        rval <.> MaxPD(gamma*bv1,gamma*bv2)
  where
    ls = [(2,2),(1,4),(2,4)]  
    rval = getElem x y rm 
    MaxPD(bv1,bv2) = sconcat allVals
    allVals = [calcRew p a um | a <- [UP,DOWN,LEFT,RIGHT]]

valuIter :: UtilMatrix -> RewMatrix -> DiscountFact -> UtilMatrix 
valuIter um rm gamma = fromList 3 4 uls   
    where 
      uls = map (\s -> calcUtil s rm um gamma) ls   
      ls  = [(x,y)| x <- [1..3], y <- [1..4]]  

--policyExtract :: UtilMatrix -> Matrix PairL
policyExtract um = head ra' 
  where
    ls  = zip [1..3][1..4]
    as  = [UP,DOWN,LEFT,RIGHT]
    g s = [(calcRew s a um,a) | a <- as]
    ra  = [g l  | l <- ls]
    compare1 (MaxPD(a,b)) (MaxPD(c,d)) = flip compare (a+b) (c+d)
    sfun = sortBy (\(m1,_) (m2,_) -> compare1 m1 m2) 
    ra' = map (sfun) ra 



    -- getOptAct :: (State,Action) -> UtilMatrix -> 
    --              (MaxPlusD(Double,Double),Action)
    -- getOptAct (st,a) um = (r,a)
    --   where
    --     r = calcRew st a um 


someFun 0 um _ _ = policyExtract um 
someFun n um rm gamma = someFun (n-1) um' rm gamma  
  where
    um' = valuIter um rm gamma

initUMat :: Matrix (MaxPlusD (Double,Double))
initUMat = fromLists [r1,r2,r3]
    where
      r1 = replicate 3 (MaxPD (0.0,0.0)) ++ [MaxPD (1.0,0.0)]    
      r2 = replicate 3 (MaxPD (0.0,0.0)) ++ [MaxPD (0.0,-(1.0))]  
      r3 = replicate 4 (MaxPD (0.0,0.0))  


-- calcRew :: State -> Action -> UtilMatrix -> MaxPlusD(Double,Double)
-- calcRew p a um = g p1 0.8 <.> g p2 0.1 <.> g p3 0.1 
--   where 
--     g (x,y) w = let V.Max10Best ls = getElem x y um
--                     MaxPD(v1,v2)   = (fst.head) ls  
--                 in  (MaxPD(w*v1,w*v2)) 
--     [p1,p2,p3]= getAllPos p a 

-- calcUtil :: State -> RewMatrix -> UtilMatrix -> 
--             DiscountFact -> V.Max10Best SRing1 
-- calcUtil p@(x,y) rm um gamma = 
--     case elem p ls of
--       True  -> 
--         getElem x y um 
--       False ->
--         V.Max10Best [(rval,mkDerivation [])] <.> nval
--   where
--     ls = [(2,2),(1,4),(2,4)]  
--     rval = getElem x y rm 
--     nval = V.Max10Best [(MaxPD(gamma*bv1,gamma*bv2),der)]
--     (MaxPD(bv1,bv2),der) = sconcat allVals
--     allVals = [(calcRew p a um, mkDerivation [a]) 
--                | a <- [UP,DOWN,LEFT,RIGHT]]



-- valuIter :: UtilMatrix -> RewMatrix -> DiscountFact -> UtilMatrix 
-- valuIter um rm gamma = fromList 3 4 uls   
--     where 
--       uls = map (\s -> calcUtil s rm um gamma) ls   
--       ls  = [(x,y)| x <- [1..3], y <- [1..4]]  


-- someFun 0 um _ _ = um 
-- someFun n um rm gamma = someFun (n-1) um' rm gamma  
--   where
--     um' = valuIter um rm gamma

-- initUMat :: UtilMatrix 
-- initUMat = fromLists [r1,r2,r3]
--     where
--       ffun x = V.Max10Best [(MaxPD x,mkDerivation []) ] 
--       r1 = replicate 3 (ffun(0.00,0.00)) ++ [ffun(1.00,0.00)]    
--       r2 = replicate 3 (ffun(0.00,0.00)) ++ [ffun(0.00,-(1.0))]  
--       r3 = replicate 4 (ffun(0.00,0.00))  

-- Derivation [(Int,Int,Double)]