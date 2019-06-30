module ValueIterarion where 
import DP
import Solvers
import Data.Array.Unboxed
import Semiring
import Counting
import Data.Array.ST
import qualified Data.Map as M
import Control.Monad.ST
import Control.Monad
import Control.Monad.Identity
import Data.Int
import Safe
import Prob 
import qualified ViterbiNBest as V
import ViterbiNBestDerivation
import Derivation
import Viterbi
import Data.Matrix 


data Action =  UP 
             | DOWN 
             | LEFT  
             | RIGHT  

type RMatrix = Matrix MaxPlus
type UMatrix = Matrix MaxPlus
type MaxPlusDer m  = V.ViterbiNBest One (Weighted MaxPlus (Derivation m))


rewMat :: RMatrix
rewMat = fromLists ls 
    where
      nval = -(0.04)
      rval = replicate 4 (MaxP nval)
      ls   = replicate 3 rval

type State = (Int,Int)

getPos :: State -> Action -> State 
getPos p@(x,y) act = 
  case act of 
    UP    -> handleWall (x-1,y)  
    DOWN  -> handleWall (x+1,y) 
    LEFT  -> handleWall (x,y-1)
    RIGHT -> handleWall (x,y+1)
  where
    cond ns@(nx,ny) = or [ns == (2,2),nx < 1, nx > 3,ny > 4,ny < 1]
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

calcRew :: State -> Action -> UMatrix -> MaxPlus
calcRew p a um =  g p1 0.8 <.> g p2 0.1 <.> g p3 0.1 
  where
    g (x,y) w  = (MaxP w) * (getElem x y um) 
    [p1,p2,p3] = getAllPos p a 



instance Functor Maybe where
  fmap f (Finite x) = Finite (f x)
  fmap _ Infinity = Infinity

instance Applicative Maybe where
  pure = Finite
  Finite f <*> Finite x = Finite (f x)
  _        <*> _        = Infinity

instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger = Just . fromInteger



instance Show a => Show (Large a) where
  show m = 
    case m of
      Finite d  -> show d 
      Infinity -> "Inf"
   

-- calcUtil :: State -> RMatrix -> UMatrix -> DiscountFact -> MaxPlusDer  
-- calcUtil p@(x,y) rm um gamma = 
--     case elem p ls of
--       True  -> 
--         getElem x y um 
--       False ->
--         rval <.> MaxP (gamma * bval)
--   where
--     ls = [(2,2),(1,4),(2,4)]  
--     rval = getElem x y rm 
--     MaxP bval = sconcat [calcRew p a um | a <- [UP,DOWN,LEFT,RIGHT]]


-- type DiscountFact = Float    

-- valuIter :: UMatrix -> RMatrix -> DiscountFact -> UMatrix 
-- valuIter um rm gamma = fromList 3 4 uls   
--     where 
--       uls = map (\s -> calcUtil s rm um gamma) ls   
--       ls  = [(x,y)| x <- [1..3], y <- [1..4]]  


-- someFun 0 um _ _ = um 
-- someFun n um rm gamma = someFun (n-1) um' rm gamma  
--   where
--     um' = valuIter um rm gamma

-- initUMat = fromLists [r1,r2,r3]
--     where
--       r1 = replicate 3 (MaxP 0.0) ++ [MaxP (1.0)]    
--       r2 = replicate 3 (MaxP 0.0) ++ [MaxP (-1.0)]  
--       r3 = replicate 4 (MaxP 0.0)  
