{-
****************************************************************************************************************
This code is primarily Sasha Rush's Pragmatic DP Haskell library (http://hackage.haskell.org/package/DP-0.1.1/).
The SolverInternal.hs file doesn't change anything from Sasha Rush's original file of the same name in the aforementioned 
library.
****************************************************************************************************************
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures #-}
module DPSolver.SolverInternal where 

import DPSolver.DP 
import qualified Data.Map as M 
import Safe 
import Control.Monad.Identity

newtype DPSolver  (monad :: * -> *) solver   (chart :: * -> * -> *) ind cell internal = DPSolver (solver monad chart ind cell internal)

type DPSolverSame m solver chart ind cell = DPSolver m solver chart ind cell cell

data SolveState = SolveState

class DPSolveBase s where 
    type Chart s :: * -> * -> *
    type DCell s 
    type Ind s  
    type Internal s 
    type DPMonad s :: * -> * 
    
instance (Monad m) => DPSolveBase (DPSolver m s ch ind cell int) where 
    type Chart (DPSolver m s ch ind cell int) = ch 
    type DCell (DPSolver m s ch ind cell int) = cell
    type Ind   (DPSolver m s ch ind cell int) = ind
    type Internal   (DPSolver m s ch ind cell int) = int
    type DPMonad   (DPSolver m s ch ind cell int) = m


class (DPSolveBase s, Monad (DPMonad s)) => SolveDP s where 
    type Frame s
    startSolver :: 
               (DCell s -> (DPMonad s) (DCell s))  -> 
               SolveFn s

data DPSolution chart ind cell internal = DPSolution {
      -- | The solution of the full dynamic program, i.e. the value at the last index computed 
      getResult :: cell,
      -- | The entire DP chart. The type is defined by the solver used.
      getChart :: chart ind internal
}

type DPSimpleSolution chart ind val = DPSolution chart ind (Identity val)

type SolveSimpleFn s b =      
    s -> Frame s -> DPGen (Ind s) b -> DPMonad s (DPSolution (Chart s) (Ind s) (DCell s) (Internal s))

type SolveFn s =      
    s -> Frame s -> DPGen (Ind s) (DCell s) -> DPMonad s (DPSolution (Chart s) (Ind s) (DCell s) (Internal s))