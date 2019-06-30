module DPSolver.Solvers where 

import Control.Monad.Identity
import DPSolver.DP
import DPSolver.TopDown
import DPSolver.SolverAPI
import Semiring.Semiring

type DP a b = SimpleDP a b 

runDP :: (Semiring r, Ord ind) => DP ind r -> ind -> r
runDP x y = getSimpleResult $ runIdentity $ solveSimpleDP topDownMap y x


