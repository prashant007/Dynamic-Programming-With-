
module Examples.Fibonacci where 

import DPSolver.DP
import DPSolver.Solvers

import Semiring.Semiring
import Semiring.Counting
import Control.Monad.Identity


sfib 0 = 0
sfib 1 = 1
sfib i = sfib (i-1) + sfib (i-2)

type Counting = Integer 

fib :: DP Int Counting 
fib 1 = zero
fib 2 = one
fib i = memoize(i-2) <+> memoize(i-1) <+> one 


runFib :: Int -> Counting  
runFib i = runDP fib i





