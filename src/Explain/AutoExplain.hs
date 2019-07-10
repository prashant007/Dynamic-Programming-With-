module Explain.AutoExplain where


import Explain.Dominators
import Explain.Decomposed
import Explain.Principal
import Explain.Labeled


-- This DOSN'T WORK!
--
{-
Because whatever we do to the input, the resulting decomposed value
is incorrect:
- If we eliminate minor categories, they are lacking for the comparison
- If we set the minor categories to 0, the values in the result will be 0
and can't be used for computing the mds.
-}
-- explain :: (Decompose a b,Ord b,Num b) =>
--            (i -> a) -> [String] -> i -> (a,Decomposed (Labeled b))
-- explain f cs i = (o,explainWith cs o o')
--                    where o = f i
--                          o' = asDecomposed (f i)

explain :: (FromPrincipal f,Decompose (f (Decomposed a)) b,
            Eq (f (Decomposed a)),Ord b,Num b) =>
           (i -> f (Decomposed a),i -> f (Principal a)) -> [String] -> i ->
           (f (Decomposed a),Maybe (f (Decomposed a),Decomposed (Labeled b)))
explain (f,g) cs i | o==o'     = (o,Nothing)
                   | otherwise = (o,Just (o',explainWith cs o o'))
                   where (o,o') = (f i, fromPrincipal (g i))


{-
explain' :: (FromPrincipal f,Decompose (f (Decomposed a)) b,Ord b,Num b) =>
           (i -> f (Decomposed a),i -> f (Principal a)) -> [String] -> i ->
           (f (Decomposed a),f (Decomposed a),Decomposed (Labeled b))
explain' (f,g) cs i = (o,o',explainWith' cs o o')
                   where (o,o') = (f i, fromPrincipal (g i))


explainWith' :: (Decompose a b,Ord b,Num b) =>
               [String] -> a -> a -> Decomposed (Labeled b)
explainWith' cs d d' = Values $ head $ sortBy (compare `on` length) doms
    where (support,barrier) = partition sup $ values delta
          doms = [d | d <- sublists support, abs (sum d) > abs (sum barrier)]
          delta = dec d `withCategories` cs - dec d' `withCategories` cs
          sup = supportive d . unlabel
-}
