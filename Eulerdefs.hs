module Eulerdefs
(rcCircuit,
 timeConstant,
 lrcCircuit
) where

import Control.Parallel (par, pseq)

-- Simulation of an RC circuit
-- The first line is a type signature
--   that says the function will take
--   four floating values and return
--   a list of floating values
-- This first definition is a dispatch
--   to set up the list for the logic
--   function, the return list is flipped
--   since it is less expensive to work
--   on the head of a list than the tail
rcCircuit :: Double -> Double -> Double -> Double -> [Double]
rcCircuit r c q0 dt = reverse $ rcCircuit' r c dt lowerBound [q0]
                      where
                          lowerBound = q0 / 200

-- Logic for RC circuit simulation
-- In Haskell looping is done using
--   recursion, here it is particularly
--   optimized being tail-end recursion
rcCircuit' :: Double -> Double -> Double -> Double -> [Double] -> [Double]
rcCircuit' r c dt lowerBound (q:qs)
    | q < 0          = qs
    | q < lowerBound = q:qs
    | otherwise = rcCircuit' r c dt lowerBound (qnew:q:qs)
      where
         qnew = q - ((q * dt) / (r * c))

-- Dispatch to set up for the LRC simulation
lrcCircuit :: Double -> Double -> Double -> Double -> Double -> Double -> [Double]
lrcCircuit r c l q0 i0 dt = reverse $ lrcCircuit' r c l dt maxlength [(q0, i0)]
                         where
                             maxlength = 150

-- Logic for LRC simulation, returns a list of voltages
-- This function shows off just how easy parallel 
--   computation is in Haskell. The method `par` executes
--   the left and right function in parallel and
--   `pseq` makes sure that everything to the left has
--   finished before executing the right. Par is optimized
--   so that it will only run in parallel when it knows that
--   the time savings are greater than the cost of forking.
lrcCircuit' :: Double -> Double -> Double -> Double -> Double-> [(Double, Double)] -> [Double]
lrcCircuit' r c l dt maxlength ((q, i):pairs)
    | maxlength == 0  = map voltage ((q, i):pairs)
    | otherwise = inew `par` qnew `pseq` (lrcCircuit' r c l dt (maxlength-1) ((qnew, inew):(q, i):pairs))
      where
          voltage (charge, _) = charge
          inew = i - ((q/(l*c)) + ((i*r)/l)) * dt
          qnew = q + i*dt


-- Calculates the time constant from
--   a list of values time dt apart
timeConstant :: Double -> [Double] -> Double
timeConstant dt vals = (*) dt $ fromIntegral $ length smallerVals
                       where
                           e = exp 1
                           tc = (head vals) / e
                           smallerVals = takeWhile (> tc) vals
