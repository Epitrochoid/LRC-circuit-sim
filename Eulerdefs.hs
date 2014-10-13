module Eulerdefs
(rcCircuit,
 timeConstant
) where

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

-- Calculates the time constant from
--   a list of values time dt apart
timeConstant :: Double -> [Double] -> Double
timeConstant dt vals = (*) dt $ fromIntegral $ length smallerVals
                       where
                           e = exp 1
                           tc = (head vals) / e
                           smallerVals = takeWhile (> tc) vals
