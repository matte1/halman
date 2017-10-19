module Sim
    ( State(..)
    , Plant(..)
    , Measurement(..)
    , simulate
    ) where

import Numeric.LinearAlgebra

type Measurement = Vector Double
type State = Vector Double

data Plant = Plant {fA, fB :: (State -> State),
                    fC, fD :: (State -> Measurement)}

simulate :: Plant -> State -> Int -> [(State, Measurement)]
simulate (Plant fA fB fC fD) x n = (x', y) : simulate (Plant fA fB fC fD) x' (n+1) where
  x' = fA x
  y' = fC x'
  y  = y' + randomVector n Gaussian (size y')
