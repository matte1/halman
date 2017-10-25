{-# LANGUAGE RankNTypes #-}

module Sim
    ( State(..)
    , Plant(..)
    , Measurement(..)
    , simulate
    ) where

import Numeric.LinearAlgebra

type Measurement = Vector Double
type State = Vector Double

data Plant = Plant {fA, fC, fB, fD :: (forall a. Floating a => ([a] -> [a]))}

simulate :: Plant -> State -> Int -> [(State, Measurement)]
simulate (Plant fA fB fC fD) x n = (x', y') : simulate (Plant fA fB fC fD) x' (n+1)
  where
    x' = vector $ fA (toList x)
    y  = vector $ fC (toList x')
    y' = y + randomVector n Gaussian (size y)
