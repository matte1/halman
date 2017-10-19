module Kalman
    ( moving_average
    , LinearSystem(..)
    , NonLinearSystem(..)
    , Estimate(..)
    , Measurement(..)
    , GaussianParams(..)
    , filter'
    ) where

import Numeric.LinearAlgebra
import System.Random
import Sim

data LinearSystem = LinearSystem {kA, kC, kQ, kR :: Matrix Double}

data NonLinearSystem = NonLinearSystem {jA :: (State->Matrix Double),
                          fA :: (State->State),
                          jH :: (State->Matrix Double),
                          fH :: (State->State),
                          q  :: (Matrix Double),
                          r  :: (Matrix Double)}

data Estimate = Estimate {sX :: Vector Double,
                          sP :: Matrix Double} deriving (Show)

data GaussianParams = GaussianParams {mu    :: Vector Double,
                                      sigma :: Herm Double}

-- Simple 2 tap FIR to appx moving average filter
moving_average :: [Vector Double] -> [Vector Double]
moving_average x = scanl1 (\v1 v2 -> (0.9*v1) + (0.1*v2)) x


kalman :: LinearSystem -> Estimate -> Measurement -> Estimate
kalman (LinearSystem a c q r) (Estimate x p) z = Estimate x' p' where
  -- prediction step
  px = a #> x
  pp = a <> p <> tr a + q
  -- update step
  g = pp <> tr c <> inv (c <> pp <> tr c + r)
  x' = px + g #> (z - c #> px)
  p' = (ident (size x) - g <> c) <> pp


ekf :: NonLinearSystem -> Estimate -> Measurement -> Estimate
ekf (NonLinearSystem jA fA jH fH q r) (Estimate x p) z = Estimate x' p' where
  -- prediction step
  a = jA x
  px = fA x
  pp = a <> p <> tr a + q
  -- update step
  h = jH x
  g = pp <> tr h <> inv (h <> pp <> tr h + r)
  x' = px + g #> (z - (fH x))
  p' = (ident (size x) - g <> h) <> pp


filter' :: [Measurement] -> NonLinearSystem -> Estimate -> [Estimate]
filter' zs sys s0 = scanl (\s z -> ekf sys s z) s0 zs
