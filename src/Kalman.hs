{-# LANGUAGE RankNTypes #-}

module Kalman
    (moving_average
    , System(..)
    , Estimate(..)
    , Measurement(..)
    -- , GaussianParams(..)
    , genSigmas
    , ekf
    , ut
    , ukf
    ) where

import Numeric.LinearAlgebra
import System.Random
import Sim
import Numeric.AD
import Numeric.AD.Internal.Reverse

data System = System {fA, fH :: (forall a. Floating a => ([a] -> [a])),
                      kC, kD :: Matrix Double}

data Estimate = Estimate {sX :: Vector Double,
                          sP :: Matrix Double} deriving (Show)

-- Simple 2 tap FIR to appx moving average filter
moving_average :: [Vector Double] -> [Vector Double]
moving_average x = scanl1 (\v1 v2 -> (0.9*v1) + (0.1*v2)) x


-- kalman :: System -> Estimate -> Measurement -> Estimate
-- kalman (System a c q r) (Estimate x p) z = Estimate x' p' where
--   -- prediction step
--   px = a #> x
--   pp = a <> p <> tr a + q
--   -- update step
--   g = pp <> tr c <> inv (c <> pp <> tr c + r)
--   x' = px + g #> (z - c #> px)
--   p' = (ident (size x) - g <> c) <> pp


ekf :: System -> Estimate -> Measurement -> Estimate
ekf (System fA fH kQ kR) (Estimate x p) z = Estimate x' p'
  where
    -- prediction step
    a = fromLists (jacobian fA (toList x))
    px = fA $ toList x
    pp = a <> p <> tr a + kQ
    -- update step
    h  = fromLists $ jacobian fH px
    g  = pp <> tr h <> inv (h <> pp <> tr h + kR)
    x' = vector px + g #> (z - vector (take (size z) $ fH px))
    p' = (ident (size x) - g <> h) <> pp


-- TODO: Pretty ugly method for converting back and forth between
-- matrices/vectors/list. A lot of this could be cleaner with more 'matlab'
-- esque functions.
-- TODO: Maybe return [Vector] instead of [[Double]]
-- TODO: Need to multiple p by the weight c
genSigmas :: Estimate -> Double -> [[Double]]
genSigmas (Estimate x p) c = sigmas
  where
    cP = scale c $ chol $ sym p         -- Cholesky Factorization to get sqrt of matrix
    pos = toLists $ (fromLists $ replicate (size x) (toList x)) + cP
    neg = toLists $ (fromLists $ replicate (size x) (toList x)) - cP
    sigmas = [(toList x)] ++ pos ++ neg


-- Unscented Transformation
-- 1. Generate new sigma points based on current covariance/mean
-- 2. Run those points through the nonlinear update function
-- 3. Calculate the new statistics of the state and measurement
ut :: System -> Estimate -> Estimate
ut (System fA fH _ _) (Estimate x p) = Estimate x' p'
  where
    n = fromIntegral $ size x
    alpha = 1e-3
    beta = 2.0
    lambda = (alpha**2 * n) - n
    wmo = lambda / (n + lambda)
    wco = wmo + (1.0 - alpha**2 + beta)
    wi = 1.0 / (2.0 * (n + lambda))

    sigmas = genSigmas (Estimate x p) (sqrt $ n+lambda)

    fX = map (fromList) $ map fA sigmas
    x' = foldl (\v1 v2 -> v1 + (scale wi v2)) (scale wmo $ head fX) (tail fX)

    fP = map (cov x') fX
    p' = foldl (\m1 m2 -> m1 + (scale wi m2)) (scale wco $ head fP) (tail fP)

    cov :: Vector Double -> Vector Double -> Matrix Double
    cov v1 v2 = m
      where
        dV = [v2 - v1]
        m = fromColumns dV <> fromRows dV


-- ukf :: System -> Estimate -> Measurement -> Estimate
ukf :: System -> Estimate -> Estimate
ukf sys@(System fA fH kQ kR) est@(Estimate x p) = e'
  where
    e' = ut sys est


-- filter' :: [Measurement] -> System -> Estimate -> [Estimate]
-- filter' zs sys s0 = scanl (\s z -> ekf sys s z) s0 zs
-- x' = foldl (\v1 v2 -> v1 + (scale wi v2)) (scale wmo $ (vector $ fA $ toList x)) fX
