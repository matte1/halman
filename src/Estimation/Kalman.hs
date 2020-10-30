{-# LANGUAGE RankNTypes #-}

module Kalman
  ( movingAverage,
    System (..),
    Estimate (..),
    Measurement (..),
    genSigmas,
    ekf,
    ut,
    ukf,
    weightedCrossCov,
    runKF,
  )
where

import Control.Applicative
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Numeric.LinearAlgebra
import Sim
import System.Random

type Sigmas = [[Double]]

data System = System
  { fA, fH :: forall a. Floating a => ([a] -> [a]),
    kQ, kR :: Matrix Double
  }

data Estimate = Estimate
  { sX :: Vector Double,
    sP :: Matrix Double
  }
  deriving (Show)

-- Simple 2 tap FIR to appx moving average filter
movingAverage :: [Vector Double] -> [Vector Double]
movingAverage = scanl1 (\v1 v2 -> (0.9 * v1) + (0.1 * v2))

-- kalman :: System -> Estimate -> Measurement -> Estimate
-- kalman (System a c q r) (Estimate x p) z = Estimate x' p' where
--   -- Convert lists to matrices since we have linear system
--   lA = fromLists a
--   lC = fromLists c
--   -- prediction step
--   px = lA #> x
--   pp = lA <> p <> tr lA + q
--   -- update step
--   g = pp <> tr lC <> inv (lC <> pp <> tr lC + r)
--   x' = px + g #> (z - lC #> px)
--   p' = (ident (size x) - g <> lC) <> pp

runKF :: (System -> Estimate -> Measurement -> Estimate) -> System -> Estimate -> [Measurement] -> [Estimate]
runKF kf sys = scanl (kf sys)

ekf :: System -> Estimate -> Measurement -> Estimate
ekf (System fA fH kQ kR) (Estimate x p) z = Estimate x' p'
  where
    -- prediction step
    a = fromLists (jacobian fA (toList x))
    px = fA $ toList x
    pp = a <> p <> tr a + kQ
    -- update step
    h = fromLists $ jacobian fH px
    k = pp <> tr h <> inv (h <> pp <> tr h + kR)
    x' = vector px + k #> (z - vector (take (size z) $ fH px))
    p' = (ident (size x) - k <> h) <> pp

genSigmas :: Estimate -> Double -> Sigmas
genSigmas (Estimate x p) c = sigmas
  where
    -- Cholesky Factorization to get sqrt of matrix
    cP = scale (sqrt c) $ chol $ sym p
    -- Add/sub the sqrt covariance matrix from the current estimate of x
    xs = fromLists $ replicate (size x) (toList x)
    pos = toLists $ xs + cP
    neg = toLists $ xs - cP
    -- Combine all sigma points including the original estimate of x at the head
    sigmas = [toList x] ++ pos ++ neg

-- Unscented Transformation
ut :: [Vector Double] -> [Double] -> [Double] -> Estimate
ut xs wm wc = Estimate x' p'
  where
    -- Weight each sigma by scaling factor
    xw = getZipList $ scale <$> ZipList wm <*> ZipList xs
    x' = sum xw
    -- Compute forecasted state statistics
    v1 = map (\v -> fromColumns [v - x']) xs
    v2 = map (\v -> fromRows [v - x']) xs
    cov = getZipList $ (<>) <$> ZipList v1 <*> ZipList v2
    covw = getZipList $ scale <$> ZipList wc <*> ZipList cov
    p' = sum covw

weightedCrossCov :: Vector Double -> [Vector Double] -> Vector Double -> [Vector Double] -> [Double] -> Matrix Double
weightedCrossCov v1 vs1 v2 vs2 wc = cP
  where
    vs1' = map (\v -> fromColumns [v - v1]) vs1
    vs2' = map (\v -> fromRows [v - v2]) vs2
    cov = getZipList $ (<>) <$> ZipList vs1' <*> ZipList vs2'
    covw = getZipList $ scale <$> ZipList wc <*> ZipList cov
    cP = sum covw

ukf :: System -> Estimate -> Measurement -> Estimate
ukf (System fA fH kQ kR) (Estimate x p) z = Estimate xh ph
  where
    l = fromIntegral $ size x
    m = fromIntegral $ size z
    alpha = 1e-3
    ki = 0
    beta = 2

    lambda = alpha ^ 2 * (l + ki) - l
    c = l + lambda

    wm = (lambda / c) : replicate (2 * fromIntegral (size x)) (0.5 / c)
    wc = ((lambda / c) + (1 - alpha ^ 2 + beta)) : replicate (2 * fromIntegral (size x)) (0.5 / c)

    -- Generate sigma points around current estimate
    sigmas = genSigmas (Estimate x p) c

    -- Run sigmas through non linear function to produce new points
    xs = map (fromList . fA) sigmas
    zs = map (fromList . fH) sigmas

    Estimate x' cX = ut xs wm wc
    Estimate z' cZ = ut zs wm wc

    -- Calculate the weighted cross covariance
    cXZ = weightedCrossCov x' xs z' zs wc

    kK = cXZ <> inv (cZ + kR)
    xh = x' + kK #> (z - z')
    ph = (cX + kQ) - kK <> (cZ + kR) <> tr kK
