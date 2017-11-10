{-# LANGUAGE RankNTypes #-}

module Kalman
    (moving_average
    , System(..)
    , Estimate(..)
    , Measurement(..)
    , genSigmas
    , ekf
    , ut
    , ukf
    , weightedCrossCov
    ) where

import Numeric.LinearAlgebra
import System.Random
import Sim
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Control.Applicative

type Sigmas = [[Double]]

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
    sigmas = [(toList x)] ++ pos ++ neg


-- Unscented Transformation
ut :: [Vector Double] -> [Double] -> [Double] -> Estimate
ut xs wm wc = Estimate x' p'
  where
    -- Weight each sigma by scaling factor
    xw = getZipList $ (scale) <$> ZipList wm <*> ZipList xs
    x' = foldl1 (\v1 v2 -> v1 + v2) xw

    -- Compute predicted (forecasted) state variable statistics
    v1 = map (\v -> fromColumns [v]) $ map (-x'+) xs
    v2 = map (\v -> fromRows [v])    $ map (-x'+) xs
    cov = getZipList $ (<>) <$> ZipList v1 <*> ZipList v2
    covw = getZipList $ (scale) <$> ZipList wc <*> ZipList cov
    p' = foldl1 (\m1 m2 -> m1 + m2) covw


-- Compute weighted covariance given two vectors
-- weightedCov :: Vector Double -> [Vector Double] -> Double -> Double -> Matrix Double
-- weightedCov v vs w1 w2 = cP where
--   cPo = scale w1 $ cov v (head vs)
--   cPi = map (scale w2) $ map (cov v) (tail vs)
--   cP  = foldl (\m1 m2 -> m1 + m2) cPo cPi
--
--   cov :: Vector Double -> Vector Double -> Matrix Double
--   cov v1 v2 = fromColumns ([v2 - v1]) <> fromRows ([v2 - v1])


weightedCrossCov :: Vector Double -> [Vector Double] -> Vector Double -> [Vector Double] -> [Double] -> Matrix Double
weightedCrossCov v1 vs1 v2 vs2 wc = cP where
  vs1' = map (\v -> fromColumns [v]) $ map (-v1+) vs1
  vs2' = map (\v -> fromRows [v])    $ map (-v2+) vs2
  cov  = getZipList $ (<>) <$> ZipList vs1' <*> ZipList vs2'
  covw = getZipList $ (scale) <$> ZipList wc <*> ZipList cov
  cP   = foldl1 (\m1 m2 -> m1 + m2) covw


-- ukf :: System -> Estimate -> Measurement -> (Double, Double)
-- ukf (System fA fH kQ kR) (Estimate x p) z = (wmo, wi)
-- ukf :: System -> Estimate -> Measurement -> Sigmas
-- ukf (System fA fH kQ kR) (Estimate x p) z = sigmas
-- ukf :: System -> Estimate -> Measurement -> [Vector Double]
-- ukf (System fA fH kQ kR) (Estimate x p) z = xs
-- ukf :: System -> Estimate -> Measurement -> Estimate
-- ukf (System fA fH kQ kR) (Estimate x p) z = (Estimate z' cZ)
-- ukf :: System -> Estimate -> Measurement -> Estimate
-- ukf (System fA fH kQ kR) (Estimate x p) z = (Estimate x' cX)
ukf :: System -> Estimate -> Measurement -> Estimate
ukf (System fA fH kQ kR) (Estimate x p) z = (Estimate xh ph)
  where
    -- Params
    -- n = fromIntegral $ size x
    -- wo = 0
    -- wi = (1-wo)/(2.0 * n)

    l = fromIntegral $ size x
    m = fromIntegral $ size z
    alpha = 1e-3
    ki = 0
    beta = 2

    lambda = alpha^2 * (l + ki) - l
    c = l + lambda

    wm = (lambda / c) : (replicate (2*(fromIntegral $ size x)) (0.5/c))
    wc = ((lambda / c) + (1-alpha^2+beta)) : (replicate (2*(fromIntegral $ size x)) (0.5/c))

    -- Generate sigma points around current estimate
    sigmas = genSigmas (Estimate x p) c

    -- Run sigmas through non linear function to produce new points
    xs = map (fromList) (map (fA) sigmas)
    zs = map (fromList) (map (fH) sigmas)

    Estimate x' cX = ut xs wm wc
    Estimate z' cZ = ut zs wm wc

    -- Calculate the weighted cross covariance of transformed forecast and
    -- measurements
    cXZ = weightedCrossCov x' xs z' zs wc

    kK = cXZ <> inv (cZ + kR)
    xh = x' + kK #> (z - z')
    ph = (cX + kQ) - kK <> (cZ + kR) <> tr kK
