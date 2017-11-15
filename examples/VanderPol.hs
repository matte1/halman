module Main where

import Sim
import Kalman
import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Control.Applicative


vanderPol = toFile Plt.def "VanderPol.svg" $ do
  let x0 = [1.4, 0]
  let q = diagl (replicate 2 0.0001)
  let r = diagl (replicate 2 0.5)
  let s0  = Estimate (vector [0, 5]) (diagl (replicate 2 5))

  let plant = Plant fA2 id fH2 id
  let sim = take 500 $ simulate plant (vector x0) 0
  let measurements = map snd sim
  let trajectory = map fst sim

  let sys = System fA2 fH2 q r
  let ekfs = scanl (ekf sys) s0 measurements
  let ukfs = scanl (ukf sys) s0 measurements

  let t1 = linspace 500 (0,50::Double)
  let toPoints v = oneList (toList t1) $ map ((\[x] -> x) . toList . subVector 1 1) v

  Plt.plot (Plt.line "Plant" [toPoints trajectory])
  Plt.plot (Plt.line "UKF" [toPoints $ map sX ukfs])
  Plt.plot (Plt.line "EKF" [toPoints $ map sX ekfs])
  Plt.plot (Plt.line "Measurements" [toPoints measurements])
  -- Plt.plot (Plt.line "LPF" [toPoints (movingAverage measurements)])

fA2 :: (Floating a) => [a] -> [a]
fA2 [x1,x2] = [x1 - x2*dt, x2 + (-u*(1-x1**2)*x2 + x1)*dt]
  where
    u = 0.2
    dt = 0.1

fH2 :: (Floating a) => [a] -> [a]
fH2 [x1, x2] = [x1, x2]

oneList :: [a] -> [b] -> [(a, b)]
oneList []     _      = []
oneList (x:xs) (y:ys) = (x, y) : oneList xs ys

main = vanderPol
