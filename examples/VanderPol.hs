module Main where

import Sim
import Kalman
import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Control.Applicative


vanderPol = toFile Plt.def "VanderPol.svg" $ do
  let x0 = [1.4, 0]
  let q = diagl (replicate 2 0.001)
  let r = diagl (replicate 2 0.5)
  let s0  = Estimate (vector [0, 5]) (diagl (replicate 2 5))

  let plant = Plant fA2 id fH2 id
  let sim = take 500 $ simulate plant (vector x0) 0
  let measurements = map (\(s, m) -> m) sim
  let trajectory = map(\(s,m) -> s) sim

  let sys = System fA2 fH2 q r
  let ekfs = scanl (\s z -> ekf sys s z) s0 measurements
  let ukfs = scanl (\s z -> ukf sys s z) s0 measurements

  let t1 = linspace 500 (0,50::Double)
  let pts  = oneList (toList t1) (map (\[x] -> x) $ map toList $ map (subVector 0 1) trajectory)
  let pts2 = oneList (toList t1) (map (\[x] -> x) $ map toList $ map (subVector 0 1) $ map (sX) ukfs)
  let pts3 = oneList (toList t1) (map (\[x] -> x) $ map toList $ map (subVector 0 1) $ map (sX) ekfs)
  let pts4 = oneList (toList t1) (map (\[x] -> x) $ map toList $ map (subVector 0 1) measurements)

  Plt.plot (Plt.line "Plant" [pts])
  Plt.plot (Plt.line "UKF" [pts2])
  Plt.plot (Plt.line "EKF" [pts3])
  Plt.plot (Plt.line "Measurements" [pts4])

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

main = do
  vanderPol
