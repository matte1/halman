module Main where

import Sim
import Kalman
import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Control.Applicative

fA1 :: (Floating a) => [a] -> [a]
fA1 [x,y,xh,yh,ax,ay] = [x + xh*dt + c*ax*dt*dt,
                         y + yh*dt + c*ay*dt*dt,
                         xh + ax*dt,
                         yh + ay*dt,
                         ax,
                         ay]
  where
    dt = 0.1
    c = 0.5

fH1 :: (Floating a) => [a] -> [a]
fH1 [x, y, xh, yh, ax, ay] = [x, y]

cannonBall = toFile Plt.def "CannonBall.svg" $ do
  -- KF Initial conditions
  let x0 = [0, 0, 10, 10, 0, -9.8]
  let q = diagl [0.1, 0.1, 1e-3, 1e-3, 1e-3, 1e-3]
  let r = diagl (replicate 2 1)
  let s0  = Estimate (vector x0) (diagl (replicate 6 5))

  -- Fire the cannon ball
  let plant = Plant fA1 id fH1 id
  let sim = take 5000 $ simulate plant (vector x0) 0
  let measurements = map (\(s, m) -> m) sim
  let trajectory = map(\(s,m) -> s) sim

  -- Run KF/EKF/UKF
  let sys = System fA1 fH1 q r
  let ekfs = scanl (\s z -> ekf sys s z) s0 measurements
  let ukfs = scanl (\s z -> ukf sys s z) s0 measurements

  -- Plot the results
  let vector2Chart v = map (\[x,y]->(x,y)) $ map toList $ map (subVector 0 2) $ v
  let state2Chart xs = vector2Chart $ map (sX) xs

  Plt.plot (Plt.line "Plant" [vector2Chart trajectory])
  Plt.plot (Plt.line "UKF" [state2Chart ukfs])
  Plt.plot (Plt.line "EKF" [state2Chart ekfs])
  -- Plt.plot (Plt.points "measurements" (vector2Chart measurements))

main = do
  -- KF Initial conditions
  let x0 = [0, 0, 10, 10, 0, -9.8]
  let q = diagl [0.1, 0.1, 1e-3, 1e-3, 1e-6, 1e-6]
  let r = diagl (replicate 2 1)
  let s0  = Estimate (vector x0) (diagl (replicate 6 5))

  -- Fire the cannon ball
  let plant = Plant fA1 id fH1 id
  let sim = take 5000 $ simulate plant (vector x0) 0
  let measurements = map (\(s, m) -> m) sim

  let sys = System fA1 fH1 q r
  let ekfs = scanl (\s z -> ekf sys s z) s0 measurements
  let ukfs = scanl (\s z -> ukf sys s z) s0 measurements

  print $ last measurements
  print $ last ekfs
  print $ last ukfs
  -- print $ fromLists (ukf sys s0 $ head measurements)
  -- print (ukf sys s0 $ head measurements)

  cannonBall
