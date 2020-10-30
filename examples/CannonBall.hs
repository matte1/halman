module Main where

import Control.Applicative
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import qualified Graphics.Rendering.Chart.Easy as Plt
import Kalman
import Numeric.LinearAlgebra
import Sim

fA1 :: (Floating a) => [a] -> [a]
fA1 [x, y, xh, yh, ax, ay] =
  [ x + xh * dt + c * ax * dt * dt,
    y + yh * dt + c * ay * dt * dt,
    xh + ax * dt,
    yh + ay * dt,
    ax,
    ay
  ]
  where
    dt = 0.1
    c = 0.5

fH1 :: (Floating a) => [a] -> [a]
fH1 [x, y, xh, yh, ax, ay] = [x, y]

cannonBall = toFile Plt.def "CannonBall.svg" $ do
  -- KF Initial conditions
  let x0 = [0, 0, 10, 10, 0, -9.8]
  let q = diagl [0.1, 0.1, 1e-3, 1e-3, 1e-3, 1e-3]
  let r = diagl (replicate 2 0.5)
  let s0 = Estimate (vector x0) (diagl (replicate 6 5))

  -- Fire the cannon ball
  let plant = Plant fA1 id fH1 id
  let sim = take 50 $ simulate plant (vector x0) 0
  let measurements = map snd sim
  let trajectory = map fst sim

  -- Run KF/EKF/UKF
  let sys = System fA1 fH1 q r
  let ekfs = scanl (ekf sys) s0 measurements
  let ukfs = scanl (ukf sys) s0 measurements

  -- Plot the results
  let vector2Chart = map ((\[x, y] -> (x, y)) . toList . subVector 0 2)
  let state2Chart xs = vector2Chart $ map sX xs

  Plt.plot (Plt.line "Plant" [vector2Chart trajectory])
  Plt.plot (Plt.line "UKF" [state2Chart ukfs])
  Plt.plot (Plt.line "EKF" [state2Chart ekfs])
  Plt.plot (Plt.points "measurements" (vector2Chart measurements))

main = cannonBall
