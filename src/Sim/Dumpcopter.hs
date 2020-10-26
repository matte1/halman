module Main where

import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Numeric.LinearAlgebra
import Numeric.GSL.ODE

dynamics :: Double -> [Double] -> [Double]
dynamics _ [_, x1, w] =
    [ x1
    , -mass*g + k*w + cd*x1**2
    , (torque - c * w) / j
    ]
  where
    -- timestep
    dt = 0.01
    -- Coefficient of drag
    cd = 0.1
    -- mass
    mass = 22
    -- gravity
    g = 9.8
    -- inertia
    j = 0.05
    -- Omega to thrust
    k = 5
    -- Coefficient of friction motor
    c = 0.1
    -- Calculate a physics based torque
    torque =
      clamp (-10, 10) $
        mass * g * j / (k * dt)
        - x1 / dt**2
        - w * j / dt
        + c * w
    -- Clamp between lower bound and upper bound
    clamp (lb, ub) x = max lb (min ub x)

dumbCopter :: IO ()
dumbCopter = toFile Plt.def "DumpCopter.svg" $ do
  let totalTime = 5
      dt = 0.01
      ts = (linspace (round $ totalTime / dt) (0, totalTime))
      sol = odeSolve dynamics [0, -10, 0] ts
      ([pos, vel, omega]) = toColumns sol
      ts' = toList ts
  Plt.plot (Plt.line "pos" [zip ts' (toList pos)])
  Plt.plot (Plt.line "vel" [zip ts' (toList vel)])
  Plt.plot (Plt.line "omega" [zip ts' (toList omega)])

main :: IO ()
main = dumbCopter
