module Main where

import Lib
import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

dt = 0.02

a = fromLists
    [[1, 0, dt,  0],
     [0, 1,  0, dt],
     [0, 0,  1,  0],
     [0, 0,  0,  1]] :: Matrix Double

a' = fromLists
    [[1, 0, dt, 0, 0.5*dt*dt, 0],
     [0, 1, 0, dt, 0, 0.5*dt*dt],
     [0, 0, 1, 0, dt, 0],
     [0, 0, 0, 1, 0, dt],
     [0, 0, 0, 0, 1, 0],
     [0, 0, 0, 0, 0, 1]] :: Matrix Double

q = diagl (replicate 4 0.00000000000001)
-- c = diagl (replicate 4 1)
c = fromLists [[1,0,0,0],
               [0,1,0,0]]

r = diagl (replicate 2 1)

-- Noise generation
g1 = GaussianParams (vector (replicate 2 0)) (sym $ r)
sys = System a c q r
s0  = State (vector [10,10,1,1]) (diagl (replicate 4 1000))

-- main = do
--   let spaceship = take 5 $ traj (vector [10,10,1,2]) a
--   let measurements = measurement g1 spaceship
--   let estimate = filter' measurements sys s0
--   let mavg = moving_average measurements
--   print estimate

main = toFile Plt.def "halman.svg" $ do
  let spaceship = take 15000 $ traj (vector [10,10,1,2]) a
  let measurements = measurement g1 spaceship
  let estimate = filter' measurements sys s0
  let mavg = moving_average measurements

  Plt.plot (Plt.line "Matrix Row" [vector2Chart spaceship])
  Plt.plot (Plt.line "Matrix Row" [state2Chart estimate])
  Plt.plot (Plt.line "Matrix Row" [vector2Chart mavg])
  -- Plt.plot (Plt.points "Matrix Row" (vector2Chart measurements))
    where
      vector2Chart v = map (\[x,y]->(x,y)) $ map toList $ map (subVector 0 2) $ v
      state2Chart xs = vector2Chart $ map (sX) xs

--
-- x0 = vector [30, 100, 10, -50, 0, -9.8]

-- main = do
--   let t = take 10 $ traj x0 a
--   let n = take 10 $ noise' (vector [0,0]) (sym $ diagl [3,3])
--   let m = take 10  $ measurement (vector [0,0]) (sym $ diagl [3,3]) x0 a
--   let avg = moving_average m
--   let avg = map toList $ moving_average m
--   print avg

-- main = toFile Plt.def "halman.svg" $ do
--   let t = take 1000 $ traj x0 a
--   let m = take 1000 $ measurement (vector [0,0]) (sym $ diagl [50,50]) x0 a
--   let traj = toChart $ map (subVector 0 2) $ t
--   let avg = toChart $ moving_average m
--   let m1 = toChart m
--
--   Plt.plot (Plt.line "Matrix Row" [traj])
--   Plt.plot (Plt.line "Matrix Row" [avg])
--   Plt.plot (Plt.points "Matrix Row" m1)
--     where
--       toChart v = map (\[x,y]->(x,y)) $ map toList $ v
