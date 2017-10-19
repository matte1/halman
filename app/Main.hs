module Main where

-- import Lib
import Sim
import Kalman
import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

dt = 0.01
a = fromLists
    [[1, 0, dt,  0],
     [0, 1,  0, dt],
     [0, 0,  1,  0],
     [0, 0,  0,  1]] :: Matrix Double
q' = diagl (replicate 4 0.000000000000001)
c = fromLists [[1,0,0,0],
               [0,1,0,0]]
r' = diagl (replicate 2 1)
g1 = GaussianParams (vector (replicate 2 0)) (sym $ r')
sys = LinearSystem a c q' r'
s0  = Estimate (vector [10,10,1,1]) (diagl (replicate 4 10000000))

newtUpdate :: State -> State
newtUpdate s = a #> s where
  a = fromLists
      [[1, 0, dt,  0],
       [0, 1,  0, dt],
       [0, 0,  1,  0],
       [0, 0,  0,  1]] :: Matrix Double

newtJacobian :: State -> Matrix Double
newtJacobian s = fromLists
    [[1, 0, dt,  0],
     [0, 1,  0, dt],
     [0, 0,  1,  0],
     [0, 0,  0,  1]] :: Matrix Double

-- logMeasure :: State -> Measurement
-- logMeasure s = (\[x,y] -> vector [x,logBase 2 y]) $ toList $ subVector 0 2 s
--
-- logJacobian :: State -> Matrix Double
-- logJacobian x = fromLists [[1,0,0,0],[0,1.0/(0.693*((toList x) !! 1)),0,0]]


nonlinear_update = toFile Plt.def "halman.svg" $ do
  let plant = Plant (a #>) id logMeasure id
  let sim = take 500 $ simulate plant (vector [10,10,1,1]) 0
  let measurements = map (\(s, m) -> m) sim
  let trajectory = map(\(s,m) -> s) sim

  let sys = NonLinearSystem (\x -> a) (a #>) logJacobian logMeasure q' r'
  let estimate = filter' measurements sys s0

  Plt.plot (Plt.line "Matrix Row" [vector2Chart trajectory])
  Plt.plot (Plt.line "Matrix Row" [state2Chart estimate])
  Plt.plot (Plt.points "Matrix Row" (vector2Chart measurements))
    where
      logJacobian x = fromLists [[1,0,0,0],[0,1.0/(0.693*((toList x) !! 1)),0,0]]
      logMeasure s = (\[x,y] -> vector [x,logBase 2 y]) $ toList $ subVector 0 2 s
      vector2Chart v = map (\[x,y]->(x,y)) $ map toList $ map (subVector 0 2) $ v
      state2Chart xs = vector2Chart $ map (sX) xs

main = do
  nonlinear_update
  print 1

-- main = do
--   let spaceship = take 5 $ traj (vector [10,10,1,2]) a
--   let measurements = measurement g1 spaceship
--   let estimate = filter' measurements sys s0
--   let mavg = moving_average measurements
--   print estimate

-- main = toFile Plt.def "halman.svg" $ do
--   let spaceship = take 1500 $ traj (vector [10,10,1,2]) a
--   let measurements = measurement g1 spaceship
--   let estimate = filter' measurements sys s0
--   let mavg = moving_average measurements
--
--   Plt.plot (Plt.line "Matrix Row" [vector2Chart spaceship])
--   Plt.plot (Plt.line "Matrix Row" [state2Chart estimate])
--   Plt.plot (Plt.line "Matrix Row" [vector2Chart mavg])
--   Plt.plot (Plt.points "Matrix Row" (vector2Chart measurements))
--     where
--       vector2Chart v = map (\[x,y]->(x,y)) $ map toList $ map (subVector 0 2) $ v
--       state2Chart xs = vector2Chart $ map (sX) xs
