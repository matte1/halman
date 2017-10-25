module Main where

-- import Lib
import Sim
import Kalman
import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

x0 = [10,10,1,10]
q = diagl (replicate 4 1e-6)
r = diagl (replicate 2 1)
s0  = Estimate (vector x0) (diagl (replicate 4 1e6))

stateUpdate :: (Floating a) => [a] -> [a]
stateUpdate [x,y,xh,yh] = [x + xh*dt, y + yh*dt, xh, yh]
  where
    dt = 1

measurementUpdate :: (Floating a) => [a] -> [a]
measurementUpdate [x,y,xh,yh] = [x, log y]

-- nonlinear = toFile Plt.def "nonlinear.svg" $ do
--   let plant = Plant stateUpdate id measurementUpdate id
--   let sim = take 500 $ simulate plant (vector x0) 0
--   let measurements = map (\(s, m) -> m) sim
--   let trajectory = map(\(s,m) -> s) sim
--
--   let sys = System stateUpdate measurementUpdate q r
--   let estimate = scanl (\s z -> ekf sys s z) s0 measurements
--
--   Plt.plot (Plt.line "Matrix Row" [vector2Chart trajectory])
--   Plt.plot (Plt.line "Matrix Row" [state2Chart estimate])
--   -- Plt.plot (Plt.points "Matrix Row" (  vector2Chart measurements))
--     where
--       vector2Chart v = map (\[x,y]->(x,y)) $ map toList $ map (subVector 0 2) $ v
--       state2Chart xs = vector2Chart $ map (sX) xs

-- main = do
--   let plant = Plant stateUpdate id measurementUpdate id
--   let sim = take 50 $ simulate plant (vector x0) 0
--   let measurements = map (\(s, m) -> m) sim
--   let trajectory = map(\(s,m) -> s) sim
--   let sys = System stateUpdate measurementUpdate q r
--   let estimate = scanl (\s z -> ekf sys s z) s0 measurements
--
--   nonlinear
--   print 1
main = do
  let sys = System stateUpdate measurementUpdate q r
  let estimate = Estimate (vector [0, 0, 1, 1]) (diagl [1,2,1,1])
  print $ ut sys estimate

-- plotSigmas = toFile Plt.def "sigmas.svg" $ do
--   let plant = Plant stateUpdate id measurementUpdate id
--   let sim = take 500 $ simulate plant (vector x0) 0
--   let measurements = map (\(s, m) -> m) sim
--   let trajectory = map(\(s,m) -> s) sim
--
--   let sys = System stateUpdate measurementUpdate q r
--   let estimate = scanl (\s z -> ekf sys s z) s0 measurements
--
--   let sigmas = genSigmas (last estimate) 2.0
--
--   Plt.plot (Plt.points "Matrix Row" (list2Chart sigmas))
--     where
--       list2Chart v = map (\[x,y,xh,yh]->(x,y)) v
--
-- main = do
--   plotSigmas
--   print 1
