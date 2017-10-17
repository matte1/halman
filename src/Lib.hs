module Lib
    ( traj
    , noise
    , measurement
    , moving_average
    , System(..)
    , State(..)
    , Measurement(..)
    , GaussianParams(..)
    , filter'
    , kalman
    ) where

import Numeric.LinearAlgebra
import System.Random

data System = System {kA, kC, kQ, kR :: Matrix Double}
data State = State {sX :: Vector Double , sP :: Matrix Double} deriving (Show)
type Measurement = Vector Double
data GaussianParams = GaussianParams {mu :: Vector Double, sigma :: Herm Double}

traj :: Numeric t => Vector t -> Matrix t -> [Vector t]
traj x0 state = scanl (\x0 s -> s #> x0) x0 $ repeat state

noise :: Vector Double -> Herm Double -> [Vector Double]
noise mu sigma = [flatten $ gaussianSample x 1 mu sigma | x <- [1..]]

measurement :: GaussianParams -> [Vector Double] -> [Vector Double]
measurement (GaussianParams mu sigma) trajectory  = zipWith (+) n t
  where
    n = noise mu sigma
    t = map (\[x,y]->vector [x,logBase 2 y]) $ map toList $ map (subVector 0 2) trajectory
-- 1/0.693x

moving_average :: [Vector Double] -> [Vector Double]
moving_average x = scanl1 (\v1 v2 -> (0.9*v1) + (0.1*v2)) x

kalman :: System -> State -> Measurement -> State
kalman (System a c q r) (State x p) z = State x' p' where
  -- prediction step
  px = a #> x
  pp = a <> p <> tr a + q
  -- update step
  g = pp <> tr c <> inv (c <> pp <> tr c + r)
  x' = px + g #> (z - c #> px)
  p' = (ident (size x) - g <> c) <> pp

ekf :: System -> State -> Measurement -> State
ekf (System a c q r) (State x p) z = State x' p' where

  -- prediction step
  px = a #> x
  pp = a <> p <> tr a + q
  -- update step
  h = fromLists [[1,0,0,0],[0,1.0/(0.693*((toList x) !! 1)),0,0]]
  hx = (\[x,y]->vector [x,(logBase 2 y)]) $ toList $ subVector 0 2 x

  g = pp <> tr h <> inv (h <> pp <> tr h + r)
  x' = px + g #> (z - hx)
  p' = (ident (size x) - g <> h) <> pp


filter' :: [Measurement] -> System -> State -> [State]
filter' zs sys s0 = scanl (\s z -> ekf sys s z) s0 zs
-- filter' zs sys s0 = scanl (\s z -> kalman sys s z) s0 zs
