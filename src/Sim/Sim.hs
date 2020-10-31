module Sim.Sim
  ( stepOde,
  )
where

import Linear hiding (cross)
import Math.Frames
import Math.Spatial
import Math.Vectorize (Vectorize (..))
import Models.Aircraft
import Numeric.GSL.ODE (odeSolve)
import Numeric.LinearAlgebra (Vector (..), fromList, toList, toRows)
import Physics.RigidBody

runOde ::
  RigidBodyConstants ->
  V3T Body Double ->
  V3T Body Double ->
  AircraftState Double ->
  AircraftState Double
runOde constants forces moments state =
  AircraftState $ stepRigidBodyOde constants forces moments (dsRigidBody state)

-- | Steps the simulation forward by one timestep us RK45 ode solver.
--
-- This is pretty ugly because we have to convert back and forth from a list of
-- doubles in order to interact with GSL's ode library.
stepOde ::
  RigidBodyConstants ->
  V3T Body Double ->
  V3T Body Double ->
  AircraftState Double ->
  AircraftState Double
stepOde constants forces moments state = devectorize $ toList solution
  where
    solution :: Vector Double
    (_ : solution : _) =
      toRows $
        odeSolve
          wrapOde
          (vectorize state)
          (fromList [0.0, 0.01])

    wrapOde :: Double -> [Double] -> [Double]
    wrapOde t0 x0 =
      vectorize $
        runOde
          constants
          forces
          moments
          (devectorize x0)
