module Sim.Sim
  ( stepOde
  ) where

import Linear hiding ( cross )
import Math.Frames
import Math.Spatial
import Math.Vectorize ( Vectorize(..) )
import Models.Aircraft
import Numeric.GSL.ODE ( odeSolve )
import Numeric.LinearAlgebra ( Vector(..), toList, fromList, toRows )
import Physics.RigidBody


runOde ::
  V3T Body Double ->
  V3T Body Double ->
  AircraftState Double ->
  AircraftState Double
runOde forces moments state =
  AircraftState $ stepRigidBodyOde forces moments (dsRigidBody state)

-- | Steps the simulation forward by one timestep us RK45 ode solver.
--
-- This is pretty ugly because we have to convert back and forth from a list of
-- doubles in order to interact with GSL's ode library.
stepOde :: V3T Body Double -> V3T Body Double -> AircraftState Double -> AircraftState Double
stepOde forces moments state = devectorize $ toList solution
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
        forces
        moments
        (devectorize x0)
