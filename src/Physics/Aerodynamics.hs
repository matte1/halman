{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.RigidBody
  ( stepAerodynamics,
  )
where

import Linear hiding (cross)
import Math.Frames
import Math.Spatial
import Math.Vectorize (Vectorize (..))

stepAerodynamics ::
  () ->
  () ->
  () ->
  () ->
  (V3T Body Double, V3T Body Double)
stepAerodynamics = (forces, moments)
