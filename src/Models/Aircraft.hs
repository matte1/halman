{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Aircraft
  ( AircraftState (..),
  )
where

import Math.Frames
import Math.Spatial
import Math.Vectorize (Vectorize (..))
import Physics.RigidBody (RigidBodyState)

newtype AircraftState a = AircraftState
  { dsRigidBody :: RigidBodyState a
  }
  deriving (Show)

instance Vectorize AircraftState a where
  vectorize = vectorize . dsRigidBody
  devectorize = AircraftState . devectorize
