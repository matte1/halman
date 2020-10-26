{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Aircraft
  ( AircraftState (..)
  ) where

import Math.Vectorize ( Vectorize(..) )
import Math.Frames
import Math.Spatial
import Physics.RigidBody ( RigidBodyState )

data AircraftState a =
  AircraftState
  { dsRigidBody :: RigidBodyState a
  } deriving Show

instance Vectorize AircraftState a where
  vectorize = vectorize . dsRigidBody
  devectorize = AircraftState . devectorize
