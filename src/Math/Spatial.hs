{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.Spatial
  ( Dcm (..),
    Euler (..),
    V3 (..),
    V3T (..),
    cross,
    composeDcm,
    dcmOfEuler321,
    transposeDcm,
    rot,
  ) where

import Math.Frames ( Body, Vehicle, Vehicle1, Vehicle2 )
import Math.Vectorize ( Vectorize(..) )
import qualified Linear as L
import Linear ( Additive(..), V3(..), M33, (!*), (!*!) )
import Data.Distributive (Distributive (..))

instance Vectorize V3 a where
  vectorize (V3 a b c) = [a, b, c]
  devectorize (a : b : c : []) = V3 a b c
  devectorize _ = error "devectorize: Wrong number of elements"

-- | V3 with a phantom type representing frame.
newtype V3T frame a = V3T { unV3T :: V3 a }
  deriving (Show, Functor, Foldable, Fractional, Num, Applicative, Additive)

instance Vectorize (V3T frame) a where
  vectorize = vectorize . unV3T
  devectorize = V3T . devectorize

instance Distributive (V3T f) where
  distribute f =
    V3T $
      V3
        (fmap (\(V3T (V3 x _ _)) -> x) f)
        (fmap (\(V3T (V3 _ y _)) -> y) f)
        (fmap (\(V3T (V3 _ _ z)) -> z) f)

data Euler frame1 frame2 a = Euler
  { eulerRoll :: a
  , eulerPitch :: a
  , eulerYaw :: a
  }
  deriving (Show, Functor)

instance Vectorize (Euler frame1 frame2) a where
  vectorize (Euler roll pitch yaw) = [roll, pitch, yaw]
  devectorize ([roll, pitch, yaw]) = Euler roll pitch yaw

newtype Dcm f1 f2 a = DcmUnitVectors
  { getUnitVectors :: V3T f2 (V3T f1 a)
  }
  deriving (Show, Functor)

transposeDcm :: Dcm f1 f2 a -> Dcm f2 f1 a
transposeDcm = DcmUnitVectors . L.transpose . getUnitVectors

composeDcm :: Num a => Dcm f1 f2 a -> Dcm f2 f3 a -> Dcm f1 f3 a
composeDcm (DcmUnitVectors dcm_a2b) (DcmUnitVectors dcm_b2c) = DcmUnitVectors (dcm_b2c !*! dcm_a2b)

cross :: Num a => V3T f a -> V3T f a -> V3T f a
cross (V3T v1) (V3T v2) = V3T $ v1 `L.cross` v2

rot :: Num a => Dcm frame1 frame2 a -> V3T frame1 a -> V3T frame2 a
rot (DcmUnitVectors dcm) vector = dcm !* vector

-- | Creates a Dcm that transforms frame1 into frame2.
dcmOfEuler321 :: Floating a => Euler frame1 frame2 a -> Dcm frame1 frame2 a
dcmOfEuler321 (Euler roll pitch yaw) = DcmUnitVectors dcm
  where
    cPs = cos yaw
    sPs = sin yaw
    cTh = cos pitch
    sTh = sin pitch
    cPh = cos roll
    sPh = sin roll
    dcm =
      V3T $
        V3
          (V3T (V3 (cTh * cPs) (cTh * sPs) (- sTh)))
          (V3T (V3 (cPs * sTh * sPh - cPh * sPs) (cPh * cPs + sTh * sPh * sPs) (cTh * sPh)))
          (V3T (V3 (cPh * cPs * sTh + sPh * sPs) (- cPs * sPh + cPh * sTh * sPs) (cTh * cPh)))

-- | Rotates the 'Vehicle' frame about z axis to get the 'Vehicle1' frame.
dcmOfEulerYaw :: Floating a => a -> Dcm Vehicle Vehicle1 a
dcmOfEulerYaw yaw = dcmOfEuler321 (Euler 0 0 yaw)

-- | Rotates the 'Vehicle1' frame about x axis to get the 'Vehicle2' frame.
dcmOfEulerRoll :: Floating a => a -> Dcm Vehicle1 Vehicle2 a
dcmOfEulerRoll roll = dcmOfEuler321 (Euler 0 roll 0)

-- | Rotates the 'Vehicle2' frame about y axis to get the 'Body' frame.
dcmOfEulerPitch :: Floating a => a -> Dcm Vehicle2 Body a
dcmOfEulerPitch pitch = dcmOfEuler321 (Euler pitch 0 0)
