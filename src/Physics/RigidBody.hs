{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.RigidBody
  ( RigidBodyState (..),
    RigidBodyConstants (..),
    stepRigidBodyOde,
  )
where

import Linear hiding (cross)
import Math.Frames
import Math.Spatial
import Math.Vectorize (Vectorize (..))

import Physics.Constants ( gravity )

data RigidBodyState a = RigidBodyState
  { -- | vector from NED origin to body CG
    ds_r_n2b_n :: V3T Ned a,
    -- | CG velocity w.r.t. NED origin expressed in body frame
    ds_v_bn_b :: V3T Body a,
    -- | direction cosine matrix rotating from NED to body
    dsEulerN2B :: Euler Ned Body a,
    -- | angular velocity of body w.r.t. NED expressed in body frame
    ds_w_bn_b :: V3T Body a
  }
  deriving (Show)

instance Vectorize RigidBodyState a where
  vectorize (RigidBodyState pos vel euler pqr) =
    vectorize pos
      ++ vectorize vel
      ++ vectorize euler
      ++ vectorize pqr
  devectorize [x, y, z, u, v, w, roll, pitch, yaw, p, q, r] =
    RigidBodyState
      { ds_r_n2b_n = V3T (V3 x y z),
        ds_v_bn_b = V3T (V3 u v w),
        dsEulerN2B = Euler roll pitch yaw,
        ds_w_bn_b = V3T (V3 p q r)
      }

data RigidBodyConstants = RigidBodyConstants
  { cInertia :: V3T Body (V3T Body Double)
  , cMass :: Double
  }
  deriving (Show)

-- TODO(matte): Make the correct invertable and composable rotation transformations
-- so you don't have to do this by hand.
stepRigidBodyOde ::
  RigidBodyConstants ->
  V3T Body Double ->
  V3T Body Double ->
  RigidBodyState Double ->
  RigidBodyState Double
stepRigidBodyOde
  constants
  forces'
  moments
  RigidBodyState
    { ds_r_n2b_n = r_n2b_n,
      ds_v_bn_b = v_bn_b,
      ds_w_bn_b = w_bn_b,
      dsEulerN2B = eulerN2B
    } =
    RigidBodyState
      { ds_r_n2b_n = r_n2b_n',
        ds_v_bn_b = v_bn_b',
        dsEulerN2B = dsEulerN2B',
        ds_w_bn_b = w_bn_b'
      }
    where
      inertia :: V3T Body (V3T Body Double)
      inertia = cInertia constants
      mass :: Double
      mass = cMass constants
      dcmN2B :: Dcm Ned Body Double
      dcmN2B = dcmOfEuler321 eulerN2B
      dcmB2N :: Dcm Body Ned Double
      dcmB2N = transposeDcm dcmN2B
      -- Update position
      r_n2b_n' :: V3T Ned Double
      r_n2b_n' = rot dcmB2N v_bn_b
      -- Update velocities w/ forces
      v_bn_b' :: V3T Body Double
      v_bn_b' = w_bn_b `cross` v_bn_b + forces ^/ mass
        where
          forces = forces' ^+^ rot dcmN2B gravityWithGround
          -- TODO(matte): Add a more complicated ground model.
          gravityWithGround = groundContactModel mass r_n2b_n (rot dcmB2N v_bn_b)
      -- Update euler angles
      dsEulerN2B' :: Euler Ned Body Double
      dsEulerN2B' = Euler x y z
        where
          (V3T (V3 x y z)) = rot dcmB2N' w_bn_b
          (Euler roll pitch _) = eulerN2B
          sr = sin roll
          tp = tan pitch
          cr = cos roll
          cp = cos pitch
          dcmB2N' :: Dcm Body Ned Double
          dcmB2N' =
            DcmUnitVectors . V3T $
              V3
                (V3T (V3 1 (sr * tp) (cr * tp)))
                (V3T (V3 0 cr (- sr)))
                (V3T (V3 0 (sr / cp) (cr / cp)))
      -- Update body angular rates w/ moments
      w_bn_b' :: V3T Body Double
      w_bn_b' = inertiaInv !* (moments ^-^ w_bn_b `cross` (inertia !* w_bn_b))
        where
          inertiaInv :: V3T Body (V3T Body Double)
          inertiaInv = invertInertia inertia

-- | Modeled as a mass spring damper system
groundContactModel :: Double -> V3T Ned Double -> V3T Ned Double -> V3T Ned Double
groundContactModel mass (V3T (V3 _ _ z')) (V3T (V3 _ _ vz) )= V3T (V3 0 0 fz)
  where
    fz =
      if z' > 0
        then mass * gravity - springConstant * z  - dampeningCoefficient * vz
        else mass * gravity
    -- Subtract the maxPenitration so that the mass spring damper system evens out at 0
    z = z' - negate maxPenitration
    maxPenitration = 0.1
    springConstant = mass * gravity / maxPenitration
    dampeningCoefficient = 1000

-- | Inverse of inertia.
invertInertia ::
  Fractional a =>
  V3T Body (V3T Body a) ->
  V3T Body (V3T Body a)
invertInertia
  inertia@( V3T
              ( V3
                  (V3T (V3 a b c))
                  (V3T (V3 d e f))
                  (V3T (V3 g h i))
                )
            ) =
    (1 / det)
      *!! V3T
        ( V3
            (V3T (V3 a' b' c'))
            (V3T (V3 d' e' f'))
            (V3T (V3 g' h' i'))
        )
    where
      a' = cofactor (e, f, h, i)
      b' = cofactor (c, b, i, h)
      c' = cofactor (b, c, e, f)
      d' = cofactor (f, d, i, g)
      e' = cofactor (a, c, g, i)
      f' = cofactor (c, a, f, d)
      g' = cofactor (d, e, g, h)
      h' = cofactor (b, a, h, g)
      i' = cofactor (a, b, d, e)
      cofactor (q, r, s, t) = det22 (V2 (V2 q r) (V2 s t))
      det = det33 (V3 (V3 a b c) (V3 d e f) (V3 g h i))
