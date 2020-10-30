{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.RigidBody
  ( RigidBodyState (..),
    stepRigidBodyOde,
  )
where

import Linear hiding (cross)
import Math.Frames
import Math.Spatial
import Math.Vectorize (Vectorize (..))

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

-- TODO(matte): Move this to a physical constants type
gravity :: Double
gravity = 9.81

-- TODO(matte): Move these into an aircraft constants struct
mass :: Double
mass = 10

inertia :: V3T Body (V3T Body Double)
inertia =
  let ix = 1
      iy = 1
      iz = 1
      ixz = 0.0
   in V3T $
        V3
          (V3T (V3 ix 0 (- ixz)))
          (V3T (V3 0 iy 0))
          (V3T (V3 (- ixz) 0 iz))

-- | Inverse of inertia.
invertInertia :: Fractional a => V3T Body (V3T Body a) -> V3T Body (V3T Body a)
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

-- TODO(matte): Make the correct invertable and composable rotation transformations
-- so you don't have to do this by hand.
stepRigidBodyOde ::
  V3T Body Double ->
  V3T Body Double ->
  RigidBodyState Double ->
  RigidBodyState Double
stepRigidBodyOde
  forces
  moments
  RigidBodyState
    { ds_v_bn_b = v_bn_b,
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
