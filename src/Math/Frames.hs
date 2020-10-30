module Math.Frames
  ( Ned,
    Vehicle,
    Vehicle1,
    Vehicle2,
    Body,
  )
where

-- | Inertial frame.
--
-- In cartesian coordinates north would be along the x axis, East would be along the Y axis, and
-- Down would be pointed down along the z axis.
data Ned

-- | Vehicle frame.
--
-- Located at the center of mass of the aircraft, but aligned with the NED axis.
data Vehicle

-- | Vehicle 1 frame.
--
-- Located at the center of mass of the aircraft, but rotated in the positive right-handed direction
-- about the z axis by the yaw angle.
data Vehicle1

-- | Vehicle 2 frame.
--
-- Located at the center of mass of the aircraft, but is obtained by rotating the Vehicle1 frame
-- in a right handed rotation about the y axis by the pitch angle theta.
data Vehicle2

-- | Body frame.
--
-- Located at the center of mass of the aircraft, but is obtained by rotating the Vehicle2 frame
-- in a right handed rotation about the x axis by the roll angle phi.
data Body
