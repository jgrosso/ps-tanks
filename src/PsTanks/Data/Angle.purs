module PsTanks.Data.Angle where

import Prelude

import CSS.Size as CSS

import Data.Newtype (class Newtype, unwrap)

import Math (pi)

class ToCssAngle a b | a → b where
  toCssAngle ∷ a → CSS.Angle b

newtype Degrees =
  Degrees Number

derive newtype instance commutativeRingDegrees ∷ CommutativeRing Degrees
derive newtype instance euclideanRingDegrees ∷ EuclideanRing Degrees
derive newtype instance ringDegrees ∷ Ring Degrees
derive newtype instance semiringDegrees ∷ Semiring Degrees
derive instance newtypeDegrees ∷ Newtype Degrees _

instance toCssAngleDegrees ∷ ToCssAngle Degrees CSS.Deg where
  toCssAngle ∷ Degrees → CSS.Angle CSS.Deg
  toCssAngle =
    unwrap >>> CSS.deg

deg ∷ Number → Degrees
deg = Degrees

newtype Radians =
  Radians Number

derive newtype instance semiringRadians ∷ Semiring Radians
derive instance newtypeRadians ∷ Newtype Radians _

instance toCssAngleRadians ∷ ToCssAngle Radians CSS.Rad where
  toCssAngle ∷ Radians → CSS.Angle CSS.Rad
  toCssAngle =
    unwrap >>> CSS.rad

rad ∷ Number → Radians
rad = Radians

degreesToRadians ∷ Degrees → Radians
degreesToRadians (Degrees degrees) =
  degrees * pi / 180.0 # rad

radiansToDegrees ∷ Radians → Degrees
radiansToDegrees (Radians radians) =
  radians * 180.0 / pi # deg
