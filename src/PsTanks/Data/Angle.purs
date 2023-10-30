module PsTanks.Data.Angle where

import Prelude

import CSS.Size as CSS

import Data.Newtype (class Newtype, unwrap)

import Math as Math

π ∷ Number
π = Math.pi

class Angle a where
  toDegrees ∷ a → Degrees
  toRadians ∷ a → Radians

cos ∷ ∀ a. Angle a ⇒ a → Number
cos =
  toRadians >>> unwrap >>> Math.cos

sin ∷ ∀ a. Angle a ⇒ a → Number
sin =
  toRadians >>> unwrap >>> Math.sin

class ToCssAngle a b | a → b where
  toCssAngle ∷ a → CSS.Angle b

newtype Degrees =
  Degrees Number

derive newtype instance commutativeRingDegrees ∷ CommutativeRing Degrees
derive newtype instance euclideanRingDegrees ∷ EuclideanRing Degrees
derive newtype instance ringDegrees ∷ Ring Degrees
derive newtype instance semiringDegrees ∷ Semiring Degrees
derive instance newtypeDegrees ∷ Newtype Degrees _

instance angleDegrees ∷ Angle Degrees where
  toDegrees ∷ Degrees → Degrees
  toDegrees = id

  toRadians ∷ Degrees → Radians
  toRadians θ =
    unwrap θ * π / 180.0 # rad

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

instance angleRadians ∷ Angle Radians where
  toDegrees ∷ Radians → Degrees
  toDegrees θ =
    unwrap θ * 180.0 / π # deg

  toRadians ∷ Radians → Radians
  toRadians = id

instance toCssAngleRadians ∷ ToCssAngle Radians CSS.Rad where
  toCssAngle ∷ Radians → CSS.Angle CSS.Rad
  toCssAngle =
    unwrap >>> CSS.rad

rad ∷ Number → Radians
rad = Radians

