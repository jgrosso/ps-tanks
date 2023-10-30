module PsTanks.Data.Vector2 where

import Prelude

import Data.Lens (lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((+~), (*~))
import Data.Lens.Types (Lens')

import Lens (class HasX, class HasY, _x, _y)

import Math (cos, sin)

import PsTanks.Data.Angle (Radians(Radians))

newtype Vector2 =
  Vector2
  { x ∷ Number
  , y ∷ Number
  }

instance hasXVector2 ∷ HasX Vector2 Number where
  _x ∷ Lens' Vector2 Number
  _x =
    lens
      (\(Vector2 o) -> o.x)
      (\(Vector2 o) -> Vector2 <<< o { x = _ })

instance hasYVector2 ∷ HasY Vector2 Number where
  _y ∷ Lens' Vector2 Number
  _y =
    lens
      (\(Vector2 o) -> o.y)
      (\(Vector2 o) -> Vector2 <<< o { y = _ })

instance semiringVector2 ∷ Semiring Vector2 where
  add ∷ Vector2 → Vector2 → Vector2
  add a b =
    a#_x +~ b^._x
     #_y +~ b^._y

  zero ∷ Vector2
  zero =
    Vector2
    { x: 0.0
    , y: 0.0
    }

  mul ∷ Vector2 → Vector2 → Vector2
  mul a b =
    a#_x *~ b^._x
     #_y *~ b^._y

  one ∷ Vector2
  one =
    Vector2
    { x: 1.0
    , y: 1.0
    }

polarToCartesian ∷ Number → Radians → Vector2
polarToCartesian r (Radians φ) =
  Vector2
  { x: r * cos (-φ)
  , y: r * sin (-φ)
  }
