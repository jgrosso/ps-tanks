module PsTanks.Data.Coordinate where

import Prelude

import Lens (class HasX, class HasY, _x, _y)

import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Setter ((.~))
import Optic.Types (Lens')

import PsTanks.Data.Vector2 (Vector2)

newtype Coordinate =
  Coordinate Vector2

derive newtype instance semiringCoordinate ∷ Semiring Coordinate

instance hasXCoordinate ∷ HasX Coordinate Number where
  _x ∷ Lens' Coordinate Number
  _x =
    lens
      (\(Coordinate o) → o^._x)
      (\(Coordinate o) x → Coordinate $ o#_x .~ x)

instance hasYCoordinate ∷ HasY Coordinate Number where
  _y ∷ Lens' Coordinate Number
  _y =
    lens
      (\(Coordinate o) → o^._y)
      (\(Coordinate o) y → Coordinate $ o#_y .~ y)
