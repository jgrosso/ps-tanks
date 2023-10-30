module PsTanks.Data.Dimensions where

import Prelude

import Data.Lens (lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((.~))
import Data.Lens.Types (Lens')

import Lens (class HasX, class HasY, _x, _y)

import PsTanks.Data.Vector2 (Vector2)

newtype Dimensions =
  Dimensions Vector2

instance hasXDimensions ∷ HasX Dimensions Number where
  _x ∷ Lens' Dimensions Number
  _x =
    lens
      (\(Dimensions o) → o^._x)
      (\(Dimensions o) x → Dimensions $ o#_x .~ x)

instance hasYDimensions ∷ HasY Dimensions Number where
  _y ∷ Lens' Dimensions Number
  _y =
    lens
      (\(Dimensions o) → o^._y)
      (\(Dimensions o) y → Dimensions $ o#_y .~ y)
