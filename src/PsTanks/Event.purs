module PsTanks.Event where

import Prelude

import Data.Newtype (unwrap)

import Lens (_player, _position, _rotation)

import Math (cos, sin)

import Optic.Core ((..))
import Optic.Getter ((^.))
import Optic.Setter ((+~))

import PsTanks.Angle (Degrees, deg, degreesToRadians)
import PsTanks.State (State)
import PsTanks.Vector2 (Vector2(Vector2))

import Pux (EffModel, noEffects)

playerRotateSpeed ∷ Number
playerRotateSpeed = 3.0

playerMoveSpeed ∷ Number
playerMoveSpeed = 1.1

data RotateDirection
  = Clockwise
  | Counterclockwise

data TranslateDirection
  = Backward
  | Forward

data Event
  = MoveBullets
  | PlayerRotate RotateDirection
  | PlayerTranslate TranslateDirection

update ∷ ∀ e. Event → State → EffModel State Event e
update MoveBullets state =
  noEffects state
update (PlayerRotate rotateDirection) state =
  let
    sign ∷ Number
    sign =
      case rotateDirection of
        Clockwise → 1.0
        Counterclockwise → -1.0

    playerRotationΔ ∷ Degrees
    playerRotationΔ =
      sign * playerRotateSpeed # deg
  in
    noEffects $
      state#_player.._rotation +~ playerRotationΔ
update (PlayerTranslate translateDirection) state =
  let
    sign ∷ Number
    sign =
      case translateDirection of
        Backward → -1.0
        Forward → 1.0

    playerRotationRadians ∷ Number
    playerRotationRadians =
      unwrap $ degreesToRadians $ state^._player.._rotation

    playerPositionΔ ∷ Vector2
    playerPositionΔ =
      Vector2
      { x: playerMoveSpeed * sign * cos (-playerRotationRadians)
      , y: playerMoveSpeed * sign * sin (-playerRotationRadians)
      }
  in
    noEffects $
      state#_player.._position +~ playerPositionΔ
