module PsTanks.Event where

import Prelude

import Data.Array (cons)

import Lens (_bullets, _player, _position, _rotation)

import Optic.Core ((..))
import Optic.Getter ((^.))
import Optic.Setter ((%~), (+~))

import PsTanks.Data.Angle (Degrees, deg, degreesToRadians)
import PsTanks.Data.Coordinate (Coordinate(Coordinate))
import PsTanks.Data.Dimensions (Dimensions(Dimensions))
import PsTanks.Data.Url (Url(Url))
import PsTanks.Data.Vector2 (Vector2(Vector2), polarToCartesian)
import PsTanks.Image (Image(Image), centerRight)
import PsTanks.State (Bullet(Bullet), State)
import Pux (EffModel, noEffects)

bulletMoveSpeed ∷ Number
bulletMoveSpeed = 0.5

playerRotateSpeed ∷ Number
playerRotateSpeed = 1.0

playerMoveSpeed ∷ Number
playerMoveSpeed = 1.1

data RotateDirection
  = Clockwise
  | Counterclockwise

data TranslateDirection
  = Backward
  | Forward

data Event
  = FireBullet
  | MoveBullets
  | PlayerRotate RotateDirection
  | PlayerTranslate TranslateDirection

update ∷ ∀ e. Event → State → EffModel State Event e
update FireBullet state =
  let
    newBullet ∷ Bullet
    newBullet =
      Bullet
      { image:
        Image
        { dimensions:
          Dimensions $ Vector2
          { x: 25.0
          , y: 10.0
          }
        , sourceUrl: Url "./img/bullet.png"
        }
      , position: centerRight $ state^._player
      , rotation: state^._player.._rotation
      }
  in
    noEffects $
      state#_bullets %~ cons newBullet
update MoveBullets state =
  noEffects $
    state#_bullets %~ map moveBullet
  where
    moveBullet ∷ Bullet → Bullet
    moveBullet bullet =
      let
        bulletPositionΔ :: Coordinate
        bulletPositionΔ =
          Coordinate $
            polarToCartesian bulletMoveSpeed (degreesToRadians $ bullet^._rotation)
      in
        bullet#_position +~ bulletPositionΔ
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

    playerPositionΔ ∷ Coordinate
    playerPositionΔ =
      Coordinate $
        polarToCartesian (sign * playerMoveSpeed) (degreesToRadians $ state^._player.._rotation)
  in
    noEffects $
      state#_player.._position +~ playerPositionΔ
