module PsTanks.State where

import Prelude

import Lens (class HasBullets, class HasImage, class HasPlayer, class HasPosition, class HasRotation)

import PsTanks.Data.Angle (Degrees, deg)
import PsTanks.Data.Coordinate (Coordinate)
import PsTanks.Data.Dimensions (Dimensions(Dimensions))
import PsTanks.Data.Url (Url(Url))
import PsTanks.Data.Vector2 (Vector2(Vector2))
import PsTanks.Image (Image(Image))

import Optic.Lens (lens)
import Optic.Types (Lens')

newtype Bullet =
  Bullet
  { image ∷ Image
  , position ∷ Coordinate
  , rotation ∷ Degrees
  }

instance hasImageBullet ∷ HasImage Bullet Image where
  _image ∷ Lens' Bullet Image
  _image =
    lens
      (\(Bullet o) → o.image)
      (\(Bullet o) → Bullet <<< o { image = _ })

instance hasPositionBullet ∷ HasPosition Bullet Coordinate where
  _position ∷ Lens' Bullet Coordinate
  _position =
    lens
      (\(Bullet o) → o.position)
      (\(Bullet o) → Bullet <<< o { position = _ })

instance hasRotationBullet ∷ HasRotation Bullet Degrees where
  _rotation ∷ Lens' Bullet Degrees
  _rotation =
    lens
      (\(Bullet o) → o.rotation)
      (\(Bullet o) → Bullet <<< o { rotation = _ })

newtype PlayerState =
  PlayerState
  { image ∷ Image
  , position ∷ Coordinate
  , rotation ∷ Degrees
  }

instance hasImagePlayerState ∷ HasImage PlayerState Image where
  _image ∷ Lens' PlayerState Image
  _image =
    lens
      (\(PlayerState o) → o.image)
      (\(PlayerState o) → PlayerState <<< o { image = _ })

instance hasPositionPlayerState ∷ HasPosition PlayerState Coordinate where
  _position ∷ Lens' PlayerState Coordinate
  _position =
    lens
      (\(PlayerState o) → o.position)
      (\(PlayerState o) → PlayerState <<< o { position = _ })

instance hasRotationPlayerState ∷ HasRotation PlayerState Degrees where
  _rotation ∷ Lens' PlayerState Degrees
  _rotation =
    lens
      (\(PlayerState o) → o.rotation)
      (\(PlayerState o) → PlayerState <<< o { rotation = _ })

newtype State =
  State
  { bullets ∷ Array Bullet
  , player ∷ PlayerState
  }

instance hasBulletsState ∷ HasBullets State (Array Bullet) where
  _bullets ∷ Lens' State (Array Bullet)
  _bullets =
    lens
      (\(State o) → o.bullets)
      (\(State o) → State <<< o { bullets = _ })

instance hasPlayerState ∷ HasPlayer State PlayerState where
  _player ∷ Lens' State PlayerState
  _player =
    lens
      (\(State o) -> o.player)
      (\(State o) -> State <<< o { player = _ })

initialState ∷ State
initialState =
  State
  { bullets: []
  , player:
    PlayerState
    { image:
      Image
      { dimensions:
        Dimensions $ Vector2
        { x: 100.0
        , y: 50.0
        }
      , sourceUrl: Url "./img/tank.png"
      }
    , position: zero
    , rotation: 0.0 # deg
    }
  }
