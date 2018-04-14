module PsTanks.State where

import Prelude

import Lens (class HasBullets, class HasPlayer, class HasPosition, class HasRotation)

import PsTanks.Angle (Degrees, deg)
import PsTanks.Vector2 (Vector2)

import Optic.Lens (lens)
import Optic.Types (Lens')

newtype Bullet =
  Bullet
  { position ∷ Vector2
  , rotation ∷ Degrees
  }

instance hasPositionBullet ∷ HasPosition Bullet Vector2 where
  _position ∷ Lens' Bullet Vector2
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
  { position ∷ Vector2
  , rotation ∷ Degrees
  }

instance hasPositionPlayerState ∷ HasPosition PlayerState Vector2 where
  _position ∷ Lens' PlayerState Vector2
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
    { position: zero
    , rotation: 0.0 # deg
    }
  }
