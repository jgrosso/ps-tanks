module PsTanks.GameState where

import Prelude

import PsTanks.Angle (Degrees, deg)
import PsTanks.Lens (class HasPlayer, class HasPosition, class HasRotation)
import PsTanks.Vector2 (Vector2)

import Optic.Lens (lens)
import Optic.Types (Lens')

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
  { player ∷ PlayerState
  }

instance hasPlayerState ∷ HasPlayer State PlayerState where
  _player ∷ Lens' State PlayerState
  _player =
    lens
      (\(State o) -> o.player)
      (\(State o) -> State <<< o { player = _ })

initialState ∷ State
initialState =
  State
  { player:
    PlayerState
    { position: zero
    , rotation: 0.0 # deg
    }
  }
