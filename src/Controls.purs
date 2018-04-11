module PsTanks.Controls where

import Prelude

import Data.Array (filter)

import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens')

import PsTanks.GameEvent as GameEvent
import PsTanks.InputsState (InputsState, KeyCode)
import PsTanks.Lens (class HasAction, class HasTrigger, _action, _keysDown, _trigger)
import PsTanks.Utils ((∈))

newtype Control =
  Control
  { action ∷ GameEvent.Event
  , trigger ∷ KeyCode
  }

instance hasActionControl ∷ HasAction Control GameEvent.Event where
  _action ∷ Lens' Control GameEvent.Event
  _action =
    lens
      (\(Control o) → o.action)
      (\(Control o) → Control <<< o { action = _ })

instance hasTriggerControl ∷ HasTrigger Control KeyCode where
  _trigger ∷ Lens' Control KeyCode
  _trigger =
    lens
      (\(Control o) → o.trigger)
      (\(Control o) → Control <<< o { trigger = _ })

deriveGameEvents ∷ Array Control → InputsState → Array GameEvent.Event
deriveGameEvents controls inputsState =
  let
    triggeredControls ∷ Array Control
    triggeredControls =
      filter
        (\control →
          (control^._trigger) ∈ (inputsState^._keysDown))
        controls
  in
    map (_^._action) triggeredControls

