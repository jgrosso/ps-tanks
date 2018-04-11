module PsGame.Controls where

import Prelude

import Data.Array (filter)

import Lens (class HasAction, class HasTrigger, _action, _keysDown, _trigger)

import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens')

import PsGame.InputsState (InputsState, KeyCode)
import PsGame.Utils ((∈))

newtype Control gameEvent =
  Control
  { action ∷ gameEvent
  , trigger ∷ KeyCode
  }

instance hasActionControl ∷ HasAction (Control gameEvent) gameEvent where
  _action ∷ Lens' (Control gameEvent) gameEvent
  _action =
    lens
      (\(Control o) → o.action)
      (\(Control o) → Control <<< o { action = _ })

instance hasTriggerControl ∷ HasTrigger (Control gameEvent) KeyCode where
  _trigger ∷ Lens' (Control gameEvent) KeyCode
  _trigger =
    lens
      (\(Control o) → o.trigger)
      (\(Control o) → Control <<< o { trigger = _ })

deriveGameEvents ∷ ∀ gameEvent. Array (Control gameEvent) → InputsState → Array gameEvent
deriveGameEvents controls inputsState =
  let
    triggeredControls ∷ Array (Control gameEvent)
    triggeredControls =
      filter
        (\control →
          (control^._trigger) ∈ (inputsState^._keysDown))
        controls
  in
    map (_^._action) triggeredControls

