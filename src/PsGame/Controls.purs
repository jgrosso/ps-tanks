module PsGame.Controls where

import Prelude

import Data.Array (catMaybes, cons)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))

import Lens (class HasAction, class HasControls, class HasCooldown, class HasEvent, class HasTrigger, _1, _2, _action, _controls, _cooldown, _elapsedTime, _event, _expectedTime, _trigger)

import Optic.Core ((..))
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Setter ((%~), (.~), (+~))
import Optic.Types (Lens')

import PsGame.Data.Cooldown (Cooldown)
import PsGame.Data.Milliseconds (Milliseconds)
import PsGame.InputsState (InputsState)
import PsGame.Utils ((∧), (≥))

newtype ControlsState gameEvent =
  ControlsState
  { controls ∷ Array (Control gameEvent)
  }

instance hasControlsControlsState ∷ HasControls (ControlsState gameEvent) (Array (Control gameEvent)) where
  _controls ∷ Lens' (ControlsState gameEvent) (Array (Control gameEvent))
  _controls =
    lens
      (\(ControlsState o) → o.controls)
      (\(ControlsState o) → ControlsState <<< o { controls = _ })

newtype Action gameEvent =
  Action
  { event ∷ gameEvent
  , trigger ∷ InputsState → Boolean
  }

instance hasEventAction ∷ HasEvent (Action gameEvent) gameEvent where
  _event ∷ Lens' (Action gameEvent) gameEvent
  _event =
    lens
      (\(Action o) → o.event)
      (\(Action o) → Action <<< o { event = _ })

instance hasTriggerAction ∷ HasTrigger (Action gameEvent) (InputsState → Boolean) where
  _trigger ∷ Lens' (Action gameEvent) (InputsState → Boolean)
  _trigger =
    lens
      (\(Action o) → o.trigger)
      (\(Action o) → Action <<< o { trigger = _ })

newtype Control gameEvent =
  Control
  { action ∷ Action gameEvent
  , cooldown ∷ Cooldown
  }

instance hasActionControl ∷ HasAction (Control gameEvent) (Action gameEvent) where
  _action ∷ Lens' (Control gameEvent) (Action gameEvent)
  _action =
    lens
      (\(Control o) → o.action)
      (\(Control o) → Control <<< o { action = _ })

instance hasCooldownControl ∷ HasCooldown (Control gameEvent) Cooldown where
  _cooldown ∷ Lens' (Control gameEvent) Cooldown
  _cooldown =
    lens
      (\(Control o) → o.cooldown)
      (\(Control o) → Control <<< o { cooldown = _ })

update ∷
  ∀ gameEvent
  . Milliseconds
  → InputsState
  → ControlsState gameEvent
  → Tuple (ControlsState gameEvent) (Array gameEvent)
update tickTime inputsState controlsState =
  let
    Tuple controls events =
      handleTriggers $ map tickCooldown (controlsState^._controls)
  in
    Tuple (controlsState#_controls .~ controls) events
  where
    tickCooldown ∷ Control gameEvent → Control gameEvent
    tickCooldown =
      _cooldown.._elapsedTime +~ tickTime

    resetCooldown ∷ Control gameEvent → Control gameEvent
    resetCooldown =
      _cooldown.._elapsedTime .~ zero

    handleTrigger ∷ Control gameEvent → Tuple (Control gameEvent) (Maybe gameEvent)
    handleTrigger control =
      let
        timerExpired ∷ Boolean
        timerExpired =
          (control^._cooldown.._elapsedTime) ≥ (control^._cooldown.._expectedTime)

        triggerFired ∷ Boolean
        triggerFired =
          control^._action.._trigger $ inputsState
      in
        if timerExpired ∧ triggerFired
        then Tuple (resetCooldown control) (Just $ control^._action.._event)
        else Tuple control Nothing

    handleTriggers ∷ Array (Control gameEvent) → Tuple (Array (Control gameEvent)) (Array gameEvent)
    handleTriggers controls =
      let
        Tuple controls' events =
          foldl
            (\acc control →
              let
                Tuple control' event = handleTrigger control
              in
                acc#_1 %~ cons control'
                  #_2 %~ cons event)
            mempty
            controls
      in
        Tuple controls' (catMaybes events)

makeInitialState ∷ ∀ gameEvent. Array (Control gameEvent) → ControlsState gameEvent
makeInitialState gameControls =
  ControlsState
  { controls: gameControls
  }
