module PsGame where

import Prelude

import Effect (Effect)

import Data.Foldable (foldl)
import Data.Lens (lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((.~), (%~))
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple))

import Lens (class HasControlsState, class HasGameState, class HasInputsState, _controlsState, _gameState, _inputsState)

import PsGame.Controls (Control, ControlsState)
import PsGame.Controls as Controls
import PsGame.Data.Milliseconds (Milliseconds(Milliseconds))
import PsGame.InputsEvent (InputsEvent, foldpInputsEvent)
import PsGame.InputsState (InputsState, initialInputsState)

type GameControls gameEvent =
  Array (Control gameEvent)
type GameDeriveEvents gameEvent =
  InputsState → Array gameEvent
type GameUpdate gameEvent gameState effects =
  gameEvent → gameState → Tuple gameState (Array (Aff (Maybe gameEvent)))
type GameView gameState gameEvent =
  gameState → Html gameEvent

newtype State gameEvent gameState =
  State
  { controlsState ∷ ControlsState gameEvent
  , gameState ∷ gameState
  , inputsState ∷ InputsState
  }

instance hasControlsStateState ∷ HasControlsState (State gameEvent gameState) (ControlsState gameEvent) where
  _controlsState ∷ Lens' (State gameEvent gameState) (ControlsState gameEvent)
  _controlsState =
    lens
      (\(State o) → o.controlsState)
      (\(State o) → State <<< o { controlsState = _ })

instance hasGameStateState ∷ HasGameState (State gameEvent gameState) gameState where
  _gameState ∷ Lens' (State gameEvent gameState) gameState
  _gameState =
    lens
      (\(State o) → o.gameState)
      (\(State o) → State <<< o { gameState = _ })

instance hasInputsStateState ∷ HasInputsState (State gameEvent gameState) InputsState where
  _inputsState ∷ Lens' (State gameEvent gameState) InputsState
  _inputsState =
    lens
      (\(State o) → o.inputsState)
      (\(State o) → State <<< o { inputsState = _ })

data Event gameEvent
  = GameEvent gameEvent
  | InputsEvent InputsEvent
  | Tick Milliseconds

makeUpdate
  ∷ ∀ gameEvent gameState effects
  . GameDeriveEvents gameEvent
  → GameUpdate gameEvent gameState effects
  → Event gameEvent
  → State gameEvent gameState
  → Tuple gameState (Array (Aff (Maybe gameEvent)))
makeUpdate _ gameUpdate (GameEvent gameEvent) state =
  updateGameState gameUpdate state [gameEvent]
makeUpdate _ _ (InputsEvent inputsEvent) state =
  Tuple (state#_inputsState %~ foldpInputsEvent inputsEvent) []
makeUpdate gameDeriveEvents gameUpdate (Tick tickTime) state =
  let
    Tuple controlsState controlsGameEvents =
      Controls.update tickTime (state^._inputsState) (state^._controlsState)

    gameEvents = controlsGameEvents <> gameDeriveEvents (state^._inputsState)
  in
    updateGameState gameUpdate (state#_controlsState .~ controlsState) gameEvents

applyGameEventsToGameState
  ∷ ∀ gameEvent gameState effects
  . GameUpdate gameEvent gameState effects
  → gameState
  → Array gameEvent
  → Tuple gameState (Array (Aff (Maybe gameEvent)))
applyGameEventsToGameState gameUpdate gameState =
  foldl
    (\acc gameEvent →
      let Tuple gameState' gameEffects = gameUpdate gameEvent acc.state
      in (Tuple gameState' (acc.effects <> gameEffects)))
    (Tuple gameState [])

updateGameState
  ∷ ∀ gameEvent gameState effects
  . GameUpdate gameEvent gameState effects
  → State gameEvent gameState
  → Array gameEvent
  → Tuple gameState (Array (Aff (Maybe gameEvent)))
updateGameState gameUpdate state gameEvents =
  let
    { state: gameState
    , effects: gameEffects
    } =
      applyGameEventsToGameState gameUpdate (state^._gameState) gameEvents
  in
    { state:
      state#_gameState .~ gameState
    , effects:
      map gameEffectToEffect gameEffects
    }

makeView
  ∷ ∀ gameEvent gameState
  . GameView gameState
  → State gameEvent gameState
  → Html gameEvent
makeView gameView state =
  gameView (state^._gameState)

makeInitialState ∷ ∀ gameEvent gameState. Array (Control gameEvent) → gameState → State gameEvent gameState
makeInitialState gameControls initialGameState =
  State
  { controlsState: Controls.makeInitialState gameControls
  , gameState: initialGameState
  , inputsState: initialInputsState
  }

start
  ∷ ∀ gameEvent gameState effects
  . gameState
  → GameControls gameEvent
  → GameDeriveEvents gameEvent
  → GameUpdate gameEvent gameState effects
  → GameView gameState
  → Effect Unit
start gameInitialState gameControls gameDeriveEvents gameUpdate gameView =
  do
    mount_ (QuerySelector "#app")
      { subscribe: []
      , view: makeView gameView
      , init: makeInitialState gameControls gameInitialState
      , update: makeUpdate gameDeriveEvents gameUpdate
      }
    let tickTime = 10
    void $ setInterval tickTime (send id (Tick tickTime))
