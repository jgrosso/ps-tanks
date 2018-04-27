module PsGame where

import Prelude

import Lens (class HasControlsState, class HasGameState, class HasInputsState, _controlsState, _gameState, _inputsState)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)

import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple))

import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Setter ((.~), (%~))
import Optic.Types (Lens')

import PsGame.Controls (Control, ControlsState)
import PsGame.Controls as Controls
import PsGame.Data.Milliseconds (Milliseconds(Milliseconds))
import PsGame.InputsEvent (InputsEvent, foldpInputsEvent)
import PsGame.InputsState (InputsState, initialInputsState)

import Pux (CoreEffects, EffModel, noEffects)
import Pux as Pux
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)

import Signal.Time (every, millisecond)

import Text.Smolder.Markup (mapEvent)

type GameControls gameEvent =
  Array (Control gameEvent)
type GameDeriveEvents gameEvent =
  InputsState → Array gameEvent
type GameUpdate gameEvent gameState effects =
  gameEvent → gameState → EffModel gameState gameEvent effects
type GameView gameState =
  gameState → HTML InputsEvent

type Effect' event effects =
  Aff (CoreEffects effects) (Maybe event)
type Effect gameEvent e =
  Effect' (Event gameEvent) e

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

gameEffectToEffect ∷ ∀ gameEvent effects. Effect' gameEvent effects → Effect gameEvent effects
gameEffectToEffect =
  map (map GameEvent)

makeFoldp
  ∷ ∀ gameEvent gameState effects
  . GameDeriveEvents gameEvent
  → GameUpdate gameEvent gameState effects
  → Event gameEvent
  → State gameEvent gameState
  → EffModel (State gameEvent gameState) (Event gameEvent) effects
makeFoldp _ gameUpdate (GameEvent gameEvent) state =
  updateGameState gameUpdate state [gameEvent]
makeFoldp _ _ (InputsEvent inputsEvent) state =
  noEffects $
    state#_inputsState %~ foldpInputsEvent inputsEvent
makeFoldp gameDeriveEvents gameUpdate (Tick tickTime) state =
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
  → EffModel gameState gameEvent effects
applyGameEventsToGameState gameUpdate gameState =
  foldl
    (\acc gameEvent →
      let
        { state: gameState'
        , effects: gameEffects
        } =
          gameUpdate gameEvent acc.state
      in
        acc
          { state = gameState'
          , effects = acc.effects <> gameEffects
          })
    { state: gameState
    , effects: []
    }

updateGameState
  ∷ ∀ gameEvent gameState effects
  . GameUpdate gameEvent gameState effects
  → State gameEvent gameState
  → Array gameEvent
  → EffModel (State gameEvent gameState) (Event gameEvent) effects
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
  → HTML (Event gameEvent)
makeView gameView state =
  mapEvent (InputsEvent <<< _) $ gameView (state^._gameState)

makeInitialState ∷ ∀ gameEvent gameState. Array (Control gameEvent) → gameState → State gameEvent gameState
makeInitialState gameControls initialGameState =
  State
  { controlsState: Controls.makeInitialState gameControls
  , gameState: initialGameState
  , inputsState: initialInputsState
  }

type Game effects = Eff (CoreEffects effects) Unit

start
  ∷ ∀ gameEvent gameState effects
  . gameState
  → GameControls gameEvent
  → GameDeriveEvents gameEvent
  → GameUpdate gameEvent gameState effects
  → GameView gameState
  → Eff (CoreEffects effects) Unit
start gameInitialState gameControls gameDeriveEvents gameUpdate gameView =
  do
    let tickTime = 10.0 * millisecond
    let tick = every tickTime $> Tick (Milliseconds tickTime)
    app ← Pux.start
      { initialState: makeInitialState gameControls gameInitialState
      , view: makeView gameView
      , foldp: makeFoldp gameDeriveEvents gameUpdate
      , inputs: [tick]
      }
    renderToDOM "#app" app.markup app.input
