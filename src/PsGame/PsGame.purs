module PsGame where

import Prelude

import Lens (class HasGameState, class HasInputsState, _gameState, _inputsState)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Foldable (foldl)
import Data.Maybe (Maybe)

import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Setter ((.~), (%~))
import Optic.Types (Lens')

import PsGame.Controls (Control, deriveGameEvents)
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

newtype State gameState =
  State
  { gameState ∷ gameState
  , inputsState ∷ InputsState
  }

instance hasGameStateState ∷ HasGameState (State gameState) gameState where
  _gameState ∷ Lens' (State gameState) gameState
  _gameState =
    lens
      (\(State o) → o.gameState)
      (\(State o) → State <<< o { gameState = _ })

instance hasInputsStateState ∷ HasInputsState (State gameState) InputsState where
  _inputsState ∷ Lens' (State gameState) InputsState
  _inputsState =
    lens
      (\(State o) → o.inputsState)
      (\(State o) → State <<< o { inputsState = _ })

data Event gameEvent
  = GameEvent gameEvent
  | InputsEvent InputsEvent
  | Tick

gameEffectToEffect ∷ ∀ gameEvent effects. Effect' gameEvent effects → Effect gameEvent effects
gameEffectToEffect =
  map (map GameEvent)

makeFoldp
  ∷ ∀ gameEvent gameState effects
  . GameControls gameEvent
  → GameDeriveEvents gameEvent
  → GameUpdate gameEvent gameState effects
  → Event gameEvent
  → State gameState
  → EffModel (State gameState) (Event gameEvent) effects
makeFoldp _ _ gameUpdate (GameEvent gameEvent) state =
  updateGameState gameUpdate state [gameEvent]
makeFoldp _ _ _ (InputsEvent inputsEvent) state =
  noEffects $
    state#_inputsState %~ foldpInputsEvent inputsEvent
makeFoldp gameControls gameDeriveEvents gameUpdate Tick state =
  let
    gameEvents ∷ Array gameEvent
    gameEvents =
      deriveGameEvents gameControls (state^._inputsState) <> gameDeriveEvents (state^._inputsState)
  in
    updateGameState gameUpdate state gameEvents

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
  → State gameState
  → Array gameEvent
  → EffModel (State gameState) (Event gameEvent) effects
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
  → State gameState
  → HTML (Event gameEvent)
makeView gameView state =
  mapEvent (InputsEvent <<< _) $ gameView (state^._gameState)

makeInitialState ∷ ∀ gameState. gameState → State gameState
makeInitialState initialGameState =
  State
  { gameState: initialGameState
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
    let tick = every (10.0 * millisecond) $> Tick
    app ← Pux.start
      { initialState: makeInitialState gameInitialState
      , view: makeView gameView
      , foldp: makeFoldp gameControls gameDeriveEvents gameUpdate
      , inputs: [tick]
      }
    renderToDOM "#app" app.markup app.input
