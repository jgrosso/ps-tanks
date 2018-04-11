module PsTanks.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Set (delete, insert)

import Math (cos, sin)

import Optic.Core ((..))
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Setter ((.~), (%~), (+~))
import Optic.Types (Lens')

import PsTanks.Angle (Degrees, deg, degreesToRadians)
import PsTanks.Controls (deriveGameEvents)
import PsTanks.Lens (class HasGameState, class HasInputsState, _gameState, _inputsState)
import PsTanks.GameControls as GameControls
import PsTanks.GameEvent as GameEvent
import PsTanks.GameState as GameState
import PsTanks.GameView as GameView
import PsTanks.InputsEvent (InputsEvent, foldpInputsEvent)
import PsTanks.InputsState (InputsState, initialInputsState)

import Pux (CoreEffects, EffModel, noEffects, start)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)

import Signal.Time (every, millisecond)

import Text.Smolder.Markup (mapEvent)

newtype State =
  State
  { gameState ∷ GameState.State
  , inputsState ∷ InputsState
  }

instance hasGameStateState ∷ HasGameState State GameState.State where
  _gameState ∷ Lens' State GameState.State
  _gameState =
    lens
      (\(State o) → o.gameState)
      (\(State o) → State <<< o { gameState = _ })

instance hasInputsStateState ∷ HasInputsState State InputsState where
  _inputsState ∷ Lens' State InputsState
  _inputsState =
    lens
      (\(State o) → o.inputsState)
      (\(State o) → State <<< o { inputsState = _ })

data Event
  = GameEvent GameEvent.Event
  | InputsEvent InputsEvent
  | Tick

gameEffectToEffect ∷ ∀ e. GameEffect e → Effect e
gameEffectToEffect =
  map (map GameEvent)

foldp ∷ ∀ e. Event → State → EffModel State Event e
foldp (GameEvent gameEvent) state =
  updateGameState state [gameEvent]
foldp (InputsEvent inputsEvent) state =
  noEffects $
    state#_inputsState %~ foldpInputsEvent inputsEvent
foldp Tick state =
  let
    gameEvents =
      deriveGameEvents GameControls.controls (state^._inputsState)
  in
    updateGameState state gameEvents

type Effect' event effects =
  Aff (CoreEffects effects) (Maybe event)
type Effect e =
  Effect' Event e
type GameEffect e = Effect' GameEvent.Event e

applyGameEventsToGameState ∷ ∀ e. GameState.State → Array GameEvent.Event → EffModel GameState.State GameEvent.Event e
applyGameEventsToGameState gameState =
  foldl
    (\acc gameEvent →
      let
        { state: gameState'
        , effects: gameEffects
        } =
          GameEvent.update gameEvent acc.state
      in
        acc
          { state = gameState'
          , effects = acc.effects <> gameEffects
          })
    { state: gameState
    , effects: []
    }

updateGameState ∷ ∀ e. State -> Array GameEvent.Event -> EffModel State Event e
updateGameState state gameEvents =
  let
    { state: gameState
    , effects: gameEffects
    } =
      applyGameEventsToGameState (state^._gameState) gameEvents
  in
    { state:
      state#_gameState .~ gameState
    , effects:
      map gameEffectToEffect gameEffects
    }

view ∷ State → HTML Event
view state =
  mapEvent (InputsEvent <<< _) $ GameView.view (state^._gameState)

initialState ∷ State
initialState =
  State
  { gameState: GameState.initialState
  , inputsState: initialInputsState
  }

main ∷ Eff (CoreEffects ()) Unit
main = do
  let tick = every (50.0 * millisecond) $> Tick
  app ← start
    { initialState
    , view
    , foldp
    , inputs: [tick]
    }
  renderToDOM "#app" app.markup app.input
