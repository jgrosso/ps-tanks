module PsGame.InputsEvent where

import Prelude

import Data.Set (delete, insert)

import DOM.Event.KeyboardEvent (code)
import DOM.Event.Types (KeyboardEvent)

import Lens (_keysDown)

import Optic.Setter ((%~))

import PsGame.InputsState (InputsState, KeyCode(KeyCode))

data InputsEvent
  = KeyDown KeyboardEvent
  | KeyUp KeyboardEvent
  | Noop

foldpInputsEvent ∷ InputsEvent → InputsState → InputsState
foldpInputsEvent (KeyDown keyboardEvent) =
  _keysDown %~ insert (KeyCode $ code keyboardEvent)
foldpInputsEvent (KeyUp keyboardEvent) =
  _keysDown %~ delete (KeyCode $ code keyboardEvent)
foldpInputsEvent Noop = id
