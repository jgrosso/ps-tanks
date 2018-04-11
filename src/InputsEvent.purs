module PsTanks.InputsEvent where

import Prelude

import Data.Int (round)
import Data.Set (delete, insert)

import DOM.Event.KeyboardEvent (code, eventToKeyboardEvent)
import DOM.Event.Types (KeyboardEvent)

import Optic.Setter ((%~))

import PsTanks.InputsState (InputsState, KeyCode(KeyCode))
import PsTanks.Lens (_keysDown)

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
