module PsGame.InputsState where

import Prelude

import Data.Lens (lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Types (Lens')
import Data.Monoid (mempty)
import Data.Set (Set)

import Lens (class HasKeysDown, _keysDown)

import PsGame.Utils ((∈))

newtype KeyCode =
  KeyCode String

derive instance eqKeyCode ∷ Eq KeyCode
derive instance ordKeyCode ∷ Ord KeyCode

newtype InputsState =
  InputsState
  { keysDown ∷ Set KeyCode
  }

derive instance eqInputsState ∷ Eq InputsState

instance hasKeysDownInputsState ∷ HasKeysDown InputsState (Set KeyCode) where
  _keysDown ∷ Lens' InputsState (Set KeyCode)
  _keysDown =
    lens
      (\(InputsState o) → o.keysDown)
      (\(InputsState o) → InputsState <<< o { keysDown = _ })

initialInputsState ∷ InputsState
initialInputsState =
  InputsState
  { keysDown: mempty
  }

keyDown ∷ KeyCode → InputsState → Boolean
keyDown keyCode inputsState =
  keyCode ∈ (inputsState^._keysDown)
