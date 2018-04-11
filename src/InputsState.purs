module PsTanks.InputsState where

import Prelude

import Data.Monoid (mempty)
import Data.Set (Set)

import Optic.Lens (lens)
import Optic.Types (Lens')

import PsTanks.Lens (class HasKeysDown)

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
