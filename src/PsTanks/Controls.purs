module PsTanks.Controls where

import PsGame.Controls (Control(Control))
import PsGame.InputsState (KeyCode(KeyCode))

import PsTanks.Event (Event(PlayerRotate, PlayerTranslate), RotateDirection(Clockwise, Counterclockwise), TranslateDirection(Backward, Forward))

controls ∷ Array (Control Event)
controls =
  [ Control { trigger: KeyCode "KeyA", action: PlayerRotate Counterclockwise }
  , Control { trigger: KeyCode "KeyD", action: PlayerRotate Clockwise }
  , Control { trigger: KeyCode "KeyS", action: PlayerTranslate Backward }
  , Control { trigger: KeyCode "KeyW", action: PlayerTranslate Forward }
  ]
