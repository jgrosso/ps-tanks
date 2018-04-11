module PsTanks.GameControls where

import PsTanks.Controls (Control(Control))
import PsTanks.GameEvent (Event(PlayerRotate, PlayerTranslate), RotateDirection(Clockwise, Counterclockwise), TranslateDirection(Backward, Forward))
import PsTanks.InputsState (KeyCode(KeyCode))

controls ∷ Array Control
controls =
  [ Control { trigger: KeyCode "KeyA", action: PlayerRotate Counterclockwise }
  , Control { trigger: KeyCode "KeyD", action: PlayerRotate Clockwise }
  , Control { trigger: KeyCode "KeyS", action: PlayerTranslate Backward }
  , Control { trigger: KeyCode "KeyW", action: PlayerTranslate Forward }
  ]
