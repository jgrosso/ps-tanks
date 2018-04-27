module PsTanks.Inputs where

import Prelude

import PsGame.Controls (Action(Action), Control(Control))
import PsGame.Data.Cooldown (mkCooldown, noCooldown)
import PsGame.Data.Milliseconds (Milliseconds(Milliseconds))
import PsGame.InputsState (InputsState, KeyCode(KeyCode), keyDown)

import PsTanks.Event (Event(FireBullet, MoveBullets, PlayerRotate, PlayerTranslate), RotateDirection(Clockwise, Counterclockwise), TranslateDirection(Backward, Forward))

controls ∷ Array (Control Event)
controls =
  [ Control
    { action:
      Action
      { event: PlayerRotate Counterclockwise
      , trigger: keyDown $ KeyCode "KeyA"
      }
    , cooldown: noCooldown
    }
  , Control
    { action:
      Action
      { event: PlayerRotate Clockwise
      , trigger: keyDown $ KeyCode "KeyD"
      }
    , cooldown: noCooldown
    }
  , Control
    { action:
      Action
      { event: PlayerTranslate Backward
      , trigger: keyDown $ KeyCode "KeyS"
      }
    , cooldown: noCooldown
    }
  , Control
    { action:
      Action
      { event: PlayerTranslate Forward
      , trigger: keyDown $ KeyCode "KeyW"
      }
    , cooldown: noCooldown
    }
  , Control
    { action:
      Action
      { event: FireBullet
      , trigger: keyDown $ KeyCode "Space"
      }
    , cooldown:
      mkCooldown $ Milliseconds 500.0
    }
  ]

deriveEvents ∷ InputsState → Array Event
deriveEvents =
  const [MoveBullets]
