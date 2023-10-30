module PsGame.Data.Cooldown where

import Prelude

import Data.Lens (lens)
import Data.Lens.Types (Lens')

import Lens (class HasElapsedTime, class HasExpectedTime)

import PsGame.Data.Milliseconds (Milliseconds(Milliseconds))

newtype Cooldown =
  Cooldown
  { elapsedTime ∷ Milliseconds
  , expectedTime ∷ Milliseconds
  }

instance hasElapsedTimeCooldown ∷ HasElapsedTime Cooldown Milliseconds where
  _elapsedTime ∷ Lens' Cooldown Milliseconds
  _elapsedTime =
    lens
      (\(Cooldown o) → o.elapsedTime)
      (\(Cooldown o) → Cooldown <<< o { elapsedTime = _ })

instance hasExpectedTimeCooldown ∷ HasExpectedTime Cooldown Milliseconds where
  _expectedTime ∷ Lens' Cooldown Milliseconds
  _expectedTime =
    lens
      (\(Cooldown o) → o.expectedTime)
      (\(Cooldown o) → Cooldown <<< o { expectedTime = _ })

mkCooldown ∷ Milliseconds → Cooldown
mkCooldown expectedTime =
  Cooldown
  { elapsedTime: Milliseconds 0.0
  , expectedTime
  }

noCooldown ∷ Cooldown
noCooldown =
  Cooldown
  { elapsedTime: Milliseconds 0.0
  , expectedTime: Milliseconds 0.0
  }
