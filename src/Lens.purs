module Lens where

import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Data.Tuple (Tuple(Tuple))

class Has_1 a r | a → r where
  _1 ∷ Lens' a r

instance has_1Tuple ∷ Has_1 (Tuple a b) a where
  _1 ∷ Lens' (Tuple a b) a
  _1 =
    lens
      (\(Tuple a _) → a)
      (\(Tuple _ b) a → Tuple a b)

class Has_2 a r | a → r where
  _2 ∷ Lens' a r

instance has_2Tuple ∷ Has_2 (Tuple a b) b where
  _2 ∷ Lens' (Tuple a b) b
  _2 =
    lens
      (\(Tuple _ b) → b)
      (\(Tuple a _) b → Tuple a b)

class HasAction a r | a → r where
  _action ∷ Lens' a r

class HasBullets a r | a → r where
  _bullets ∷ Lens' a r

class HasControls a r | a → r where
  _controls ∷ Lens' a r

class HasControlsState a r | a → r where
  _controlsState ∷ Lens' a r

class HasCooldown a r | a → r where
  _cooldown ∷ Lens' a r

class HasDimensions a r | a → r where
  _dimensions ∷ Lens' a r

class HasElapsedTime a r | a → r where
  _elapsedTime ∷ Lens' a r

class HasEvent a r | a → r where
  _event ∷ Lens' a r

class HasExpectedTime a r | a → r where
  _expectedTime ∷ Lens' a r

class HasGameState a r | a → r where
  _gameState ∷ Lens' a r

class HasImage a r | a → r where
  _image ∷ Lens' a r

class HasInputsState a r | a → r where
  _inputsState ∷ Lens' a r

class HasKeysDown a r | a → r where
  _keysDown ∷ Lens' a r

class HasPlayer a r | a → r where
  _player ∷ Lens' a r

class HasPosition a r | a → r where
  _position ∷ Lens' a r

class HasRotation a r | a → r where
  _rotation ∷ Lens' a r

class HasSourceUrl a r | a → r where
  _sourceUrl ∷ Lens' a r

class HasTrigger a r | a → r where
  _trigger ∷ Lens' a r

class HasX a r | a → r where
  _x ∷ Lens' a r

class HasY a r | a → r where
  _y ∷ Lens' a r
