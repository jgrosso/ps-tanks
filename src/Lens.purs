module Lens where

import Optic.Types (Lens')

class HasAction a r | a → r where
  _action ∷ Lens' a r

class HasBullets a r | a → r where
  _bullets ∷ Lens' a r

class HasGameState a r | a → r where
  _gameState ∷ Lens' a r

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

class HasTrigger a r | a → r where
  _trigger ∷ Lens' a r

class HasX a r | a → r where
  _x ∷ Lens' a r

class HasY a r | a → r where
  _y ∷ Lens' a r
