module PsGame.Data.Milliseconds where

import Prelude

newtype Milliseconds =
  Milliseconds Number

derive newtype instance eqMilliseconds ∷ Eq Milliseconds
derive newtype instance ordMilliseconds ∷ Ord Milliseconds
derive newtype instance semiringMilliseconds ∷ Semiring Milliseconds
