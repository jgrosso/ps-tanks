module PsTanks.View where

import Prelude hiding (div, top)

import Control.Monad.Except (runExcept)

import CSS.Display (absolute, position)
import CSS.Geometry (height, left, top, width)
import CSS.Size (px, vh, vw)
import CSS.Transform (rotate, transform)

import Data.Either (either)

import DOM.Event.KeyboardEvent (eventToKeyboardEvent)

import Lens (class HasPosition, class HasRotation, _player, _position, _rotation, _x, _y)

import Optic.Core ((..))
import Optic.Getter ((^.))

import PsGame.InputsEvent (InputsEvent(KeyDown, KeyUp, Noop))

import PsTanks.Angle (Degrees, toCssAngle)
import PsTanks.State (State)
import PsTanks.Vector2 (Vector2)

import Pux.DOM.Events (onKeyDown, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)

import Text.Smolder.HTML (div, img)
import Text.Smolder.HTML.Attributes (src, tabindex)
import Text.Smolder.Markup ((!), (#!))

view ∷ State → HTML InputsEvent
view state =
  div
    ! style do
        width $ 100.0 # vw
        height $ 100.0 # vh

    ! tabindex "1" -- https://stackoverflow.com/questions/3149362#comment32854419_3149416
    #! onKeyDown
         (eventToKeyboardEvent >>> runExcept >>>
          either (const Noop) KeyDown)
    #! onKeyUp
         (eventToKeyboardEvent >>> runExcept >>>
          either (const Noop) KeyUp)

    $ do
      viewEntity (state^._player)
        $ img
          ! src "./img/tank.png"

viewEntity
  ∷ ∀ entity event
  . HasPosition entity Vector2
  ⇒ HasRotation entity Degrees
  ⇒ entity
  → HTML event
  → HTML event
viewEntity entity entityHtml =
  div
    ! style do
        position absolute
        left $ (entity^._position.._x) # px
        top $ -(entity^._position.._y) # px
        transform $ rotate (toCssAngle (entity^._rotation))
    $ entityHtml
