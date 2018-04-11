module PsTanks.GameView where

import Prelude hiding (div, top)

import Control.Monad.Except (runExcept)

import CSS.Display (absolute, position)
import CSS.Geometry (height, left, top, width)
import CSS.Size (deg, px, vh, vw)
import CSS.Transform (rotate, transform)

import Data.Either (either)
import Data.Int (toNumber)
import Data.Newtype (unwrap)

import DOM.Event.KeyboardEvent (eventToKeyboardEvent)

import Optic.Core ((..))
import Optic.Getter ((^.))

import PsTanks.GameState (State)
import PsTanks.InputsEvent (InputsEvent(KeyDown, KeyUp, Noop))
import PsTanks.Lens (_player, _position, _rotation, _x, _y)

import Pux.DOM.Events (onKeyDown, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)

import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (tabindex)
import Text.Smolder.Markup ((!), (#!), text)

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
      div
        ! style do
            position absolute
            left $ toNumber (state^._player.._position.._x) # px
            top $ -(toNumber (state^._player.._position.._y)) # px
            transform $ rotate (unwrap (state^._player.._rotation) # deg)
        $ text "Tank"
