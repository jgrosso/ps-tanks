module PsTanks.View where

import Prelude hiding (div, top)

import Control.Monad.Except (runExcept)

import CSS.Display (absolute, position)
import CSS.Geometry (height, left, top, width)
import CSS.Size (px, vh, vw)
import CSS.Transform (rotate, transform)

import Data.Either (either)
import Data.Foldable (for_)
import Data.Lens.Core ((..))
import Data.Lens.Getter ((^.))
import Data.Newtype (unwrap)

import DOM.Event.KeyboardEvent (eventToKeyboardEvent)

import Lens (class HasImage, class HasPosition, class HasRotation, _bullets, _dimensions, _image, _player, _position, _rotation, _sourceUrl, _x, _y)

import PsGame.InputsEvent (InputsEvent(KeyDown, KeyUp, Noop))

import PsTanks.Data.Angle (Degrees, toCssAngle)
import PsTanks.Data.Coordinate (Coordinate)
import PsTanks.Image (Image)
import PsTanks.State (State)

import Pux.DOM.Events (onKeyDown, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)

import Text.Smolder.HTML (div, img)
import Text.Smolder.HTML.Attributes (src, tabindex)
import Text.Smolder.HTML.Attributes as Html
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
        for_ (state^._bullets) viewEntity

viewEntity
  ∷ ∀ entity event
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ HasRotation entity Degrees
  ⇒ entity
  → HTML event
viewEntity entity =
  div
    ! style do
        position absolute
        left $ (entity^._position<<<_x) # px
        top $ -(entity^._position<<<_y) # px
        transform $ rotate (toCssAngle (entity^._rotation))
    $ img
        ! Html.width (show $ entity^._image<<<_dimensions.._x)
        ! Html.height (show $ entity^._image<<<dimensions<<<_y)
        ! src (unwrap $ entity^._image<<<_sourceUrl)
