module PsTanks.Image where

import Prelude

import Data.Lens (lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((+~))
import Data.Lens.Types (Lens')

import Lens (class HasDimensions, class HasImage, class HasPosition, class HasRotation, class HasSourceUrl, _dimensions, _image, _position, _rotation, _x, _y)

import PsTanks.Data.Coordinate (Coordinate)
import PsTanks.Data.Angle (class Angle, cos, sin)
import PsTanks.Data.Dimensions (Dimensions)
import PsTanks.Data.Url (Url)

newtype Image =
  Image
  { dimensions ∷ Dimensions
  , sourceUrl ∷ Url
  }

instance hasDimensionsImage ∷ HasDimensions Image Dimensions where
  _dimensions ∷ Lens' Image Dimensions
  _dimensions =
    lens
      (\(Image o) → o.dimensions)
      (\(Image o) → Image <<< o { dimensions = _ })

instance hasSourceUrlImage ∷ HasSourceUrl Image Url where
  _sourceUrl ∷ Lens' Image Url
  _sourceUrl =
    lens
      (\(Image o) → o.sourceUrl)
      (\(Image o) → Image <<< o { sourceUrl = _ })

topLeft ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
topLeft entity =
  entity^._position

bottomLeft ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
bottomLeft entity =
  entity^._position#_y +~ entity^._image<<<_dimensions<<<_y

topRight ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
topRight entity =
  entity^._position#_x +~ entity^._image<<<_dimensions<<<_x

bottomRight ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
bottomRight entity =
  entity^._position#_x +~ entity^._image<<<_dimensions<<<_x
                   #_y +~ entity^._image<<<_dimensions<<<_y

centerRight ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
centerRight entity =
  topRight entity#_y +~ (entity^._image<<<_dimensions<<<_y) / 2.0

-- TODO Rename
-- TODO Handle rotation
-- getCoordinate ∷
--   ∀ entity
--   . HasImage entity Image
--   ⇒ HasPosition entity Coordinate
--   ⇒ Vertical
--   → Horizontal
--   → entity
--   → Coordinate
-- getCoordinate vertical horizontal entity = _

data Horizontal
  = HorizontalCenter
  | Left
  | Right

data Vertical
  = Bottom
  | Top
  | VerticalCenter

-- TODO Rename
-- TODO Handle rotation
getHorizontalCoordinate ∷
  ∀ angle entity
  . Angle angle
  ⇒ HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ HasRotation entity angle
  ⇒ Horizontal
  → entity
  → Number
getHorizontalCoordinate HorizontalCenter entity =
  (entity^._image<<<_dimensions<<<_x) / 2.0
getHorizontalCoordinate Left entity =
  getHorizontalCoordinate HorizontalCenter entity
  + ((entity^._image<<<_dimensions<<<_x) * cos (entity^._rotation))
  - ((entity^._image<<<_dimensions<<<_y) * sin (entity^._rotation))
getHorizontalCoordinate _ _ = -1.0

-- TODO Rename
-- TODO Handle rotation
getVerticalCoordinate ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ Vertical
  → entity
  → Number
getVerticalCoordinate VerticalCenter entity =
  (entity^._image<<<_dimensions<<<_y) / 2.0
getVerticalCoordinate _ _ = -1.0
