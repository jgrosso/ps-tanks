module PsTanks.Image where

import Prelude

import Lens (class HasDimensions, class HasImage, class HasPosition, class HasSourceUrl, _dimensions, _image, _position, _x, _y)

import Optic.Core ((..))
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Setter ((+~))
import Optic.Types (Lens')

import PsTanks.Data.Coordinate (Coordinate)
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
  entity^._position#_y +~ entity^._image.._dimensions.._y

topRight ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
topRight entity =
  entity^._position#_x +~ entity^._image.._dimensions.._x

bottomRight ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
bottomRight entity =
  entity^._position#_x +~ entity^._image.._dimensions.._x
                   #_y +~ entity^._image.._dimensions.._y

centerRight ∷
  ∀ entity
  . HasImage entity Image
  ⇒ HasPosition entity Coordinate
  ⇒ entity
  → Coordinate
centerRight entity =
  topRight entity#_y +~ (entity^._image.._dimensions.._y) / 2.0

-- TODO Rename
-- TODO Handle rotation
-- getCoordinate ∷ Vertical → Horizontal → entity -> Coordinate
-- getCoordinate Bottom Left = todo
-- getCoordinate Bottom Right = todo
-- getCoordinate Middle Left = todo
-- …
