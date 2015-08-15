module GeoLabel.Unit.Location (
    Location(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Geometry.QuadTree (Subface)

data Location = Location Int [Subface] deriving Show
