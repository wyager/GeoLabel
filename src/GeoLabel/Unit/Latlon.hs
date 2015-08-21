module GeoLabel.Unit.Latlon (
    Latlon(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude

data Latlon = Latlon {lat :: Angle Double, lon :: Angle Double} deriving Show
