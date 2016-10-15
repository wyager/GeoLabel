module GeoLabel.Unit.Latlon (
    Latlon(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Real (R)

data Latlon = Latlon {lat :: Angle R, lon :: Angle R} deriving Show
