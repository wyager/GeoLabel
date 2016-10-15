module GeoLabel.Geometry.Polar (Polar(..)) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Real (R)

data Polar = Polar { r :: Length R, 
                     theta :: Angle R, 
                     phi :: Angle R } deriving Show
