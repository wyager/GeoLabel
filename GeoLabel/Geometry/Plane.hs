module GeoLabel.Geometry.Plane (
    Plane(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Geometry.Point (Point(..))

data Plane = Plane { x :: Area Double, y :: Area Double,
                     z :: Area Double, d :: Volume Double }

plane :: Point -> Point -> Point -> Plane
plane a b c = 
    where
    ab = a <-> b
    ac = a <-> c
    normal = ab <%> ac



data Side = Behind | Inside | Infront