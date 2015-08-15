module GeoLabel.Geometry.Conversion (
    polar,
    cartesian
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude

import GeoLabel.Geometry.Point (Point(..), (<.>))
import GeoLabel.Geometry.Polar (Polar(..))

polar :: Point -> Polar
polar point@(Point x y z) = Polar r theta phi
    where
    r = sqrt (point <.> point)
    theta = atan2 y x
    phi = atan2 (sqrt (x*x+y*y)) z

cartesian :: Polar -> Point
cartesian (Polar r theta phi) = Point x y z
    where
    x = r * cos theta * sin phi
    y = r * sin theta * sin phi
    z = r * cos phi