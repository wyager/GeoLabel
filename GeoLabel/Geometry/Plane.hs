module GeoLabel.Geometry.Plane (
    Plane(..), plane,
    Side(..), side
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Geometry.Point (V3(..), Point, Bivector, (<->), (<%>), (<.>))

-- | Represents an equation of the form xX + yY + zZ = d
data Plane = Plane { x :: Area Double, y :: Area Double,
                     z :: Area Double, d :: Volume Double }

plane :: Point -> Point -> Point -> Plane
plane a b c = Plane x y z d
    where
    ac = a <-> c
    bc = b <-> c
    normal@(V3 x y z) = ac <%> bc :: Bivector
    d = normal <.> c

side :: Plane -> Point -> Side
side (Plane x y z d) point
    | sum <  _0 = Behind
    | sum == _0 = Inside
    | sum >  _0 = Infront
    where
    sum = ((V3 x y z) <.> point) - d

data Side = Behind | Inside | Infront