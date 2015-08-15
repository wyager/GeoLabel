module GeoLabel.Geometry.Point (
    Point(..), (<+>), (<->), (<.>), (<%>), scale, kick, unit,
    Bivector(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude

data V3 a = V3 a a a deriving Show

data Point = Point (Length Double) (Length Double) (Length Double) 
    deriving Show
-- Turns out cross products are super weird. 
-- https://en.wikipedia.org/wiki/Exterior_algebra
data Bivector = Bivector (Area Double) (Area Double) (Area Double)
    deriving Show

(<+>) :: Point -> Point -> Point
(Point a b c) <+> (Point x y z) = Point (a + x) (b + y) (c + z)
(<->) :: Point -> Point -> Point
(Point a b c) <-> (Point x y z) = Point (a - x) (b - y) (c - z)
(<.>) :: Point -> Point -> Area Double
(Point a b c) <.> (Point x y z) = (a * x) + (b * y) + (c * z)
(<%>) :: Point -> Point -> Bivector
(Point a b c) <%> (Point x y z) = Bivector (b*z - c*y) (c*x - a*z) (a*y - b*x)

scale :: Dimensionless Double -> Point -> Point
scale s (Point a b c) = Point (s * a) (s * b) (s * c)

kick :: Point -> Point -> Point
kick point destination = point <+> delta
    where
    delta = small (destination <-> point)

small :: Point -> Point
small vector = scale (0.01 *~ one) (unit vector) -- 1 centimeter

unit :: Point -> Point
unit vector = scale (1.0 *~ meter / length) vector
    where length = sqrt (vector <.> vector)