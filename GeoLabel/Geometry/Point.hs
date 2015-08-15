module GeoLabel.Geometry.Point (
    Point(..), (<+>), (<->), (<.>), scale, kick, unit
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude

data Point = Point (Length Double) (Length Double) (Length Double) deriving Show

(<+>) :: Point -> Point -> Point
(Point a b c) <+> (Point x y z) = Point (a + x) (b + y) (c + z)
(<->) :: Point -> Point -> Point
(Point a b c) <-> (Point x y z) = Point (a - x) (b - y) (c - z)
(<.>) :: Point -> Point -> Area Double
(Point a b c) <.> (Point x y z) = (a * x) + (b * y) + (c * z)

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