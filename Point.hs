module Point (Point, (<+>), (<->), (<.>), scale) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude

data Point = Point (Length Double) (Length Double) (Length Double)

(<+>) :: Point -> Point -> Point
(Point a b c) <+> (Point x y z) = Point (a + x) (b + y) (c + z)
(<->) :: Point -> Point -> Point
(Point a b c) <-> (Point x y z) = Point (a - x) (b - y) (c - z)
(<.>) :: Point -> Point -> Area Double
(Point a b c) <.> (Point x y z) = (a * x) + (b * y) + (c * z)

scale :: Dimensionless Double -> Point -> Point
scale s (Point a b c) = Point (s * a) (s * b) (s * c)