module GeoLabel.Geometry.Point (
    V3(..), (<+>), (<->), (<.>), (<%>), scale, kick, unit, lengthOf,
    Point,
    Bivector
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude hiding (length)

-- | A vector of three things.
data V3 a = V3 a a a deriving Show
-- | A vector in R³.
type Point = V3 (Length Double)
-- | A bivector for R³.
-- Turns out cross products are super weird. 
-- https://en.wikipedia.org/wiki/Exterior_algebra
type Bivector = V3 (Area Double)

-- | Vector Addition
(V3 a b c) <+> (V3 x y z) = V3 (a + x) (b + y) (c + z)
-- | Vector Subtraction
(V3 a b c) <-> (V3 x y z) = V3 (a - x) (b - y) (c - z)
-- | Dot Product
(V3 a b c) <.> (V3 x y z) = (a * x) + (b * y) + (c * z)
-- | Cross Product
(V3 a b c) <%> (V3 x y z) = V3 (b*z - c*y) (c*x - a*z) (a*y - b*x)

scale :: Dimensionless Double -> Point -> Point
scale s (V3 a b c) = V3 (s * a) (s * b) (s * c)

kick :: Point -> Point -> Point
kick point destination = point <+> delta
    where
    delta = small (destination <-> point)

small :: Point -> Point
small vector = scale (0.01 *~ one) (unit vector) -- 1 centimeter

unit :: Point -> Point
unit vector = scale (1.0 *~ meter / lengthOf vector) vector

lengthOf :: Point -> Length Double
lengthOf vector = sqrt (vector <.> vector)