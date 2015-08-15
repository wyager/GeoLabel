module GeoLabel.Polytope.Triangle (
    Triangle(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Polytope.Polygon (Polygon(split, centroid, contains))
import GeoLabel.Geometry.Point (V3(..), Point, scale, (<+>))
import GeoLabel.Geometry.Plane (Plane, plane, Side(Inside), side)
import Numeric.Units.Dimensional ((*~), one)

data Triangle = Triangle Point Point Point deriving Show

instance Polygon Triangle where
    split (Triangle a b c) = (tri a x y, tri x y z, tri x b z, tri y c z)
        where
        tri = Triangle
        x = scale (0.5 *~ one) (a <+> b)
        y = scale (0.5 *~ one) (a <+> c)
        z = scale (0.5 *~ one) (b <+> c)
    centroid (Triangle a b c) = scale (_1 / _3) (a <+> b <+> c)
    contains (Triangle a b c) point = a `onSameSideOf` bc &&
                                      b `onSameSideOf` ac &&
                                      c `onSameSideOf` ab
        where
        zero = V3 _0 _0 _0
        ab = plane a b zero
        ac = plane a c zero
        bc = plane b c zero
        corner `onSameSideOf` plane = side plane point == Inside ||
                                      side plane point == side plane corner