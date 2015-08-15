module GeoLabel.Polytope.Triangle (
    Triangle(..)
) where

import GeoLabel.Polytope.Polygon (Polygon(split, centroid, contains))
import GeoLabel.Geometry.Point (Point, scale, (<+>))
import GeoLabel.Geometry.Plane (Plane, plane)
import Numeric.Units.Dimensional ((*~), one)

data Triangle = Triangle Point Point Point deriving Show

instance Polygon Triangle where
    split (Triangle a b c) = (tri a x y, tri x y z, tri x b z, tri y c z)
        where
        tri = Triangle
        x = scale (0.5 *~ one) (a <+> b)
        y = scale (0.5 *~ one) (a <+> c)
        z = scale (0.5 *~ one) (b <+> c)
    centroid (Triangle a b c) = scale (1/3 *~ one) (a <+> b <+> c)
    contains = undefined