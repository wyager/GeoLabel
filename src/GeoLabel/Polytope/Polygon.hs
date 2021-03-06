module GeoLabel.Polytope.Polygon (
    Polygon, split, centroid, contains, vertices
) where

import GeoLabel.Geometry.Point (Point)

class Polygon a where
    split :: a -> (a, a, a, a)
    centroid :: a -> Point
    contains :: a -> Point -> Bool
    vertices :: a -> [Point]