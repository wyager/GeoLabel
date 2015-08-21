module GeoLabel.Geodesic.Grid (
    locationOf, pointOf
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import Data.List (findIndex)
import GeoLabel.Polytope.Polyhedron (Polyhedron(..))
import GeoLabel.Polytope.Polygon (Polygon, centroid, contains, vertices)
import GeoLabel.Geometry.QuadTree (QT(..), qt, Subface(A,B,C,D), subfaces)
import GeoLabel.Geometry.Point (Point, kick, unit)
import GeoLabel.Unit.Location (Location(..))

pointOf :: Polygon face => Location -> Polyhedron face -> Point
pointOf (Location index subs) (Polyhedron faces) = centroid subface
    where subface = subfaces (faces !! index) subs

locationOf :: Polygon face => Point -> Polyhedron face -> Location
locationOf point (Polyhedron faces) = Location index subfaces
    where
    Just index = findIndex (`contains` point) faces
    face = faces !! index
    -- This gives us a resolution of about the area of a dinner plate
    subfaces = take 25 $ explore (qt face) point

explore :: Polygon face => QT face -> Point -> [Subface]
explore (QT f a b c d) point
    | within a = A : explore a point
    | within b = B : explore b point
    | within c = C : explore c point
    | within d = D : explore d point
    | otherwise = error $ pretty f ++ "\n  :  \n" ++ show (unit point)
    | otherwise = explore (QT f a b c d) (kick point (centroid f))
    where within (QT f _ _ _ _) = f `contains` point

pretty :: Polygon face => face -> String
pretty f = concat [show (unit v) ++ "\n" | v <- vertices f]
