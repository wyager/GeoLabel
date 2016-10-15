module GeoLabel.Polytope.Triangle (
    Triangle(..)
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Polytope.Polygon (Polygon, split, centroid, contains, vertices)
import GeoLabel.Geometry.Point (V3(..), Point, Bivector, scale, (<+>), (<->), (<.>), (<%>))
import GeoLabel.Geometry.Plane (Plane, plane, Side(Inside), side)
import GeoLabel.Real (R)
import Numeric.Units.Dimensional ((*~), one)

data Triangle = Triangle Point Point Point deriving Show

instance Polygon Triangle where
    split (Triangle a b c) = (tri a x y, tri x y z, tri x b z, tri y z c)
        where
        tri = Triangle
        x = scale (0.5 *~ one) (a <+> b)
        y = scale (0.5 *~ one) (a <+> c)
        z = scale (0.5 *~ one) (b <+> c)
    centroid (Triangle a b c) = scale (_1 / _3) (a <+> b <+> c)
    ---- Projecting onto triangle + side of line
    --contains (Triangle a b c) point = all (`contain` intersection) [(a,b,c),(b,c,a),(c,a,b)]
    --    where
    --    -- Using wikipedia line/plane intersection algorithm
    --    ab = a <-> b
    --    ac = a <-> c
    --    normal = ab <%> ac
    --    numerator = a <.> normal
    --    denominator = point <.> normal
    --    scalar = numerator / denominator
    --    -- This is where the vector from the center of the earth to the point hits the triangle plane
    --    intersection = scale scalar point
    --    -- Now we check if the point is on the same side of the triangle as the opposite triangle vertex
    --    -- In the plane formed by the vector
    --    contain :: (Point, Point, Point) -> Point -> Bool
    --    (a,b,o) `contain` intersection = bothAbove || bothBelow
    --        where
    --        normal = a <%> b -- Wedge from origin
    --        pointAltitude = point <.> normal -- "Altitude" above wedge
    --        oppositeAltitude = o <.> normal -- "Altitude" of opposite vertex above wedge
    --        bothAbove = pointAltitude <= _0 && oppositeAltitude <= _0
    --        bothBelow = pointAltitude >= _0 && oppositeAltitude >= _0



        

     --Using side of plane
    contains (Triangle a b c) point = a `onSameSideOf` bc &&
                                      b `onSameSideOf` ac &&
                                      c `onSameSideOf` ab
        where
        zero = V3 _0 _0 _0
        ab = plane a b zero
        ac = plane a c zero
        bc = plane b c zero
        corner `onSameSideOf` plane = -- side plane point == Inside ||
                                      side plane point == side plane corner
    vertices (Triangle a b c) = [a,b,c]