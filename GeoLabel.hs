module GeoLabel (
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import Data.Vector (Vector, findIndex, (!))

newtype Polyhedron face = Polyhedron (Vector face)

data Point = Point (Length Double) (Length Double) (Length Double)

(<+>) :: Point -> Point -> Point
(Point a b c) <+> (Point x y z) = Point (a + x) (b + y) (c + z)
(<->) :: Point -> Point -> Point
(Point a b c) <-> (Point x y z) = Point (a - x) (b - y) (c - z)
(<.>) :: Point -> Point -> Area Double
(Point a b c) <.> (Point x y z) = (a * x) + (b * y) + (c * z)


scale :: Dimensionless Double -> Point -> Point
scale s (Point a b c) = Point (s * a) (s * b) (s * c)

class Polygon a where
    split :: a -> (a, a, a, a)
    centroid :: a -> Point
    contains :: a -> Point -> Bool

data QT face = QT face (QT face) (QT face) (QT face) (QT face)

qt :: Polygon face => face -> QT face
qt face = QT face (qt a) (qt b) (qt c) (qt d)
    where (a,b,c,d) = split face

data Subface = A | B | C | D

find :: Polygon face => Point -> Polyhedron face -> (Int, [Subface])
find point (Polyhedron faces) = (faceIndex, subfaces)
    where
    Just faceIndex = findIndex (`contains` point) faces
    subfaces = explore (qt (faces ! faceIndex)) point

explore :: Polygon face => QT face -> Point -> [Subface]
explore (QT f a b c d) point
    | within a = A : explore a point
    | within b = B : explore b point
    | within c = C : explore c point
    | within d = D : explore d point
    | otherwise = explore (QT f a b c d) (kick point (centroid f))
    where within (QT f _ _ _ _) = f `contains` point

kick :: Point -> Point -> Point
kick point destination = point <+> delta
    where
    delta = small (destination <-> point)

small :: Point -> Point
small vector = unit vector

unit :: Point -> Point
unit vector = scale (1.0 *~ meter / length) vector
    where length = sqrt (vector <.> vector)

follow :: Polygon face => face -> [Subface] -> face
follow = foldl subface

subface :: Polygon face => face -> Subface -> face
subface face A = let (a,_,_,_) = split face in a
subface face B = let (_,b,_,_) = split face in b
subface face C = let (_,_,c,_) = split face in c
subface face D = let (_,_,_,d) = split face in d

