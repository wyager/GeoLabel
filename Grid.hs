module Grid (
    Location(..),
    locate, pointTo
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import Data.Vector ((!), findIndex)
import Point (Point, kick)
import Polytope (
    Polyhedron(..),
    Polygon, split, centroid, contains,
    QT(..), Subface(..), qt) 

data Location = Location Int [Subface] deriving Show

locate :: Polygon face => Point -> Polyhedron face -> Location
locate point (Polyhedron faces) = Location faceIndex subfaces
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

pointTo :: Polygon face => Location -> Polyhedron face -> Point
pointTo (Location face subfaces) (Polyhedron faces) = centroid subface
    where subface = follow (faces ! face) subfaces

follow :: Polygon face => face -> [Subface] -> face
follow = foldl subface

subface :: Polygon face => face -> Subface -> face
subface face A = let (a,_,_,_) = split face in a
subface face B = let (_,b,_,_) = split face in b
subface face C = let (_,_,c,_) = split face in c
subface face D = let (_,_,_,d) = split face in d