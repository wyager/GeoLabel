module GeoLabel.Geometry.QuadTree (
    QT(..), qt, Subface(..), subface, subfaces
) where

import GeoLabel.Polytope.Polygon (Polygon, split)

data QT face = QT face (QT face) (QT face) (QT face) (QT face)

qt :: Polygon face => face -> QT face
qt face = QT face (qt a) (qt b) (qt c) (qt d)
    where (a,b,c,d) = split face

data Subface = A | B | C | D deriving (Show, Eq, Ord)

subface :: Polygon face => face -> Subface -> face
subface face A = let (a,_,_,_) = split face in a
subface face B = let (_,b,_,_) = split face in b
subface face C = let (_,_,c,_) = split face in c
subface face D = let (_,_,_,d) = split face in d

subfaces :: Polygon face => face -> [Subface] -> face
subfaces = foldl subface