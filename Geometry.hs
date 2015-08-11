module Geometry (
    Polyhedron(..),
    Polygon, split, centroid, contains,
    QT(..), Subface(..), qt
) where

import Point (Point, scale, (<+>))
import Data.Vector (Vector)
import Numeric.Units.Dimensional ((*~), one)

newtype Polyhedron face = Polyhedron (Vector face)

class Polygon a where
    split :: a -> (a, a, a, a)
    centroid :: a -> Point
    contains :: a -> Point -> Bool

data Triangle = Triangle Point Point Point
instance Polygon Triangle where
    split (Triangle a b c) = (tri a x y, tri x y z, tri x b z, tri y c z)
        where
        tri = Triangle
        x = scale (0.5 *~ one) (a <+> b)
        y = scale (0.5 *~ one) (a <+> c)
        z = scale (0.5 *~ one) (b <+> c)
    centroid (Triangle a b c) = scale (1/3 *~ one) (a <+> b <+> c)
    contains = undefined

type Icosahedron = Polyhedron Triangle


data QT face = QT face (QT face) (QT face) (QT face) (QT face)

qt :: Polygon face => face -> QT face
qt face = QT face (qt a) (qt b) (qt c) (qt d)
    where (a,b,c,d) = split face

data Subface = A | B | C | D