module Conversion (
    Latlon(..), latlon,
    Location(..), location
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import Polar (Polar(..))
import Point (Point(..))
import Geometry (polar, cartesian)
import Grid (Location(..), locate, pointTo)
import Polytope (Triangle(..), Icosahedron, polyhedron)

data Latlon = Latlon {lat :: Angle Double, lon :: Angle Double} deriving Show

latlon :: Location -> Latlon
latlon location = Latlon theta phi
    where 
    Polar r theta phi = polar point
    point = pointTo location geodesic

location :: Latlon -> Location
location (Latlon lat lon) = locate point geodesic
    where
    point = cartesian polar
    polar = Polar (1 *~ earths) lat lon

geodesic :: Icosahedron
geodesic = polyhedron faces

faces :: [Triangle]
faces = [Triangle (vert a) (vert b) (vert c) | (a,b,c) <- indices]
    where 
    vert i = vertices !! i
    indices = [(0,  11, 5 ),
               (0,  5,  1 ),
               (0,  1,  7 ),
               (0,  7,  10),
               (0,  10, 11),
               (1,  5,  9 ),
               (5,  11, 4 ),
               (11, 10, 2 ),
               (10, 7,  6 ),
               (7,  1,  8 ),
               (3,  9,  4 ),
               (3,  4,  2 ),
               (3,  2,  6 ),
               (3,  6,  8 ),
               (3,  8,  9 ),
               (4,  9,  5 ),
               (2,  4,  11),
               (6,  2,  10),
               (8,  6,  7 ),
               (9,  8,  1 )]

vertices :: [Point]
vertices = [Point (x *~ earths) (y *~ earths) (z *~ earths) | (x,y,z) <- raw]
   where raw = [(-1,       golden,    0   ),
                ( 1,       golden,    0   ),
                (-1,      -golden,    0   ),
                ( 1,      -golden,    0   ),
                ( 0,      -1,       golden),
                ( 0,       1,       golden),
                ( 0,      -1,      -golden),
                ( 0,       1,      -golden),
                ( golden,  0,      -1     ),
                ( golden,  0,       1     ),
                (-golden,  0,      -1     ),
                (-golden,  0,       1     )]

golden :: Double
golden = 1.61803398874989484820458

earths :: Unit DLength Double
earths = prefix 6370000 meter