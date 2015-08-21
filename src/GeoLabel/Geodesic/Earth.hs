module GeoLabel.Geodesic.Earth (
    earth, radii, earthRadius
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Geometry.Point (Point, V3(..))
import GeoLabel.Polytope.Triangle (Triangle(..))
import GeoLabel.Polytope.Polyhedron (Polyhedron(..))

earth :: Polyhedron Triangle
earth = Polyhedron faces

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
vertices = [V3 (x *~ radii) (y *~ radii) (z *~ radii) | (x,y,z) <- raw]
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

radii :: Unit DLength Double
radii = prefix 6.371 (mega meter)

earthRadius :: Length Double
earthRadius = 1 *~ radii