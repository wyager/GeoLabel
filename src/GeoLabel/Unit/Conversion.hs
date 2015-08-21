module GeoLabel.Unit.Conversion (
    latlon,
    location,
) where

import Prelude () -- Don't import anything from standard prelude
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Geometry.Polar (Polar(..))
import GeoLabel.Geometry.Conversion (polar, cartesian)
import GeoLabel.Geodesic.Grid (pointOf, locationOf)
import GeoLabel.Geodesic.Earth (earth, radii)
import GeoLabel.Unit.Location (Location(..))
import GeoLabel.Unit.Latlon (Latlon(..))

latlon :: Location -> Latlon
latlon location = Latlon theta phi
    where 
    Polar r theta phi = polar point
    point = pointOf location earth

location :: Latlon -> Location
location (Latlon lat lon) = locationOf point earth
    where
    point = cartesian polar
    polar = Polar (1 *~ radii) lat lon
