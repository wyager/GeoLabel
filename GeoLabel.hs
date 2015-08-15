module GeoLabel (
    Latlon(..), Location(..), latlon, location, format, parse
) where

import GeoLabel.Strings (parse, format)
import GeoLabel.Unit.Conversion (latlon, location)
import GeoLabel.Unit.Latlon (Latlon(..))
import GeoLabel.Unit.Location(Location)