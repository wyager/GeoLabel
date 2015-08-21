module GeoLabel (
    Latlon(..), latlon, 
    Location(..), location, 
    format, parse,
    toLatlon, fromLatlon,
    toString, fromString, fromString'
) where

import Prelude ()
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Strings (parse, format)
import GeoLabel.Unit.Conversion (latlon, location)
import GeoLabel.Unit.Latlon (Latlon(..))
import GeoLabel.Unit.Location(Location)

toLatlon :: (Double, Double) -> Latlon
toLatlon (lat, lon) = Latlon (lat *~ one) (lon *~ one)

fromLatlon :: Latlon -> (Double, Double)
fromLatlon (Latlon lat lon) = (lat /~ one, lon /~ one)

toString :: (Double, Double) -> String
toString = format . location . toLatlon

fromString :: Monad m => String -> m (Double, Double)
fromString string = fmap (fromLatlon . latlon) (parse string)

fromString' :: String -> (Double, Double)
fromString' string = case fromString string of
    Just coords -> coords
    Nothing -> error "String parse failed"