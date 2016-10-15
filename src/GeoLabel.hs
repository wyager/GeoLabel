module GeoLabel (
    toString, fromString, fromString'
) where

import Prelude ()
import Numeric.Units.Dimensional.Prelude
import GeoLabel.Strings (parse, format)
import GeoLabel.Unit.Conversion (latlon, location)
import GeoLabel.Unit.Latlon (Latlon(..))
import GeoLabel.Unit.Location(Location)
import GeoLabel.Real (R)


toString :: (R, R) -> String
toString = format . location . toLatlon
    where toLatlon (lat,lon) = Latlon (lat *~ one) (lon *~ one)

fromString :: Monad m => String -> m (R, R)
fromString = fmap (fromLatlon . latlon) . parse
    where fromLatlon (Latlon lat lon) = (lat /~ one, lon /~ one)

fromString' :: String -> (R, R)
fromString' string = case fromString string of
    Just coords -> coords
    Nothing -> error "String parse failed"