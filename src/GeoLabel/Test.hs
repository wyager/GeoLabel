{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import GeoLabel (toString, fromString')
import GeoLabel.Strings (format, parse)
import GeoLabel.Strings.Wordlist (wordlist)
import GeoLabel.Geometry.QuadTree (Subface(A,B,C,D))
import GeoLabel.Geometry.Point (lengthOf, (<->))
import GeoLabel.Geometry.Conversion (cartesian)
import GeoLabel.Geometry.Polar (Polar(..))
import GeoLabel.Geodesic.Earth (earthRadius)
import GeoLabel.Unit.Location (Location(..), idOf, fromId)
import Numeric.Units.Dimensional ((*~), one)
import Numeric.Units.Dimensional.SIUnits (meter)
import System.Exit (exitFailure)
import Test.QuickCheck (quickCheckResult, Result(..), forAll, vector, vectorOf, elements, choose)
import Data.Maybe (fromJust)
import GeoLabel.Real (R)
import Data.Number.BigFloat (BigFloat, Prec50, Epsilon)
import System.Random (Random, randomR, random)
instance Epsilon e => Random (BigFloat e) where
    randomR (a,b) g = (c,g')
        where
        (d,g') = randomR (realToFrac a :: Double, realToFrac b) g
        c = realToFrac d
    random g = (realToFrac (d :: Double), g')
        where (d,g') = random g

main :: IO ()
main = do
    print "testing location -> bits -> location"
    try $ quickCheckResult idTest
    print "testing location -> String -> location"
    try $ quickCheckResult locTest
    print "testing coordinate -> String -> coordinate"
    try $ quickCheckResult coordTest

try :: (IO Result) -> IO ()
try test = do
    result <- test
    case result of
        Failure{..} -> exitFailure
        _ -> return ()

idTest = forAll (choose (0,19)) $ \face ->
    forAll (vectorOf 25 (elements [A,B,C,D])) $ \subfaces ->
        let loc = Location face subfaces in
            loc == (fromJust . fromId . idOf $ loc)

locTest = forAll (choose (0,19)) $ \face ->
    forAll (vectorOf 25 (elements [A,B,C,D])) $ \subfaces ->
        let loc = Location face subfaces in
            loc == (fromJust . parse . format $ loc)

coordTest = forAll (choose (0.0, pi)) $ \lat ->
    forAll (choose (0.0, pi)) $ \lon ->
        acceptableError (lat, lon) (fromString' . toString $ (lat, lon))

acceptableError :: (R, R) -> (R, R) -> Bool
acceptableError (a1, b1) (a2, b2) = difference <= 0.2 *~ meter
    where
    point1 = cartesian (Polar earthRadius (a1 *~ one) (b1 *~ one))
    point2 = cartesian (Polar earthRadius (a2 *~ one) (b2 *~ one))
    difference = lengthOf (point1 <-> point2)

-- Infinite loop?
-- 1.1477102348032477
-- 2.5287052457281303
-- toString (1.1477102348032477, 2.5287052457281303)
-- It's resulting in a lot of kicks
-- It's landing really far away

-- V3 -1615695.0421316223 m 3918971.9410642236 m 582344.6221676043 m

--0.5342809894312989
--2.434338807577105
-- V3 2089283.5979154985 m 1236191.1378369904 m -2840087.204665418 m