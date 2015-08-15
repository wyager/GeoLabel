{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import GeoLabel.Strings (format, parse, word, bits, locationToBits, bitsToLocation, wordlist)
import GeoLabel.Strings.Wordlist (wordlist)
import GeoLabel.Geometry.QuadTree (Subface(A,B,C,D))
import GeoLabel.Unit.Location (Location(..))
import System.Exit (exitFailure)
import Test.QuickCheck (quickCheckResult, Result(..), forAll, vector, vectorOf, elements, choose)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    print "Testing bits -> word -> bits"
    try $ quickCheckResult bitTest
    print "Testing word -> bits -> word"
    try $ quickCheckResult strTest
    print "testing location -> bits -> location"
    try $ quickCheckResult ltbTest
    print "testing location -> String -> location"
    try $ quickCheckResult locTest
    

try :: (IO Result) -> IO ()
try test = do
    result <- test
    case result of
        Failure{..} -> exitFailure
        _ -> return ()

bitTest = forAll (vector 11) $ \bitvec ->
    bitvec == (fromJust . bits . word $ bitvec)

strTest = forAll (elements wordlist) $ \w ->
    w == (word . fromJust . bits $ w)

locTest = forAll (choose (0,19)) $ \face ->
    forAll (vectorOf 25 (elements [A,B,C,D])) $ \subfaces ->
        let loc = Location face subfaces in
            loc == (fromJust . parse . format $ loc)

ltbTest = forAll (choose (0,19)) $ \face ->
    forAll (vectorOf 25 (elements [A,B,C,D])) $ \subfaces ->
        let loc = Location face subfaces in
            loc == (bitsToLocation . locationToBits $ loc)