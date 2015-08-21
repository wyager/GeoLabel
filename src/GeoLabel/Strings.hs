module GeoLabel.Strings (
    format, parse,
) where

import Prelude hiding (lookup)
import Data.Map (Map, fromList, lookup, (!))
import Data.Bits (testBit)
import Data.List (intercalate)
import GeoLabel.Geometry.QuadTree (Subface(A,B,C,D))
import GeoLabel.Unit.Location (Location(..), fromId, idOf)
import GeoLabel.Strings.Wordlist (wordlist)
import Data.Bits ((.&.), shiftL, shiftR)
import Control.Monad (forM)
import Data.Word (Word64)

format :: Location -> String
format location = intercalate " " words
    where
    id = idOf location
    words = [wordmap ! ((id `shiftR` n) .&. 0x7FF) | n <- [44, 33, 22, 11, 0]]

parse :: Monad m => String -> m Location
parse string = do
    nums <- forM (words string) $ \word -> case lookup word indices of
        Nothing -> fail ("Invalid word: " ++ word)
        Just number -> return number
    let id = foldl (\sum num -> num + sum `shiftL` 11) 0 nums
    fromId id

indices :: Map String Word64
indices = fromList (zip wordlist [0..])

wordmap :: Map Word64 String
wordmap = fromList (zip [0..] wordlist)