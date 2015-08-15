module GeoLabel.Strings (
    format, parse,
    -- | For testing
    word, bits, locationToBits, bitsToLocation, wordlist
) where

import Prelude hiding (lookup)
import Data.Map (Map, fromList, lookup, (!))
import Data.Bits (testBit)
import Data.List (intercalate)
import GeoLabel.Geometry.QuadTree (Subface(A,B,C,D))
import GeoLabel.Unit.Location (Location(..))
import GeoLabel.Strings.Wordlist (wordlist)

format :: Location -> String
format location = intercalate " " words
    where
    bits = locationToBits location
    sections = take 5 $ map (take 11) . iterate (drop 11) $ bits
    words = map word sections

parse :: Monad m => String -> m Location
parse string = do
    let strings = words string
    bitses <- mapM bits strings
    let bits = concat bitses
    return (bitsToLocation bits)

locationToBits :: Location -> [Bool]
locationToBits (Location face subfaces) = faceBits ++ subBits subfaces
    where
    faceBits = [testBit face n | n <- [0..4]]
    subBits [] = []
    subBits (A:subs') = False : False : subBits subs'
    subBits (B:subs') = False : True  : subBits subs'
    subBits (C:subs') = True  : False : subBits subs'
    subBits (D:subs') = True  : True  : subBits subs'

bitsToLocation :: [Bool] -> Location
bitsToLocation bits = Location (int faceBits) (subfaces subfaceBits)
    where
    faceBits = take 5 bits
    subfaceBits = drop 5 bits
    subfaces (False: False: bits) = A : subfaces bits
    subfaces (False: True : bits) = B : subfaces bits
    subfaces (True : False: bits) = C : subfaces bits
    subfaces (True : True : bits) = A : subfaces bits
    subfaces _ = []

int :: [Bool] -> Int
int [] = 0
int (x:xs) = 2 * int xs + if x then 1 else 0

word :: [Bool] -> String
word bits
    | length bits /= 11 = error "Incorrect bit length"
    | otherwise         = wordmap ! int bits

bits :: Monad m => String -> m [Bool]
bits word = case lookup word indices of
    Nothing -> fail (word ++ " isn't a valid word")
    Just index -> return [testBit index n | n <- [0..10]]

indices :: Map String Int
indices = fromList (zip wordlist [0..])

wordmap :: Map Int String
wordmap = fromList (zip [0..] wordlist)