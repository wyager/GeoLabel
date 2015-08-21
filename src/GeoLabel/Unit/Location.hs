module GeoLabel.Unit.Location (
    Location(..), idOf, fromId
) where

import GeoLabel.Geometry.QuadTree (Subface(A,B,C,D))
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)
import Foreign.Ptr (Ptr, castPtr)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word64)

data Location = Location Int [Subface] deriving (Show, Eq, Ord)

idOf :: Location -> Word64
idOf (Location face subfaces) = faceId .|. subfacesId
    where
    faceId = (fromIntegral face) `shiftL` 50
    subfacesId = foldl (\sum sub -> numOf sub + sum * 4) 0 subfaces
    numOf A = 0
    numOf B = 1
    numOf C = 2
    numOf D = 3

fromId :: Monad m => Word64 -> m Location
fromId id = if face >= 20
    then fail "Invalid location ID"
    else return (Location (fromIntegral face) subs)
    where
    face = id `shiftR` 50
    subs = [fromNum ((id `shiftR` n) .&. 3) | n <- [48, 46 .. 0]]
    fromNum 0 = A
    fromNum 1 = B
    fromNum 2 = C
    fromNum 3 = D

instance Storable Location where
    sizeOf loc = 8
    alignment loc = 8
    peek locptr = peek (castPtr locptr :: Ptr Word64) >>= fromId
    poke locptr loc = poke (castPtr locptr :: Ptr Word64) (idOf loc)