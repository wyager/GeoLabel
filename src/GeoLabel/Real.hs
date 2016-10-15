{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GeoLabel.Real (R) where

import System.Random (Random)

import Data.Fixed (Fixed, HasResolution, resolution)

newtype R = R (Double) deriving (Eq, Ord, Fractional, Floating, RealFloat, Num, Real, Random, RealFrac, Show)

--instance Floating R where
    

data E30
instance HasResolution E30 where
    resolution _ = 10^30