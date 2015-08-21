module GeoLabel.Polytope.Polyhedron (
    Polyhedron(..)
) where

newtype Polyhedron face = Polyhedron [face] deriving Show
