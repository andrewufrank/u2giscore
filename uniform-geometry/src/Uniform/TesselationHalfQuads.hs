-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Delaunay

-- collect the data to store in triple storage
-- the data is not yet typed, in order no to introduce the triple store types here
-- but every relation is a single list of pairs.
-- the ids are types (N for NodeID, )
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.TesselationHalfQuads
    ( module Uniform.TesselationHalfQuads
    , module Uniform.DelaunayTriples
    , module Uniform.DelaunayTriples
    , module Uniform.Delaunay
    , module Uniform.DelaunayTiles
    , module Uniform.Point2dData
    , module Uniform.Point2d
    , module Linear.V2
    , module Control.Lens
    , Tesselation  
        ) 
         where

import UniformBase
import Uniform.Point2d
import Uniform.Point2dData
import Uniform.GeometryFunctions
import qualified Data.Map as Map 
import Uniform.Delaunay
import Uniform.DelaunayTiles
import Uniform.DelaunayTriples
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics
  

import Delaunay.Types
import Delaunay
import Qhull.Types
-- import qualified Data.Map as Ix
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict  as IM
import           Data.HashMap.Strict.InsOrd as H hiding (map)
import Language.Haskell.TH.Lens (_Overlapping)


mainHQ :: ErrIO ()
mainHQ = do 
    putIOwords ["the conversion to a tesselation As Half-Quads"]
    putIOwords ["the given tesselation", showT tess]

{- 
tess :: Tesselation
tess = read $ Tesselation {_sites = fromList [(0,Site {_point = [0.0,0.0], _neighsitesIds = fromList [1,2,3], _neighfacetsIds = fromList [1,2,4], _neightilesIds = fromList [0,1]}),(1,Site {_point = [1.5,1.5], _neighsitesIds = fromList [0,2,3], _neighfacetsIds = fromList [0,2,3], _neightilesIds = fromList [0,1]}),(2,Site {_point = [0.0,2.0], _neighsitesIds = fromList [0,1], _neighfacetsIds = fromList [3,4], _neightilesIds = fromList [1]}),(3,Site {_point = [2.0,0.0], _neighsitesIds = fromList [0,1], _neighfacetsIds = fromList [0,1], _neightilesIds = fromList [0]})], _tiles = fromList [(0,Tile {_simplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5]),(3,[2.0,0.0])], _circumcenter = [1.0,0.5], _circumradius = 1.118033988749895, _volume' = 1.5}, _neighborsIds = fromList [1], _facetsIds = fromList [0,1,2], _family' = None, _toporiented = False}),(1,Tile {_simplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5]),(2,[0.0,2.0])], _circumcenter = [0.5,1.0], _circumradius = 1.118033988749895, _volume' = 1.5}, _neighborsIds = fromList [0], _facetsIds = fromList [2,3,4], _family' = None, _toporiented = True})], _tilefacets = fromList [(0,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(1,[1.5,1.5]),(3,[2.0,0.0])], _circumcenter = [1.75,0.75], _circumradius = 0.7905694150420949, _volume' = 1.5811388300841898}, _facetOf = fromList [0], _normal' = [0.9486832980505138,0.31622776601683794], _offset' = 1.8973665961010275}),(1,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(3,[2.0,0.0])], _circumcenter = [1.0,0.0], _circumradius = 1.0, _volume' = 2.0}, _facetOf = fromList [0], _normal' = [0.0,-1.0], _offset' = 0.0}),(2,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5])], _circumcenter = [0.75,0.75], _circumradius = 1.0606601717798212, _volume' = 2.1213203435596424}, _facetOf = fromList [0,1], _normal' = [0.7071067811865476,-0.7071067811865476], _offset' = 0.0}),(3,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(1,[1.5,1.5]),(2,[0.0,2.0])], _circumcenter = [0.75,1.75], _circumradius = 0.7905694150420949, _volume' = 1.5811388300841898}, _facetOf = fromList [1], _normal' = [0.31622776601683794,0.9486832980505138], _offset' = -1.8973665961010275}),(4,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(2,[0.0,2.0])], _circumcenter = [0.0,1.0], _circumradius = 1.0, _volume' = 2.0}, _facetOf = fromList [1], _normal' = [-1.0,0.0], _offset' = 0.0})], _edges' = fromList [(Pair 0 1,([0.0,0.0],[1.5,1.5])),(Pair 0 2,([0.0,0.0],[0.0,2.0])),(Pair 0 3,([0.0,0.0],[2.0,0.0])),(Pair 1 2,([1.5,1.5],[0.0,2.0])),(Pair 1 3,([1.5,1.5],[2.0,0.0]))]} 
-}