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
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use fewer imports" #-}


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
  
import Uniform.Point2d
import Delaunay.Types
import Delaunay
import Qhull.Types
-- import qualified Data.Map as Ix
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict  as IM
import           Data.HashMap.Strict.InsOrd as H hiding (map)
import Language.Haskell.TH.Lens (_Overlapping)
-- import Uniform.TesselationHalfQuads (TesselationHQ(_HQs))

{- Original 
        data Tesselation = Tesselation {
            _sites      :: IndexMap Site
        , _tiles      :: IntMap Tile
        , _tilefacets :: IntMap TileFacet
        , _edges'     :: EdgeMap
} deriving Show
-}
data NodeHQ = NodeHQ V2d
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
data FaceHQ = FaceHQ {circumcenter ::V2d} deriving (Show, Read, Ord, Eq, Generic, Zeros)
data HQ = HQ 
    { node:: Int
    , face::Maybe Int
    , twin::Int
    , halflength :: Double}
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data TesselationHQ = TesselationHQ {
          _Nodes      :: [NodeHQ]
        , _Faces      :: [FaceHQ]
        , _HQsA        :: [HQ]      -- ^ the tileface starting 
        -- , _HQsB        :: [HQ]      -- ^ the tileface ending
        
        -- , _edges'     :: EdgeMap
        } deriving Show

toHq1 :: Tesselation -> TesselationHQ
toHq1 t = TesselationHQ 
    { _Nodes = map (NodeHQ . toV2 . _point)  
        . IM.elems . _sites $ t
    , _Faces = map (FaceHQ .   toV2 .  _circumcenter .   _simplex ) ts
           
    , _HQsA = zipWith (\tf i -> 
            HQ {node =  (!!0) . IM.keys . _vertices'  . _subsimplex $ tf
                , face = testSide tf ts  (start tf) (end tf)
                , twin =  tfCount + i   
                , halflength = (/2) . _volume' . _subsimplex $ tf
            }  ) tfs [0..]
            ++ zipWith (\tf i -> 
            HQ {node =  (!!1) . IM.keys . _vertices'  . _subsimplex $ tf
                , face = Nothing
                , twin =    i   
                , halflength = (/2) . _volume' . _subsimplex $ tf
            }
            -- HQ 1 2 3 0.0
                    ) tfs [0..]

    }
        where 
            tfs :: [TileFacet]
            tfs =  IM.elems . _tilefacets $ t
            tfCount = length tfs 
            ts :: [Tile]
            ts = IM.elems . _tiles $ t
            start tf =  (!!0) . vertices1x $ tf
            end tf =  (!!1) . vertices1x $ tf
            vertices1x :: TileFacet -> [([Double])]
            vertices1x tf = IM.elems . _vertices' . _subsimplex $ tf 

-- test for each face mentioned in a tilefacet on which side it is 
-- true - startHQ,
-- for second hq swtich star and  
testSide :: TileFacet -> [Tile] -> [Double] -> [Double]->  Maybe Int 
testSide tft tiles startxy endxy = listToMaybe . catMaybes $ res
    where 
        facetofs1 :: [Int] -- the list of faces
        facetofs1 =    IS.elems . _facetOf  $ tft
        centerxy i = _circumcenter . _simplex . (!! i)  $ tiles 
        centers = map centerxy facetofs1
        -- vertices1 ::  [(IM.Key, [Double])]
        -- vertices1 =   IM.assocs .  _vertices' .  _subsimplex $ tft
        -- (startid, startxy) =  (!!0)  vertices1 
        -- (endid, endxy) =  vertices1 !! 1
        ccws = map  (ccw_test startxy endxy) centers
            -- test the center to determine which side of face 
        res = zipWith (\b i -> if b then Just i else Nothing) ccws facetofs1



mainHQ :: ErrIO ()
mainHQ = do 
    putIOwords ["the conversion to a tesselation As Half-Quads"]
    tess4 <- liftIO $ delaunay (map (v2toList2 . p2toV2) $ fourP2) False False Nothing
    putIOwords ["the given tesselation", showT tess4]
    putIOwords ["point2d two\n", showT (toHq1 tess4), "\n"]
    -- putIOwords ["point2d two", showT tess4, "\n"]

{- 
tess :: Tesselation
tess = read $ Tesselation {_sites = fromList [(0,Site {_point = [0.0,0.0], _neighsitesIds = fromList [1,2,3], _neighfacetsIds = fromList [1,2,4], _neightilesIds = fromList [0,1]}),(1,Site {_point = [1.5,1.5], _neighsitesIds = fromList [0,2,3], _neighfacetsIds = fromList [0,2,3], _neightilesIds = fromList [0,1]}),(2,Site {_point = [0.0,2.0], _neighsitesIds = fromList [0,1], _neighfacetsIds = fromList [3,4], _neightilesIds = fromList [1]}),(3,Site {_point = [2.0,0.0], _neighsitesIds = fromList [0,1], _neighfacetsIds = fromList [0,1], _neightilesIds = fromList [0]})], _tiles = fromList [(0,Tile {_simplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5]),(3,[2.0,0.0])], _circumcenter = [1.0,0.5], _circumradius = 1.118033988749895, _volume' = 1.5}, _neighborsIds = fromList [1], _facetsIds = fromList [0,1,2], _family' = None, _toporiented = False}),(1,Tile {_simplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5]),(2,[0.0,2.0])], _circumcenter = [0.5,1.0], _circumradius = 1.118033988749895, _volume' = 1.5}, _neighborsIds = fromList [0], _facetsIds = fromList [2,3,4], _family' = None, _toporiented = True})], _tilefacets = fromList [(0,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(1,[1.5,1.5]),(3,[2.0,0.0])], _circumcenter = [1.75,0.75], _circumradius = 0.7905694150420949, _volume' = 1.5811388300841898}, _facetOf = fromList [0], _normal' = [0.9486832980505138,0.31622776601683794], _offset' = 1.8973665961010275}),(1,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(3,[2.0,0.0])], _circumcenter = [1.0,0.0], _circumradius = 1.0, _volume' = 2.0}, _facetOf = fromList [0], _normal' = [0.0,-1.0], _offset' = 0.0}),(2,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5])], _circumcenter = [0.75,0.75], _circumradius = 1.0606601717798212, _volume' = 2.1213203435596424}, _facetOf = fromList [0,1], _normal' = [0.7071067811865476,-0.7071067811865476], _offset' = 0.0}),(3,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(1,[1.5,1.5]),(2,[0.0,2.0])], _circumcenter = [0.75,1.75], _circumradius = 0.7905694150420949, _volume' = 1.5811388300841898}, _facetOf = fromList [1], _normal' = [0.31622776601683794,0.9486832980505138], _offset' = -1.8973665961010275}),(4,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(2,[0.0,2.0])], _circumcenter = [0.0,1.0], _circumradius = 1.0, _volume' = 2.0}, _facetOf = fromList [1], _normal' = [-1.0,0.0], _offset' = 0.0})], _edges' = fromList [(Pair 0 1,([0.0,0.0],[1.5,1.5])),(Pair 0 2,([0.0,0.0],[0.0,2.0])),(Pair 0 3,([0.0,0.0],[2.0,0.0])),(Pair 1 2,([1.5,1.5],[0.0,2.0])),(Pair 1 3,([1.5,1.5],[2.0,0.0]))]} 
-}