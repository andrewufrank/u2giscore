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
{-# HLINT ignore "Use newtype instead of data" #-}
-- {-# HLINT ignore "Redundant $" #-}
-- {-# HLINT ignore "Use map once" #-}
-- {-# HLINT ignore "Use newtype instead of data" #-}
-- {-# HLINT ignore "Use fewer imports" #-}


module Uniform.TesselationHalfQuads
    ( module Uniform.TesselationHalfQuads
    -- , module Uniform.DelaunayTriples
    -- , module Uniform.DelaunayTriples
    -- , module Uniform.Delaunay
    -- , module Uniform.DelaunayTiles
    , module Uniform.Point2dData
    , module Uniform.Point2d
    , module Linear.V2
    -- , module Control.Lens
    -- , Tesselation  
        )
         where

import UniformBase
import Uniform.Point2d
import Uniform.Point2dData
import Uniform.GeometryFunctions
import qualified Data.Map as Map
-- import Uniform.Delaunay
-- import Uniform.DelaunayTiles
-- import Uniform.DelaunayTriples
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.DelaunayTriangulation.Naive
import Data.Ext
import Data.PlaneGraph
import qualified Data.PlaneGraph as Plane
import qualified Data.Vector as Vec
import Data.Geometry  hiding (zero, head)
import qualified Data.PlanarGraph.Dart as Dart
import qualified Data.Geometry.PlanarSubdivision as Subdiv
-- import GHC.Generics

-- import Delaunay.Types
-- import Delaunay
-- import Voronoi2D
-- import Qhull.Types
-- import qualified Data.Map as Ix
-- import qualified Data.IntSet as IS
-- import qualified Data.IntMap.Strict  as IM
-- import           Data.HashMap.Strict.InsOrd as H hiding (map)
-- import Language.Haskell.TH.Lens (_Overlapping)

-- delaunay2 v2s = delaunay (map v2_dd v2s) False False Nothing 
-- -- ^ calling delaunay with a list of V2

-- voronoi2d tess = voronoi2 tess
-- -- fourV2 = map _v2   fourPnt2d 

-- | a data structure to represent a tesselation (and its dual)
-- with Nodes and Faces (dual to each other)
-- and half of quad edges (see Guibas & stolfi)
data NodeHQ = NodeHQ {nodeIdn :: Int, refId :: Pnt2int} -- was V2d but should contain name of point
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
data FaceHQ = FaceHQ {faceId:: Int} deriving (Show, Read, Ord, Eq, Generic, Zeros)



data HQdataHQ = HQdataHQ
    { dart_node:: Int    -- ^ end of hq (start or end of edge)
    , dart_face:: Int -- ^ face right of the hq, 0 when outerFace
    , dart_twin::Int     -- the other hq for the edge
    , dart_orbitNext :: Int
    , dart_id :: Int
    -- , halflength :: Double  -- the half of the length of the edge
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data TesselationHQ = TesselationHQ {
          _Nodes      :: [NodeHQ]
        , _Faces      :: [FaceHQ]
        , _HQdatas       :: [HQdataHQ]      -- ^ the tileface starting and ending, alternating
        } deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- -- | conversion of the Tesselation data structure from qhull to the HQ form
-- -- all indices are local

tess1 = delaunay2 fourPnt2d
-- to start construction 

hqnodes1 :: Triangulation Int Double  -> [NodeHQ]
-- construct the nodes, with their data 
hqnodes1 tess1 =  zipWith (\hp id' i ->  Pnt2d i (V2 (hp ^. xCoord) (hp ^. yCoord))) (locs1 vdat1) (locs2 vdat1)
    -- where

-- hqfaces1 tess1 = []        
vertices1 = Vec.toList . vertices . toPlaneGraph $ tess1
-- faces1 :: [Int]
faces1 :: [FaceId' s]
faces1 =  map ( fst) . Vec.toList . faces . toPlaneGraph  $ tess1
faces1x :: Triangulation v r -> [Int]
faces1x tess1=  map (_unVertexId . _unFaceId . fst) . Vec.toList . faces . toPlaneGraph  $ tess1
-- cannot get just the index - some game with primal/dual 
-- will be filled after the HQs to compute the incenter
-- faces2 :: [VertexId s 'Dual]
faces2 = map _unFaceId faces1
faces3 :: [Int]
faces3 = map _unVertexId faces2
hqfaces2 tess12 = map FaceHQ (faces1x tess12) -- [0 .. (nf - 1)] 
-- todo will need circumcenter better incenter
    where
        nf :: Int
        nf = numFaces . toPlaneGraph $ tess12

locs1 = map (\e -> fst e  ^. location) vertices1
locs2 = map (\e -> snd e  ^. location) vertices1
vdat1 = map (\e -> snd e  ^. vData) vertices1



tohqdatahq tess1 = map (tohqdataOne pg2) (map toEnum [0 .. (numD - 1)])
        where 
            pg2 = toPlaneGraph tess1 
            numD = Subdiv.numDarts . planarSubdiv2 $ tess1

psub2 = Subdiv.numDarts . planarSubdiv2 $ tess1

tohqdataOne pg2 d = HQdataHQ
    { dart_node =  (_unVertexId  . (`headOf` pg2) $ d)
    , dart_face =  (_unVertexId . _unFaceId  . (`rightFace` pg2) $ d)
    -- always, 0 is outer face?
    , dart_twin = fromEnum . Dart.twin $ d
    , dart_orbitNext =  fromEnum . (`nextEdge` pg2)  $ d
    , dart_id = fromEnum d
    }


tx1 = tohqdatahq tess1

dartsList tess1 = Vec.toList . darts' . toPlaneGraph $ tess1
dartsList2 = dartsList tess1
pg2 = toPlaneGraph tess1

dartNo1 = head dartsList2 
dno1Node =  _unVertexId  . (`headOf` pg2) $ dartNo1

dno1Face = _unVertexId . _unFaceId  . (`leftFace` pg2) $ dartNo1

dno1id = fromEnum dartNo1
dno1twin = Dart.twin dartNo1 

dno1next =  (`nextEdge` pg2)  $ dartNo1

-- dartNodes d = map (`headOf` pg2)  d
-- dartNodes' = map _unVertexId dartNodes
-- dartFaces = map (`leftFace` pg2) dartsList2
-- -- dartFaces' :: [VertexId s 'Dual]
-- dartFaces' =  map _unFaceId dartFaces
-- dartFaces'' =  map _unVertexId dartFaces'

-- dartTwins = map (\d )
thq = toHq1 tess1
-- toHq1 :: Tesselation -> TesselationHQ


toHq1 tess11 = TesselationHQ
    { _Nodes = hqnodes1 tess11
    , _Faces = hqfaces2 tess11
    , _HQdatas = tohqdatahq tess11
    }
--     { _Nodes = map (NodeHQ . dd_v2   . _point)  
--         . IM.elems . _sites $ t
--     , _Faces = map (FaceHQ .   dd_v2 .  _circumcenter .   _simplex ) ts

--     , _HQs = zipWith (\tf i -> 
--             HQ {node =  (!!0) . IM.keys . _vertices'  . _subsimplex $ tf
--                 , face = testSide tf ts  (start tf) (end tf)
--                 , twin =  tfCount + i   
--                 , halflength = (/2) . _volume' . _subsimplex $ tf
--             }  ) tfs [0..]
--             ++ zipWith (\tf i -> 
--             HQ {node =  (!!1) . IM.keys . _vertices'  . _subsimplex $ tf
--                 , face = testSide tf ts  (end tf) (start tf) 
--                 , twin =    i   
--                 , halflength = (/2) . _volume' . _subsimplex $ tf
--             } ) tfs [0..]
--     }
--         where 
--             tfs :: [TileFacet]
--             tfs =  IM.elems . _tilefacets $ t
--             tfCount = length tfs 
--             ts :: [Tile]
--             ts = IM.elems . _tiles $ t
--             start tf =  (!!0) . vertices1x $ tf
--             end tf =  (!!1) . vertices1x $ tf
--             vertices1x :: TileFacet -> [([Double])]
--             vertices1x tf = IM.elems . _vertices' . _subsimplex $ tf 

-- test for each face mentioned in a tilefacet on which side it is 
-- true - startHQ,
-- for second hq swtich start and  end
-- returns the tile to the right of the hq (in the direction of the hq)
-- Nothing if none 

-- testSide :: TileFacet -> [Tile] -> [Double] -> [Double]->  Maybe Int 
-- testSide tft tiles startxy endxy = listToMaybe . catMaybes $ res
--             -- assumes that center is on one (but noth both) sides
--             -- possible numerical issue
--     where 
--         facetofs1 :: [Int] -- the list of faces
--         facetofs1 =    IS.elems . _facetOf  $ tft
--         centerxy i = _circumcenter . _simplex . (!! i)  $ tiles 
--         centers = map centerxy facetofs1
--         -- vertices1 ::  [(IM.Key, [Double])]
--         -- vertices1 =   IM.assocs .  _vertices' .  _subsimplex $ tft
--         -- (startid, startxy) =  (!!0)  vertices1 
--         -- (endid, endxy) =  vertices1 !! 1
--         ccws = map  (ccw_test startxy endxy) centers
--             -- test the center to determine which side of face 
--         res = zipWith (\b i -> if b then Just i else Nothing) ccws facetofs1



mainHQ :: ErrIO ()
mainHQ = do
    putIOwords ["the conversion to a tesselation As Half-Quads"]
    -- tess4 <- liftIO $ delaunay (map (todd ) fourPnt2d) False False Nothing
    -- putIOwords ["the given tesselation", showT tess4]
    -- putIOwords ["point2d two\n", showT (toHq1 tess4), "\n"]
    -- putIOwords ["point2d two", showT tess4, "\n"]

