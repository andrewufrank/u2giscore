-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Delaunay

-- collect the data to store in triple storage
-- the data is not yet typed, in order no to introduce the triple store types here
-- but every relation is a single list of pairs.
-- the ids are not typed ... ? 
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
{-# HLINT ignore "Use fewer imports" #-}


module Uniform.TesselationHalfQuads
    ( module Uniform.TesselationHalfQuads
    -- , module Uniform.Point2d
    -- , module Uniform.Point2d
    -- , module Linear.V2
    -- , module Control.Lens
        )
         where

import UniformBase
import Uniform.Point2d
-- import Uniform.Point2d
import Uniform.GeometryFunctions
-- import qualified Data.Map as Map
-- import Linear.V2
-- import qualified Linear.Vector as Lin
import Control.Lens
import Algorithms.Geometry.DelaunayTriangulation.Types
-- import Algorithms.Geometry.DelaunayTriangulation.Naive
-- import Data.Ext
import Data.PlaneGraph
import qualified Data.PlaneGraph as Plane
import qualified Data.Vector as Vec
import Data.Geometry  hiding (zero, head)
import qualified Data.PlanarGraph.Dart as Dart
import qualified Data.Geometry.PlanarSubdivision as Subdiv
-- import Data.Vector.Fusion.Stream.Monadic (zipWith3M)

-- | a data structure to represent a tesselation (and its dual)
-- with Nodes and Faces (dual to each other)
-- and half of quad edges (see Guibas & stolfi)
data NodeHQ = NodeHQ {nodeIdn :: Int, refId :: Pnt2} -- was V2d but should contain name of point
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
data FaceHQ = FaceHQ {faceIdn:: Int} deriving (Show, Read, Ord, Eq, Generic, Zeros)

data HQdataHQ = HQdataHQ
    { hq_node:: Int    -- ^ end of hq (start or end of edge)
    , hq_other :: Int  -- ^ the other end of the hq
    , hq_face:: Int -- ^ face right of the hq, 0 when outerFace
    , hq_twin::Int     -- the other hq for the edge
    , hq_orbitNext :: Int
    , hq_id :: Int
    -- , halflength :: Double  -- the half of the length of the edge
    }  deriving (Show, Read, Ord, Eq, Generic, Zeros)

data TesselationHQ = TesselationHQ {
          _Nodes      :: [NodeHQ]
        , _Faces      :: [FaceHQ]
        , _HQdatas       :: [HQdataHQ]       
        } deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- -- | conversion of the Tesselation data structure from qhull to the HQ form
-- -- all indices are local

instance NiceStrings TesselationHQ where 
    showlong (TesselationHQ n f hq) = 
            "TesselationHQ {" <> showAsLines n 
                    <> showAsLines f 
                    <> showAsLines hq <> "}"

hqnodes1 :: Triangulation Int Double  -> [NodeHQ]
-- construct the nodes, with their data 
hqnodes1 tess11 =  zipWith3 (\id' hp  i -> NodeHQ id' (Pnt2d i (V2 (hp ^. xCoord) (hp ^. yCoord)))) (locs11 tess11)  (locs22 tess11)  (vdat11 tess11)
    where
        -- locs11 ::[Int]
        locs11 t = map (_unVertexId . fst) . vertices11 $ t
        -- locs22 :: [Point2 Double]
        locs22 = map (\e -> snd e  ^. location) . vertices11
        vdat11 = map (\e -> snd e  ^. vData) . vertices11
        vertices11 tess11 = Vec.toList . vertices . toPlaneGraph $ tess11

hqfaces2 tess12 = map FaceHQ (faces1x tess12) -- [0 .. (nf - 1)] 
-- todo will need circumcenter and incenter
--  done after the relations are stored 
    where
        -- nf :: Int
        -- nf = numFaces . toPlaneGraph $ tess12
        faces1x :: Triangulation v r -> [Int]
        faces1x tess13 =  map (_unVertexId . _unFaceId . fst) . Vec.toList . faces . toPlaneGraph  $ tess13 

tohqdatahq tess14 = map (tohqdataOne pg2) (map toEnum [0 .. (numD - 1)])
        where 
            pg2 = toPlaneGraph tess14 
            numD = Subdiv.numDarts . planarSubdiv2 $ tess14

tohqdataOne pg2 d = HQdataHQ
    { hq_node =  (_unVertexId  . (`headOf` pg2) $ d)
    , hq_other =  (_unVertexId  . (`tailOf` pg2) $ d)   -- not used 
    , hq_face =  (_unVertexId . _unFaceId  . (`rightFace` pg2) $ d)
    -- outer face is included, but not always id 0 
    , hq_twin = fromEnum . Dart.twin $ d
    , hq_orbitNext =  fromEnum . (`nextEdge` pg2)  $ d
    , hq_id = fromEnum d
    }
 

toHq1 tess11 = TesselationHQ
    { _Nodes = hqnodes1 tess11
    , _Faces = hqfaces2 tess11
    , _HQdatas = tohqdatahq tess11
    }

showAsLines :: Show a => [a] -> Text 
showAsLines = unlines' . map showT 
-- should include the syntax (, and [])





 