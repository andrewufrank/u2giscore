-----------------------------------------------------------------------------
--
-- Module      :  Uniform. test the HQ up to the conversion in proto-triples
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
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

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module ExampleData.HQexample where

-- import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import UniformBase
import Uniform.Point2dData
import ExampleData.HQschema

-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.Point2d
import Uniform.TesselationHalfQuads

import Uniform.TripleStore
import Uniform.NaiveTripleStore

-- import Uniform.GeometryTest (fiveV2)
-- import qualified Data.Geometry.Point as HP 

{- 
data NodeHQ = NodeHQ V2d
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
data FaceHQ = FaceHQ {circumcenter ::V2d} deriving (Show, Read, Ord, Eq, Generic, Zeros)

data HQ = HQ 
    { node:: Int    -- ^ end of hq (start or end of edge)
    , face::Maybe Int -- ^ face right of the hq
    , twin::Int     -- the other hq for the edge
    , halflength :: Double  -- the half of the length of the edge
    } 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data TesselationHQ = TesselationHQ {
          _Nodes      :: [NodeHQ]
        , _Faces      :: [FaceHQ]
        , _HQs       :: [HQ]      -- ^ the tileface starting and ending, alternating
        } deriving Show

-}


makeTripNode :: Int -> NodeHQ -> StoreTessElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
makeTripNode  i (NodeHQ v2x) = (NodeTag . Node $ i, xyMorph, PointTag . fromV2toP2d $ v2x)

getAllTrips :: TesselationHQtriples -> [StoreTessElement]
getAllTrips hqt = concat [_NodesTrip hqt, _FacesTrip hqt, _HQtrips hqt]

makeTripFace :: Int -> FaceHQ -> StoreTessElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
makeTripFace  i fhq = (FaceTag . Face $ i, xyMorph, PointTag . fromV2toP2d . circumcenter $ fhq)


makeTripHq :: Int -> Int -> HQ -> [StoreTessElement]
-- convert the HQ data to StoreTessElements
makeTripHq offset i hq = catMaybes [hqnode, hqface, hqtwin, hqhalflength]
    where
        hqid = HQTag . Hq $ i 
        hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreTessElement
        hqnode = Just $ (hqid, hqNodeMorph, NodeTag . Node   . (+offset) . node $ hq)
        hqface = fmap  (\fi -> (hqid, hqFaceMorph, FaceTag . Face  . (+offset) $ fi)) (face hq)
        hqtwin = Just $ (hqid, twinMorph, HQTag . Hq  . (+offset) . twin $ hq)
        hqhalflength = Just $ (hqid, distanceMorph, LengthTag . Length . halflength $ hq) 


hqToTrip :: Int -> TesselationHQ ->  TesselationHQtriples
hqToTrip offset teshq  = TesselationHQtriples
    { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
    , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
    , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQs teshq)
    } 

type CatStoreTess = CatStore ObjTess MorphTess

cat400 :: CatStoreTess
cat400 = catStoreEmpty
-- cat401 :: CatStore ObjPoint MorphTess
cat401 :: [(StoreTessElement)] -> CatStoreTess
cat401 ts = catStoreBatch (map wrapIns ts) $ cat400

mainMakeTess :: ErrIO () 
mainMakeTess = do 
    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    tess <- liftIO $ delaunay2 fourV2    
    let trips = hqToTrip 400 . toHq1 $ tess 
    -- putIOwords ["triples produces\n", showT trips]
    let res = cat401 (getAllTrips trips) 
    putIOwords ["triple store  produced\n", shownice res]
    return ()