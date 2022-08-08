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


module ExampleData.HQexampleLong where

-- import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import UniformBase
-- import Uniform.Point2dData
import ExampleData.HQschemaLong

-- import Control.Exception
import Uniform.GeometryFunctions
-- import Uniform.Point2d
import Uniform.TesselationHalfQuads

import Uniform.TripleStore
import Uniform.NaiveTripleStore

-- import Uniform.GeometryTest (fivePnt2d)
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


makeTripNode :: Int -> NodeHQ -> StoreTessLongElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
makeTripNode  i (NodeHQ _ pnt) = (NodeTag . Node $ i, xyMorph
            , PointTag  (Pnt2d (_p2id  pnt) (_v2 pnt)))
-- does not use the nodeIdn which is only for checking
-- data NodeHQ = NodeHQ {nodeIdn :: Int, refId :: Pnt2int} -- was V2d but should contain name of point

getAllTrips :: TessHQtriples -> [StoreTessLongElement]
getAllTrips hqt = concat [_NodesTrip hqt, _FacesTrip hqt, _HQtrips hqt]

makeTripFace :: Int -> FaceHQ -> StoreTessLongElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
makeTripFace  i fhq = (FaceTag . Face $ i, xyMorph, PointTag . fromV2toP2d . circumcenter $ fhq)


makeTripHq :: Int -> Int -> HQdataHQ -> [StoreTessLongElement]
-- convert the HQ data to StoreTessLongElements
makeTripHq offset i hq = catMaybes [hqnode, hqface, hqtwin, hqhalflength]
    where
        hqid = HQTag . Hq $ i 
        hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreTessLongElement
        hqnode = Just $ (hqid, hqNodeMorph, NodeTag . Node   . (+offset) . node $ hq)
        hqface = fmap  (\fi -> (hqid, hqFaceMorph, FaceTag . Face  . (+offset) $ fi)) (face hq)
        hqtwin = Just $ (hqid, twinMorph, HQTag . Hq  . (+offset) . twin $ hq)
        hqhalflength = Just $ (hqid, distanceMorph, LengthTag . Length . halflength $ hq) 


hqToTrip :: Int -> TesselationHQ ->  TessHQtriples
hqToTrip offset teshq  = TessHQtriples
    { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
    , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
    , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQs teshq)
    } 

type CatStoreTessLong = CatStore ObjTessLong MorphTessLong

cat400 :: CatStoreTessLong
cat400 = catStoreEmpty
-- cat401 :: CatStore ObjPoint MorphTessLong
cat401 :: [(StoreTessLongElement)] -> CatStoreTessLong
cat401 ts = catStoreBatch (map wrapIns ts) $ cat400

mainMakeTessLong :: ErrIO () 
mainMakeTessLong = do 
    putIOwords ["\nmainDelaunayTriples TessLong\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    -- tess <- liftIO $ delaunay2 fourPnt2d 
    let tess = delaunay2 fourPnt2d   
    let trips = hqToTrip 400 . toHq1 $ tess 
    -- putIOwords ["triples produces\n", showT trips]
    let res = cat401 (getAllTrips trips) 
    putIOwords ["TessLong triple store  produced\n", shownice res]
    return ()

tess :: CatStore ObjTessLong MorphTessLong
tess = CatStoreK [
    (NodeTag (Node 400),XYtag XY,PointTag (Point2d 0.0 0.0)),
    (NodeTag (Node 401),XYtag XY,PointTag (Point2d 1.5 1.5)),
    (NodeTag (Node 402),XYtag XY,PointTag (Point2d 0.0 2.0)),
    (NodeTag (Node 403),XYtag XY,PointTag (Point2d 2.0 0.0)),
    (FaceTag (Face 400),XYtag XY,PointTag (Point2d 1.0 0.5)),
    (FaceTag (Face 401),XYtag XY,PointTag (Point2d 0.5 1.0)),
    (HQTag (Hq 400),HqNodeTag HqNode,NodeTag (Node 401)),
    (HQTag (Hq 400),TwinTag Twin,HQTag (Hq 405)),
    (HQTag (Hq 400),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 401),HqNodeTag HqNode,NodeTag (Node 400)),
    (HQTag (Hq 401),HqFaceTag HqFace,FaceTag (Face 400)),
    (HQTag (Hq 401),TwinTag Twin,HQTag (Hq 406)),
    (HQTag (Hq 401),DistTag Distant,LengthTag (Length 1.0)),
    (HQTag (Hq 402),HqNodeTag HqNode,NodeTag (Node 400)),
    (HQTag (Hq 402),HqFaceTag HqFace,FaceTag (Face 401)),
    (HQTag (Hq 402),TwinTag Twin,HQTag (Hq 407)),
    (HQTag (Hq 402),DistTag Distant,LengthTag (Length 1.0606601717798212)),
    (HQTag (Hq 403),HqNodeTag HqNode,NodeTag (Node 401)),
    (HQTag (Hq 403),HqFaceTag HqFace,FaceTag (Face 401)),
    (HQTag (Hq 403),TwinTag Twin,HQTag (Hq 408)),
    (HQTag (Hq 403),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 404),HqNodeTag HqNode,NodeTag (Node 400)),
    (HQTag (Hq 404),TwinTag Twin,HQTag (Hq 409)),
    (HQTag (Hq 404),DistTag Distant,LengthTag (Length 1.0)),
    (HQTag (Hq 405),HqNodeTag HqNode,NodeTag (Node 403)),
    (HQTag (Hq 405),HqFaceTag HqFace,FaceTag (Face 400)),
    (HQTag (Hq 405),TwinTag Twin,HQTag (Hq 400)),
    (HQTag (Hq 405),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 406),HqNodeTag HqNode,NodeTag (Node 403)),
    (HQTag (Hq 406),TwinTag Twin,HQTag (Hq 401)),
    (HQTag (Hq 406),DistTag Distant,LengthTag (Length 1.0)),
    (HQTag (Hq 407),HqNodeTag HqNode,NodeTag (Node 401)),
    (HQTag (Hq 407),HqFaceTag HqFace,FaceTag (Face 400)),
    (HQTag (Hq 407),TwinTag Twin,HQTag (Hq 402)),
    (HQTag (Hq 407),DistTag Distant,LengthTag (Length 1.0606601717798212)),
    (HQTag (Hq 408),HqNodeTag HqNode,NodeTag (Node 402)),
    (HQTag (Hq 408),TwinTag Twin,HQTag (Hq 403)),
    (HQTag (Hq 408),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 409),HqNodeTag HqNode,NodeTag (Node 402)),
    (HQTag (Hq 409),HqFaceTag HqFace,FaceTag (Face 401)),
    (HQTag (Hq 409),TwinTag Twin,HQTag (Hq 404)),
    (HQTag (Hq 409),DistTag Distant,LengthTag (Length 1.0))]