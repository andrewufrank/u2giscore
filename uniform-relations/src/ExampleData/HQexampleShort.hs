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


module ExampleData.HQexampleShort where

-- import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import UniformBase
-- import Uniform.Point2dData
import ExampleData.HQschemaShort

-- import Control.Exception
-- import Uniform.GeometryFunctions
-- import Uniform.Point2d
import Uniform.TesselationHalfQuads
    ( delaunay2,
      fourV2,
      toHq1,
      FaceHQ(circumcenter),
      HQ(node, face, twin, halflength),
      NodeHQ(..),
      TesselationHQ(_Nodes, _Faces, _HQs) )

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


makeTripNode :: Int -> NodeHQ -> StoreTessShortElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
makeTripNode  i (NodeHQ v2x) = ( Node $ i, XY, toPnv $ v2x)

getAllTrips :: TessShortHQtriples -> [StoreTessShortElement]
getAllTrips hqt = concat [_NodesTrip hqt, _FacesTrip hqt, _HQtrips hqt]

makeTripFace :: Int -> FaceHQ -> StoreTessShortElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
makeTripFace  i fhq = (  Face $ i, XY, PointTag . toPnv . circumcenter $ fhq)


makeTripHq :: Int -> Int -> HQ -> [StoreTessShortElement]
-- convert the HQ data to StoreTessShortElements
makeTripHq offset i hq = catMaybes [hqnode, hqface, hqtwin, hqhalflength]
    where
        hqid =   HalfQuad $ i 
        hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreTessShortElement
        hqnode = Just $ (hqid, HqNode,  Node   . (+offset) . node $ hq)
        hqface = fmap  (\fi -> (hqid, HqFace,   Face  . (+offset) $ fi)) (face hq)
        hqtwin = Just $ (hqid, Twin, HalfQuad  . (+offset) . twin $ hq)
        hqhalflength = Just $ (hqid, Dist, LengthTag . Length . halflength $ hq) 


hqToTrip :: Int -> TesselationHQ ->  TessShortHQtriples
hqToTrip offset teshq  = TessShortHQtriples
    { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
    , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
    , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQs teshq)
    } 

type CatStoreTessShort = CatStore ObjTessShort MorphTessShort

cat400 :: CatStoreTessShort
cat400 = catStoreEmpty
-- cat401 :: CatStore ObjPoint MorphTessShort
cat401 :: [(StoreTessShortElement)] -> CatStoreTessShort
cat401 ts = catStoreBatch (map wrapIns ts) $ cat400

mainMakeTessShort :: ErrIO () 
mainMakeTessShort = do 
    putIOwords ["\nmainDelaunayTriples TessShort\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    tessShort <- liftIO $ delaunay2 fourV2    
    let trips = hqToTrip 400 . toHq1 $ tessShort 
    -- putIOwords ["triples produces\n", showT trips]
    let res = cat401 (getAllTrips trips) 
    putIOwords ["tessShort triple store  produced\n", shownice res]
    return ()

tessShort :: CatStore ObjTessShort MorphTessShort
tessShort = CatStoreK [
        (Node 400,XY,PointTag (Point2d 0.0 0.0)),
        (Node 401,XY,PointTag (Point2d 1.5 1.5)),
        (Node 402,XY,PointTag (Point2d 0.0 2.0)),
        (Node 403,XY,PointTag (Point2d 2.0 0.0)),
        (Face 400,XY,PointTag (Point2d 1.0 0.5)),
        (Face 401,XY,PointTag (Point2d 0.5 1.0)),
        (HalfQuad 400,HqNode,Node 401),
        (HalfQuad 400,Twin,HalfQuad 405),
        (HalfQuad 400,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 401,HqNode,Node 400),
        (HalfQuad 401,HqFace,Face 400),
        (HalfQuad 401,Twin,HalfQuad 406),
        (HalfQuad 401,Dist,LengthTag (Length 1.0)),
        (HalfQuad 402,HqNode,Node 400),
        (HalfQuad 402,HqFace,Face 401),
        (HalfQuad 402,Twin,HalfQuad 407),
        (HalfQuad 402,Dist,LengthTag (Length 1.0606601717798212)),
        (HalfQuad 403,HqNode,Node 401),
        (HalfQuad 403,HqFace,Face 401),
        (HalfQuad 403,Twin,HalfQuad 408),
        (HalfQuad 403,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 404,HqNode,Node 400),
        (HalfQuad 404,Twin,HalfQuad 409),
        (HalfQuad 404,Dist,LengthTag (Length 1.0)),
        (HalfQuad 405,HqNode,Node 403),
        (HalfQuad 405,HqFace,Face 400),
        (HalfQuad 405,Twin,HalfQuad 400),
        (HalfQuad 405,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 406,HqNode,Node 403),
        (HalfQuad 406,Twin,HalfQuad 401),
        (HalfQuad 406,Dist,LengthTag (Length 1.0)),
        (HalfQuad 407,HqNode,Node 401),
        (HalfQuad 407,HqFace,Face 400),
        (HalfQuad 407,Twin,HalfQuad 402),
        (HalfQuad 407,Dist,LengthTag (Length 1.0606601717798212)),
        (HalfQuad 408,HqNode,Node 402),
        (HalfQuad 408,Twin,HalfQuad 403),
        (HalfQuad 408,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 409,HqNode,Node 402),
        (HalfQuad 409,HqFace,Face 401),
        (HalfQuad 409,Twin,HalfQuad 404),
        (HalfQuad 409,Dist,LengthTag (Length 1.0))
    ]