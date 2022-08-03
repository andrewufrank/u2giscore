-----------------------------------------------------------------------------
--
-- Module      :  Uniform. test the HQ up to the conversion in proto-triples
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
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


module Uniform.HQ_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import UniformBase

-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.GeometryTest (fiveV2)
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
data ObjTess = NodeTag Node | EdgeTag Edge | FaceTag Face
    | HQTag HqType
    | PointTag (Point2d) 
    | LengthTag   Length  | AreaTag Area 
    -- | CostTag Cost 
    -- | NameTag Name
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
-- instance Zeros ObjPoint where zero = ZZpoint

-- | the sum type for the relation names
data MorphTess = 
    -- Stag S | Ttag T | 
    TwinTag Twin| XYtag XY | DistTag Distant
    | CenterTag Center | SurfacedTag Surfaced   
    | HqNodeTag HqNode | HqFaceTag HqFace 
        -- | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        -- | NamedTag
        -- | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
-- instance MorphPoint Zeros where zero = ZZm

-- constants for the tags (some have an argument, some not)
xyMorph :: MorphPoint
xyMorph = XYtag XY 
distanceMorph :: MorphPoint
distanceMorph = DistTag Distant 
sMorph :: MorphPoint
sMorph = Stag S 
tMorph :: MorphPoint
tMorph = Ttag T
twinMorph = TwinTag Twin
scMorph = SCosttag SC
-- tcMorph = TCcosttag TC
namedMorph = NamedTag 
centerMorph = CenterTag 
surfacedMorph = SurfacedTag Surfaced
hqNodeMorph = HqNodeTag HqNode
hqFaceMorph = HqFaceTag HqFace

data XY = XY 
    deriving (Show, Read, Ord, Eq, Generic)
data Distant = Distant
    deriving (Show, Read, Ord, Eq, Generic)
data Named = Named 
    deriving (Show, Read, Ord, Eq, Generic)
data Center = Center 
    deriving (Show, Read, Ord, Eq, Generic)
data Surfaced = Surfaced 
    deriving (Show, Read, Ord, Eq, Generic)
data HqNode = HqNode 
    deriving (Show, Read, Ord, Eq, Generic)
data HqFace = HqFace 
    deriving (Show, Read, Ord, Eq, Generic)

data S = S  deriving (Show, Read, Ord, Eq, Generic)
-- | the start node of an edge
data SC = SC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards s (reverse)
data T = T  deriving (Show, Read, Ord, Eq, Generic)
-- | the end node of an edge 
data Twin = Twin deriving (Show, Read, Ord, Eq, Generic)
-- | the twin HQ
data TC = TC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards t (forward)
data TesselationHQtriples = TesselationHQtriples 
    { _NodesTrip :: [StoreElement]
    , _FacesTrip :: [StoreElement]
    , _HQtrips   :: [StoreElement]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

type StoreTessElement = (ObjTess, MorphTess, ObjTess)

makeTripNode :: Int -> NodeHQ -> StoreElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
makeTripNode  i (NodeHQ v2x) = (NodeTag . Node $ i, xyMorph, PointTag . fromV2toP2d $ v2x)

getAllTrips :: TesselationHQtriples -> [StoreElement]
getAllTrips hqt = concat [_NodesTrip hqt, _FacesTrip hqt, _HQtrips hqt]

makeTripFace :: Int -> FaceHQ -> StoreElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
makeTripFace  i fhq = (FaceTag . Face $ i, xyMorph, PointTag . fromV2toP2d . circumcenter $ fhq)


makeTripHq :: Int -> Int -> HQ -> [StoreElement]
-- convert the HQ data to storeelements
makeTripHq offset i hq = catMaybes [hqnode, hqface, hqtwin, hqhalflength]
    where
        hqid = HQTag . Hq $ i 
        hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreElement
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

type CatStorePoint = CatStore ObjPoint MorphPoint

cat400 :: CatStorePoint
cat400 = catStoreEmpty
-- cat401 :: CatStore ObjPoint MorphPoint
cat401 :: [(ObjPoint, MorphPoint, ObjPoint)] -> CatStore ObjPoint MorphPoint
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