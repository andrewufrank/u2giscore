-----------------------------------------------------------------------------
--
-- Module      :  ExampleData to test toHq -- this is the schema 
-- copied initially from CatCoreConcept
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
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module ExampleData.HQschemaShort where

-- import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import UniformBase

-- import Control.Exception
-- import Uniform.GeometryFunctions
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
data ObjTess = NodeTag Node 
    | EdgeTag Edge 
    | FaceTag Face
    | HQTag Hq
    | PointTag Point2d 
    | LengthTag   Length  
    -- | AreaTag Area 
    -- | CostTag Cost 
    -- | NameTag Name
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ObjTess where zero = ZZpoint

-- | the sum type for the relation names
data MorphTess = 
    -- Stag S | Ttag T | 
    TwinTag Twin
    | XYtag XY 
    | DistTag Distant
    -- | CenterTag Center | SurfacedTag Surfaced   
    | HqNodeTag HqNode 
    | HqFaceTag HqFace 
        -- | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        -- | NamedTag
    | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
instance Zeros MorphTess  where zero = ZZm

-- data NodeType i =  Node i deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Node  where zero = Node 0
-- data EdgeType c = Edge c deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Edge where zero = Edge 0

-- data HqType = Hq Int  
--     deriving (Show, Read, Ord, Eq, Generic)
-- the obj for the half-quad edge, derived by the two node ids from hgeometry, but just a simple int as id
instance Zeros Hq where zero = Hq 0

-- data FaceType = Face Int  
    -- deriving (Show, Read, Ord, Eq, Generic)
-- the id for the half-quad edge, derived by the two node ids from hgeometry, but just a simple int as id
instance Zeros Face where zero = Face 0


-- type NodeID = Text

newtype Node = Node  Int
    deriving (Show, Read, Ord, Eq, Generic)
newtype Edge = Edge Int
    deriving (Show, Read, Ord, Eq, Generic)

newtype Face = Face Int
    deriving (Show, Read, Ord, Eq, Generic)

newtype Hq = Hq Int 
    deriving (Show, Read, Ord, Eq, Generic)


data Length = Length Double  
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data Point2d = Point2d Double Double 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- | to replace Point2, the data type to represent all 2 coordinate pairs 
instance NiceStrings Point2d where shownice = showT 

fromV2toP2d :: V2d -> Point2d 
fromV2toP2d (V2 x y) = Point2d x y 


-- constants for the tags (some have an argument, some not)
xyMorph :: MorphTess
xyMorph = XYtag XY 
-- distanceMorph :: MorphTess
distanceMorph = DistTag Distant 
-- sMorph :: MorphTess
-- -- sMorph = Stag S 
-- tMorph :: MorphTess
-- tMorph = Ttag T
twinMorph = TwinTag Twin
-- scMorph = SCosttag SC
-- tcMorph = TCcosttag TC
-- namedMorph = NamedTag 
-- centerMorph = CenterTag 
-- surfacedMorph = SurfacedTag Surfaced
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

-- data S = S  deriving (Show, Read, Ord, Eq, Generic)
-- | the start node of an edge
-- data SC = SC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards s (reverse)
-- data T = T  deriving (Show, Read, Ord, Eq, Generic)
-- | the end node of an edge 
data Twin = Twin deriving (Show, Read, Ord, Eq, Generic)
-- | the twin HQ
-- data TC = TC  deriving (Show, Read, Ord, Eq, Generic)
-- | the cost of the edge in direction towards t (forward)

data TessHQtriples = TessHQtriples 
    { _NodesTrip :: [StoreTessElement]
    , _FacesTrip :: [StoreTessElement]
    , _HQtrips   :: [StoreTessElement]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

type StoreTessElement = (ObjTess, MorphTess, ObjTess)


