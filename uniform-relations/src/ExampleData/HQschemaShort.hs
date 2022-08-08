-----------------------------------------------------------------------------
--
-- Module      :  ExampleData to test toHq -- this is the schema 
-- copied initially from CatCoreConcept

-- this is the short schema (with sumtype tag is Obj constructor)
-- using IDtype = Int 
-- other value data types are tagged 
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module ExampleData.HQschemaShort where

import Uniform.Point2dData
import Uniform.Point2d
import UniformBase


type IDtype = Int

data ObjTessShort = Node IDtype 
    | Edge IDtype 
    | Face IDtype
    | HalfQuad IDtype             -- HQ is defined in TesselationHQ
    | PointTag Pnt2
    | LengthTag   Length  
    -- | AreaTag Area 
    -- | CostTag Cost 
    -- | NameTag Name
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros ObjTessShort where zero = ZZpoint
instance NiceStrings ObjTessShort
    -- where shownice = showT
    -- would need a shownice for pointTag to propagate the shownice to the inside data 
-- instance NiceStrings Pnv2 where 
--     shownice = showT 

unPointTag :: ObjTessShort -> Pnt2
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unNodeTag - not a Node", showT x]
-- unCostTag :: ObjTessShort -> Cost
-- unCostTag (CostTag t) = t 
-- unCostTag x = errorT ["unCostTag -  not a Cost", showT x]
unLengthTag :: ObjTessShort -> Length
unLengthTag (LengthTag t) = t 
unLengthTag x = errorT ["unLengthTag - not a Length", showT x]

-- p2dToV2 :: Point2d -> V2d 
-- p2dToV2 (Point2d x y) = V2 x y 

-- | the sum type for the relation names
data MorphTessShort = 
    -- Stag S | Ttag T | 
    Twin 
    | XY
    | Dist  
    -- | CenterTag Center | SurfacedTag Surfaced   
    | HqNode  
    | HqFace  
        -- | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        -- | NamedTag
    | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
instance Zeros MorphTessShort  where zero = ZZm
instance NiceStrings MorphTessShort
    -- where shownice = showT  



data Length = Length Double  
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance NiceStrings Length where
--   showNice = showT   



data TessShortHQtriples = TessShortHQtriples 
    { _NodesTrip :: [StoreTessShortElement]
    , _FacesTrip :: [StoreTessShortElement]
    , _HQtrips   :: [StoreTessShortElement]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

type StoreTessShortElement = (ObjTessShort, MorphTessShort, ObjTessShort)


