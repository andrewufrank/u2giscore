-----------------------------------------------------------------------------
--
-- Module      :  HQschema to test toHq 
-- the schema for handling the geometry of tesselations 

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
{-# LANGUAGE DeriveFunctor  #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module HQschema.HQschemaShort
    (module HQschema.HQschemaShort
    , module Uniform.TripleStore
    ) where

import Uniform.Point2d
import Uniform.TripleStore
-- import Uniform.Point2d
import UniformBase
import Control.Monad.State


type IDtype = Int

data ObjTessShort = Node IDtype 
    | Edge IDtype 
    | Face IDtype
    | HalfQuad IDtype             -- HQ is defined in TesselationHQ
    | PointTag Pnt2
    | LengthTag   LengthD  
    | AreaTag AreaD 
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

unHalfQuad :: ObjTessShort -> IDtype
unHalfQuad (HalfQuad i) = i
unHalfQuad x = errorT ["unHalfQuad - not a HalfQuad", showT x]

unFace :: ObjTessShort -> IDtype
unFace (Face i) = i
unFace x = errorT ["unFace - not a Face", showT x]

unTagPoints2V = unName . unPointTag

unPointTag :: ObjTessShort -> Pnt2
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unPointTag - not a Point", showT x]
-- unCostTag :: ObjTessShort -> Cost
-- unCostTag (CostTag t) = t 
-- unCostTag x = errorT ["unCostTag -  not a Cost", showT x]
unLengthTag :: ObjTessShort -> LengthD
unLengthTag (LengthTag t) = t 
unLengthTag x = errorT ["unLengthTag - not a Length", showT x]

-- p2dToV2 :: Point2d -> V2d 
-- p2dToV2 (Point2d x y) = V2 x y 

-- | the sum type for the relation names
data MorphTessShort = 
    -- Stag S | Ttag T | 
    Twin 
    | XY
    | Incenter   -- where to place a label - incenter
    | Quant Int  -- the quantity with dimension n (0 count, 1 length, 2 area, 3 volume etc. )  

    -- | CenterTag Center | SurfacedTag Surfaced   
    | HqNode  
    | HqFace 
    | NextHq
        -- | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        -- | NamedTag
    | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
    
instance Zeros MorphTessShort  where zero = ZZm
instance NiceStrings MorphTessShort
    -- where shownice = showT  



data Length a = Length a  
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Functor)
instance (Show a) => NiceStrings (Length a) where
--   showNice = showT   
type LengthD = Length Double 

data Area a = Area a  
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Functor)
instance (Show a) => NiceStrings (Area a) where
--   showNice = showT   
type AreaD = Area Double 


data TessShortHQtriples = TessShortHQtriples 
    { _NodesTrip :: [StoreTessShortElement]
    , _FacesTrip :: [StoreTessShortElement]
    , _HQtrips   :: [StoreTessShortElement]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

type StoreTessShortElement = (ObjTessShort, MorphTessShort, ObjTessShort)

type CatStoreTessShort = CatStore ObjTessShort MorphTessShort
type CatStoreState = State  CatStoreTessShort [StoreTessShortElement]
