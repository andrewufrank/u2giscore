-----------------------------------------------------------------------------
--
-- Module      :  The definitions to include to use the 
--                  Country schema
--                  the top which exports all schema
--
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
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor  #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module ExampleHQ.Store
    -- (
    --     module Country.Store
    -- , module Country.Schema
    -- -- , module Country.Types
    -- -- , module Uniform.TripleStore
    -- -- , module Uniform.Rels2
    -- ) 
    where

-- import Uniform.Point2d
-- import Uniform.TripleStore
-- import Uniform.Rels2
-- -- import Uniform.Point2d
import UniformBase
import ExampleHQ.Schema
import Uniform.SchemaFoundation  

-- import Control.Monad.State
-- import Country.HQtypes  -- data types defined for HQ 
-- import Country.Schema  -- data types defined for HQ 



    
instance Zeros ObjCountry where zero = ZZpoint

instance NiceStrings ObjCountry
    -- where shownice = showT
    -- would need a shownice for pointTag to propagate the shownice to the inside data 
-- instance NiceStrings Pnv2 where 
--     shownice = showT 


instance ObjectsHQ ObjCountry where 
    nodeObj = Node 
    edgeObj = Edge
    faceObj = Face
    halfQuadObj = HalfQuad 
    pointTag = PointTag
    -- unHalfQuad :: ObjCountry -> IDtype
    unHalfQuad (HalfQuad i) = i
    unHalfQuad x = errorT ["unHalfQuad - not a HalfQuad", showT x]

    -- unFace :: ObjCountry -> IDtype
    unFace (Face i) = i
    unFace x = errorT ["unFace - not a Face", showT x]


    -- unPointTag :: ObjCountry -> Pnt2
    unPointTag (PointTag t) = t 
    unPointTag x = errorT ["unPointTag - not a Point", showT x]
    -- unCostTag :: ObjCountry -> Cost
    -- unCostTag (CostTag t) = t 
    -- unCostTag x = errorT ["unCostTag -  not a Cost", showT x]
    -- unLengthTag :: ObjCountry -> LengthD
    unLengthTag (LengthTag t) = t 
    unLengthTag x = errorT ["unLengthTag - not a Length", showT x]

unTagPoints2V:: ObjCountry -> V2 Double 
unTagPoints2V = unName . unPointTag

-- p2dToV2 :: Point2d -> V2d 
-- p2dToV2 (Point2d x y) = V2 x y 

-- class ObjectsHQ a where 
--     nodeObj :: IDtype -> a 
--     edgeObj :: IDtype -> a
--     faceObj :: IDtype -> a 
--     halfQuadObj :: IDtype -> a 
--     -- pointObj :: a
--     unHalfQuad :: a -> IDtype
--     unFace :: a -> IDtype 
--     unPointTag :: a -> Pnt2 
--     pointTag :: Pnt2 -> a 
--     unLengthTag :: a -> LengthD
--     lengthTag :: LengthD -> a 
--     areaTag :: AreaD -> a
--     unAreaTag :: a -> AreaD 
    
instance Zeros MorphCountry  where zero = ZZm
instance NiceStrings MorphCountry
    -- where shownice = showT  


data HQtriples obj rel = HQtriples 
    { _NodesTrip :: [Tup3 obj rel]
    , _FacesTrip :: [Tup3 obj rel]
    , _HQtrips   :: [Tup3 obj rel]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- reorganisationm o,p,o -> p, (o,o)

reorg214 :: (a1, a2, b) -> (a2, (a1, b))
reorg214 (a,b,c) = (b, (a,c))

-- type Tup3 obj rel = Tup3 obj rel -- (rel, (obj,  obj))
-- TODO 
-- type CountryElement = StoreElement MorphCountry ObjCountry 

-- type CatCountry = Store ObjCountry MorphCountry
-- type CatCountryState = State  (Store ObjCountry MorphCountry)  [StoreElement MorphCountry ObjCountry]
