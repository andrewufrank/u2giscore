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


module ExampleHQ.SchemaInstances
    (
        module ExampleHQ.SchemaInstances
    , module ExampleHQ.Schema
    , module Uniform.SchemaFoundation
    ) 
    where

 
import UniformBase
import ExampleHQ.Schema
import Uniform.SchemaFoundation  

  
instance Zeros ObjCountry where zero = ZZpoint

instance NiceStrings ObjCountry
    -- where shownice = showT
    -- would need a shownice for pointTag to propagate the shownice to the inside data 
-- instance NiceStrings Pnv2 where 
--     shownice = showT 

instance MorphsHQ MorphCountry where 
    hqFace = HqFace 
    hqNode = HqNode
    hqXY = XY
    hqTwin = Twin  



instance ObjectsHQ ObjCountry where 
    nodeObj = Node 
    edgeObj = Edge
    faceObj = Face
    halfQuadObj = HalfQuad 
    pointTag = PointTag
    -- pointName i p = PointTag $ putName i p
    lengthTag = LengthTag
    areaTag = AreaTag 

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
 