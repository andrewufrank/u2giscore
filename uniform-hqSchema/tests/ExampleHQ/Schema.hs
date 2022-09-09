-----------------------------------------------------------------------------
--
-- Module      :  The schema for the minimal test data (HQ)
-- based on and uses the code for the HQ schema 
-- for handling the geometry of tesselations 

-- this is the short schema style 
-- (with sumtype tag is Obj constructor)
-- using IDtype = Int
-- hq objects are not tagged (because no additional data)
-- country objects are tagged (and separately defined records)   
--      Points  
--      Persons, Places 
-- value data types are tagged 
--      Length, Area
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


module ExampleHQ.Schema
    (module ExampleHQ.Schema
    , module Uniform.SchemaFoundation
    , module Uniform.Point2d
     ) where

 
import UniformBase
import Uniform.Point2d
import Uniform.SchemaFoundation

data ObjCountry = 
  -- objects
      Node IDtype 
    | Edge IDtype 
    | Face IDtype
    | HalfQuad IDtype             -- HQ is defined in TesselationHQ

    | PointTag Pnt2
  -- values 
    | LengthTag   LengthD  
    | AreaTag AreaD 
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)

-- | the sum type for the relation names
data MorphCountry = 
 -- relations
      Twin 
    | HqNode  
    | HqFace 
    | NextHq
  -- properties
    | XY
    | Incenter   -- where to place a label - incenter
    | Quant Int  -- the quantity with dimension n (0 count, 1 length, 2 area, 3 volume etc. )  

    | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
    


