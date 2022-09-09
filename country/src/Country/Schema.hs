-----------------------------------------------------------------------------
--
-- Module      :  The schema for the minimal country data set
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


module Country.Schema
    (module Country.Schema
    , module Country.HQtypes
    , module Country.Types
    , module Uniform.TripleStore
    , module Uniform.Rels2
    ) where

import Uniform.Point2d
import Uniform.TripleStore
import Uniform.Rels2
-- import Uniform.Point2d
import UniformBase
import Control.Monad.State
import Country.HQtypes  -- data types defined for HQ 
import Country.Types  -- data types defined for HQ 


type IDtype = Int

data ObjCountry = 
  -- objects
      Node IDtype 
    | Edge IDtype 
    | Face IDtype
    | HalfQuad IDtype             -- HQ is defined in TesselationHQ

    | PointTag Pnt2
    | PersonTag Person 
    | PlaceTag Place
  -- values 
    | LengthTag   LengthD  
    | AreaTag AreaD 
    -- | CostTag Cost 
    -- | NameTag Name
    | ZZpoint
    deriving (Show, Read, Ord, Eq, Generic)

-- | the sum type for the relation names
data MorphCountry = 
    -- Stag S | Ttag T | 
  -- relations
      Twin 
    | HqNode  
    | HqFace 
    | NextHq
  -- properties
    | XY
    | Incenter   -- where to place a label - incenter
    | Quant Int  -- the quantity with dimension n (0 count, 1 length, 2 area, 3 volume etc. )  

    -- | CenterTag Center | SurfacedTag Surfaced   

        -- | SCosttag SC 
        -- | TCcosttag TC -- probably never used, cost of incoming edge?
        -- | NamedTag
    | ZZm 
    deriving (Show, Read, Ord, Eq, Generic )
    
class MorphsHQ a where 
    hqFace :: a 
    hqNode :: a 
    hqXY :: a 
instance MorphsHQ MorphCountry where 
    hqFace = HqFace 
    hqNode = HqNode
    hqXY = XY 


