-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TemporalStorage

-- | storage for relations and fields 
--   with time
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
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
-- {-# LANGUAGE TypeApplications     #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.TemporalGIS
         where

import UniformBase
import Uniform.Time
import Uniform.Point2d 
-- import Uniform.TripleStore
import HQschema.HQschemaTop
-- import Uniform.Raster
import Uniform.Field
-- import Data.Complex
 
-- import ExampleData.TerrainLike
-- import Uniform.Fourier 
-- import Uniform.Raster
type Time = UTCTime
type Storage = CatStoreTessShort
type StorageElement = StoreTessShortElement

-- copied from TripleStore
-- type StoreTessShortElement = (ObjTessShort, MorphTessShort, ObjTessShort)

-- type CatStoreTessShort = CatStore ObjTessShort MorphTessShort
-- type CatStoreState = State  CatStoreTessShort [StoreTessShortElement]

class Tstoraged ts  where 
-- ^ the storage of a time GIS 
    emptyTstorage :: Time ->  ts
    -- ^ time and spatial extend to cover, add a snapshot relation storage
    addField :: Time -> Field Double -> ts -> ts 
    -- ^ add a field (not considering yet changes of a field)
    addToRelations :: Time ->  [StorageElement]  -> ts -> ts  
    addOneRelations :: Time ->  StorageElement  -> ts -> ts  
    -- ^ add a batch update to relatiosn

    -- todo add change ops  
 
data Tstore = Tstore 
    { minTime, maxTime :: Time  -- ^ the first, last update -- only additions in sequence
    , covered :: Raster Double -- ^ a box around the area covered 
    , fields :: [(Time, Field Double)] -- ^ the fields, with a time stamp when added
    , relations :: [(Time, StorageElement)]  -- the relation storage, each entry timed
    }

    deriving (Show, Read,   Eq, Generic, Zeros)

instance Zeros UTCTime where zero = fromEpochTime' 0
-- this counts seconds since 1970-01-01

instance Tstoraged Tstore where 
    emptyTstorage t = Tstore 
        { minTime = t
        , maxTime = t
        , covered = zero 
        , fields = []
        , relations = []
        }



pageTemporal2 :: ErrIO ()
pageTemporal2 = do 
    putIOwords ["start pageTemporal2 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

