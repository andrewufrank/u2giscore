-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TempTripleStore

-- | the time wrapper around the triple store
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


module Uniform.TempTripleStore
         where

import UniformBase
import Uniform.Time
import Uniform.Point2d 
import Uniform.TripleStore
-- import HQschema.HQschemaTop
-- import Uniform.Raster
-- import Uniform.Field
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

class (Eq t, Ord t) => TimedTripleStore t trp where 
-- | a wrapper around the triples 
-- perhaps I should have created a class triple and one store
    ttempty :: [(t,trp)]
    ttinsert :: t -> trp -> [(t,trp)] -> [(t,trp)]
    -- ttdel 
    ttfind ::   t -> t -> [(t,trp)] -> [(t,trp)]
    -- provide lower and upper bound on time 

instance TimedTripleStore t trp where 
    ttempy = []
    ttinsert t trp = ((t,trp) :)
    ttfind t1 t2 = filter (\tt -> ((t1 =<).fst $ tt) && ((t2 >). fst $ tt))



pageTemporal2 :: ErrIO ()
pageTemporal2 = do 
    putIOwords ["start pageTemporal2 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

