-----------------------------------------------------------------------------
--
-- Module      :  Uniform.ExampleTempData

-- | the example data for GIS 
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

import Control.Monad.State
import UniformBase
import Uniform.Time
import Uniform.Point2d 
import Uniform.TripleStore
-- import HQschema.HQschemaTop
-- import Uniform.Raster
-- import Uniform.Field
-- import Data.Complex
 
-- import ExampleData.TerrainLike
impoty Uniform.TempStore 
-- import Uniform.Fourier 
-- import Uniform.Raster
type Time = UTCTime
-- type Storage = CatStoreTessShort
-- type StorageElement = StoreTessShortElement

-- copied from TripleStore
-- type StoreTessShortElement = (ObjTessShort, MorphTessShort, ObjTessShort)

-- type CatStoreTessShort = CatStore ObjTessShort MorphTessShort
-- type CatStoreState = State  CatStoreTessShort [StoreTessShortElement]




pageExample4 :: ErrIO ()
pageExample4 = do 
    putIOwords ["start pageExample4 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

