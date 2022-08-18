 -----------------------------------------------------------------------------
--
-- Module      :  process the fourteen point example data set 
-- as a comprehensive example 

--  used HQfaces_test as example 

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , UndecidableInstances     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module ExampleData.HQfourteenPoints
    where

import           Test.Framework hiding (scale, (.&.))

import UniformBase  
import Uniform.NaiveTripleStore
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
import ExampleData.HQconstructions4graphics 
-- -- import Control.Exception
import Uniform.GeometryFunctions
-- import Uniform.Point2d ()
import Uniform.Point2dData
import Uniform.TesselationHalfQuads
import Uniform.TripleStore (CatStores(catStoreBatch))
import Uniform.Drawings
  
-- import Uniform.TripleRels
-- import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
import Data.Functor.Identity
import ExampleData.HQconstructionsEdges
import ExampleData.HQconstructionsFaces
import Control.Monad.RWS (MonadWriter(tell))
 
pageHQfourteenPoints :: ErrIO ()
pageHQfourteenPoints = do 
    putIOwords ["the production of the catStore for fourteen points example"]
    putIOwords ["the points", showAsLines fourteenPnt2d]