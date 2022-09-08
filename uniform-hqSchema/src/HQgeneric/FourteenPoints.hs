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
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Redundant $" #-}

module HQgeneric.FourteenPoints
    where

import           Test.Framework hiding (scale, (.&.))

import UniformBase  
-- import HQschema.HQexampleShort
-- import HQschema.HQschemaShort
-- import HQschema.HQfaces_test
-- import HQschema.HQconstructions4graphics 
-- -- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.Point2d ()
import ExampleData.Point2d
import Uniform.TesselationHalfQuads
import Uniform.TripleStore  
import Uniform.Drawings
  
-- import Uniform.TripleRels
-- import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
import Data.Functor.Identity
-- import HQschema.HQconstructionsEdges
import HQgeneric.HQconstructionsFaces
import HQgeneric.HQconstructions4graphics
import HQgeneric.HQschemaTop
import Country.Schema
import Country.Store 
import Control.Monad.RWS (MonadWriter(tell))

-- tess14 = delaunay2 fourteenPnt2d 
-- -- extract the data into the half quad structure from the hgeometry processing
-- trip14 = hqToTrip 600 . toHq1 $ tess14 
-- -- produce the cat for the delaunay triangulation as extracted from hgeometry
-- cat60 = catStoreEmpty 
-- cat61 = catStoreBatch (map wrapIns . getAllTrips $ trip14) cat60
cat61 :: CatStore ObjCountry MorphCountry
cat61 = makeCatFrom  600 fourteenPnt2d
-- add the rest of the hq structure to have the primal (delaunay)
--  and the dual (voronoi) structure 

-- process to construct the addins in hqfaces  -- need cleaning
addtoCat :: [(StoreElement MorphCountry ObjCountry)] -> CatStore ObjCountry MorphCountry -> CatStore ObjCountry MorphCountry
addtoCat ts cat0 = storeBatch (map wrapIns ts) cat0

cat62 :: CatStore ObjCountry MorphCountry
cat62 = addtoCat  (allAddins cat61) cat61

-- the delaunay and voronoi diagram graphics

hqV6 :: [(ObjCountry, (V2D, V2D))]
hqV6 = evalTrans4query2cat pointsPairsv2 hqVoro cat62 
hqD6 :: [(ObjCountry, (V2D, V2D))]
hqD6 = evalTrans4query2cat pointsPairsv2 hqDela cat62 
fig6voro :: [Figure V2D]
fig6voro = map (\p -> figLine (dark green) p) . map snd $ hqV6
fig6dela :: [Figure V2D]
fig6dela = map (\p -> figLine (dark red) p) . map snd $ hqD6

mainMakeFourteenPoints :: ErrIO ()
mainMakeFourteenPoints = do 
    putIOwords ["mainMakeFourteenPoints: the production of the catStore for fourteen points example"]
    putIOwords ["the points", showAsLines fourteenPnt2d]
    putIOwords ["the voronoi lines  \n ", showT $ hqV6]
    putIOwords ["the delaunay lines  \n ", showT $ hqD6]

    showFacePage2 (fig6voro ++ fig6dela)
