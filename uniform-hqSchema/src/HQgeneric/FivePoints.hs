-----------------------------------------------------------------------------
--
-- Module      :  Uniform. 
--      convert the HQ produced by geometry 
--      to a list of load instructions 
--      requires HQschema (or its components in Country)
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}



module HQgeneric.FivePoints where

-- import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import UniformBase
import Uniform.GeometryFunctions
import ExampleData.Point2d
import HQschema.HQschemaShort
-- import Uniform.TripleStore
-- import Uniform.Point2d
import Uniform.TesselationHalfQuads
import HQschema.HQschemaTop


mainMake5points :: ErrIO () 
mainMake5points = do 
    putIOwords ["\nmainMake5points generic\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    -- tessShort4 <- liftIO $ delaunay2 fourPnt2d    
    let tessShort4 = delaunay2 fourPnt2d
    let trips4 = hqToTrip 400 . toHq1 $ tessShort4 
    -- putIOwords ["triples produces\n", showT trips]
    let res4 = intoCat (getAllTrips trips4) 
    putIOwords ["generic triple store  produced\n", shownice res4]

    -- tessShort5 <- liftIO $ delaunay2 fivePnt2d  
    let tessShort5 = delaunay2 fivePnt2d  
    let trips5 = hqToTrip 500 . toHq1 $ tessShort5 
    -- putIOwords ["triples produces\n", showT trips]
    let res5 = intoCat (getAllTrips trips5) 
    putIOwords ["generic triple store  produced\n", shownice res5]
    return ()

