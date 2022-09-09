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

import UniformBase
import Uniform.GeometryFunctions
import ExampleData.Point2d
import HQgeneric.HQschemaTop
import Country.Store

mainMake5points :: ErrIO () 
mainMake5points = do 
    putIOwords ["\nmainMake5points generic\n"]
    let tessShort4 = delaunay2 fourPnt2d
    -- trips4 :: [Tup3 ObjCountry MorphCountry]
    let trips4 :: HQtriples ObjCountry MorphCountry = hqToTrip 400 . toHq1 $ tessShort4 
    -- putIOwords ["triples produces\n", showT trips]
    let res4 = intoCat (getAllTrips trips4) 
    putIOwords ["generic triple store  produced\n", shownice res4]

    -- tessShort5 <- liftIO $ delaunay2 fivePnt2d  
    let tessShort5 = delaunay2 fivePnt2d  
    let trips5 = hqToTrip 500 . toHq1 $ tessShort5 
    -- putIOwords ["triples produces\n", showT trips]
    -- res5 :: Store ObjCountry MorphCountry
    let res5 = intoCat (getAllTrips trips5) 
    putIOwords ["generic triple store  produced\n", shownice res5]
    return ()

