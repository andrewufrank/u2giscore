-----------------------------------------------------------------------------
--
-- Module      :  Load the dual, i.e. the additional triples 
--              after hafing loaded the result of the hq tesselation 

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
{-# HLINT ignore "Redundant $" #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module HQgeneric.LoadDual
    (module HQgeneric.LoadDual
    -- , module HQgeneric.LoadTesselation
    -- , module Uniform.TripleStore
    -- , module  Uniform.TesselationHalfQuads
    -- , StoreTessShortElement (..)
    -- , CatStoreTessShort (..)
    -- , CatStoreState
    )
where

import UniformBase
import Uniform.SchemaFoundation

-- import Uniform.Point2d
-- import Uniform.TripleStore
-- -- import Uniform.Point2d
-- import Control.Monad.State
-- -- import Country.Schema
-- -- import Country.Store 
-- -- import HQschema.HQschemaShort
-- import Uniform.TesselationHalfQuads
import Uniform.GeometryFunctions
-- import Uniform.TripleStore
-- import HQgeneric.HQconstructions4graphics 
-- import HQgeneric.HQconstructionsFaces
--     ( coords2faces,
--       area2triples,
--       incenter2triple,
--       circumcenter2triple,
--       evalTrans4query2cat )
-- import HQgeneric.HQconstructionsEdges



forEdges2points = [lengthHQtriple, midpointHQtriple]  
forFaces = [circumcenter2triple, incenter2triple]
forFaces2 = [area2triples]
-- forqueries = [points12]
-- theCats = [tess41short, tess51short]
-- additinsPoints :: CatStoreTessShort -> [StoreTessShortElement]

additinsPoints :: Store ObjCountry MorphCountry -> [(MorphCountry, (ObjCountry, ObjCountry))]
additinsPoints cat =  concat [evalTrans4query2cat trans points12 cat | trans <-[lengthHQtriple, midpointHQtriple]  ] -- trans query cat
-- additinsAreas :: (MorphsHQ rel, Eq rel) => Store obj rel -> [(rel, (obj, obj))]

additinsAreas cat  =  concat [evalTrans4query2cat trans coords2faces cat | trans <-[area2triples] ] -- trans query cat

additinsCenters cat =  catMaybes . concat   $ [evalTrans4query2cat trans coords2faces cat | trans <-[circumcenter2triple, incenter2triple]  ] -- trans query cat

allAddins :: Store ObjCountry MorphCountry -> [(MorphCountry, (ObjCountry, ObjCountry))]
allAddins cat = concat [additinsPoints cat, additinsAreas cat, additinsCenters cat]
