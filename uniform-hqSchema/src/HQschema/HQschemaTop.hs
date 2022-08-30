-----------------------------------------------------------------------------
--
-- Module      :  HQschema Top
--      the operations applicable to the HQschema as necessary
--      to build the catStore 

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


module HQschema.HQschemaTop 
    (module HQschema.HQschemaTop
    , module Uniform.TripleStore
    , StoreTessShortElement (..)
    , CatStoreTessShort (..)
    , CatStoreState
    )
where

import Uniform.Point2d
import Uniform.TripleStore
-- import Uniform.Point2d
import UniformBase
import Control.Monad.State
import HQschema.HQschemaShort
import Uniform.TesselationHalfQuads
import Uniform.GeometryFunctions
-- import Uniform.TripleStore
import HQschema.HQconstructions4graphics 
import HQschema.HQconstructionsFaces
import HQschema.HQconstructionsEdges



makeTripNode :: Int -> NodeHQ -> StoreTessShortElement
-- -- | convert trip_xy   hqnx,   
-- -- a is NodeID or FaceID (for center )
-- -- note: the Face is the dual of the Node 
-- the first field is just the id to check
makeTripNode  i (NodeHQ j pnt) = reorg214  ( Node i, XY
                    , PointTag  (Pnt2d (_p2id  pnt) (_v2 pnt)))

getAllTrips :: TessShortHQtriples -> [StoreTessShortElement]
getAllTrips hqt = concat [_NodesTrip hqt, _FacesTrip hqt, _HQtrips hqt]

makeTripHq :: Int -> Int -> HQdataHQ -> [StoreTessShortElement]
-- convert the HQ data to StoreTessShortElements
makeTripHq offset i hq = catMaybes [hqnode,  hqface, hqtwin, hqhalflength]
    where
        hqid =   HalfQuad$ i 
        hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreTessShortElement
        hqnode = Just . reorg214  $ (hqid, HqNode,  Node   . (+offset) . hq_node $ hq)
        hqface =   fmap  (\fi -> reorg214 (hqid, HqFace,   Face  . (+offset) $ fi)) (Just . hq_face  $ hq)
        -- in qhull is outerface = Nothing
        -- in hgeometry is outerface just one of the faces
        hqnext =  (hqid, NextHq,   HalfQuad  . (+offset) . hq_orbitNext $   hq)        
        hqtwin = Just . reorg214 $ (hqid, Twin, HalfQuad  . (+offset) . hq_twin $ hq)
        hqhalflength = Nothing 
            -- Just $ (hqid, Dist, LengthTag . Length . halflength $ hq) 


hqToTrip :: Int -> TesselationHQ ->  TessShortHQtriples
hqToTrip offset teshq  = TessShortHQtriples
    { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
    -- , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
    -- are constructed later 
    , _FacesTrip = []
    , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQdatas teshq)
    } 


-- cat400 :: CatStoreTessShort
-- cat400 = catStoreEmpty
-- cat401 :: CatStore ObjPoint MorphTessShort
intoCat :: [(StoreTessShortElement)] -> CatStoreTessShort
intoCat ts = storeBatch (map wrapIns ts) $ storeEmpty -- cat400


-- makeCat :: 

makeCatFrom offset pnts = intoCat . getAllTrips . hqToTrip offset . toHq1 . delaunay2 $ pnts


forEdges2points = [lengthHQtriple, midpointHQtriple]  
forFaces = [circumcenter2triple, incenter2triple]
forFaces2 = [area2triples]
-- forqueries = [points12]
-- theCats = [tess41short, tess51short]
-- additinsPoints :: CatStoreTessShort -> [StoreTessShortElement]
additinsPoints cat =  concat [evalTrans4query2cat trans points12 cat | trans <-[lengthHQtriple, midpointHQtriple]  ] -- trans query cat
additinsAreas cat  =  concat [evalTrans4query2cat trans coords2faces cat | trans <-[area2triples] ] -- trans query cat
additinsCenters cat =  catMaybes . concat   $ [evalTrans4query2cat trans coords2faces cat | trans <-[circumcenter2triple, incenter2triple]  ] -- trans query cat
allAddins cat = concat [additinsPoints cat, additinsAreas cat, additinsCenters cat]
