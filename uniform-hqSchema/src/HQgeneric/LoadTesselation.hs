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


module HQgeneric.LoadTesselation
    (module HQgeneric.LoadTesselation
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
import Uniform.TesselationHalfQuads
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



makeTripNode :: forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Eq rel, Eq obj,   Ord obj)  => Int -> NodeHQ -> Tup3 obj rel -- StoreTessShortElement
-- | convert trip_xy   hqnx,   
-- a is NodeID or FaceID (for center )
-- note: the Face is the dual of the Node 
-- the first field is just the id to check
makeTripNode  i (NodeHQ j pnt) = reorg214  ( nodeObj i, hqXY
                    , pointTag  (Pnt2d (_p2id  pnt) (_v2 pnt)))

-- getAllTrips :: TessShortHQtriples -> [StoreTessShortElement]
getAllTrips :: HQtriples obj rel -> [Tup3 obj rel]
getAllTrips hqt = concat [_NodesTrip hqt, _FacesTrip hqt, _HQtrips hqt]

-- makeTripHq :: Int -> Int -> HQdataHQ -> [StoreTessShortElement]
-- convert the HQ data to StoreTessShortElements
makeTripHq offset i hq = catMaybes [hqnode,  hqface, hqtwin, hqhalflength]
    where
        hqid =   halfQuadObj i 
        -- hqnode, hqface, hqtwin, hqhalflength -- :: Maybe StoreTessShortElement
        hqnode = Just . reorg214  $ (hqid, hqNode,  nodeObj   . (+offset) . hq_node $ hq)
        hqface =   fmap  (\fi -> reorg214 (hqid, hqFace,   faceObj  . (+offset) $ fi)) (Just . hq_face  $ hq)
        -- in qhull is outerface = Nothing
        -- in hgeometry is outerface just one of the faces
        -- hqnext =  (hqid, nextHq,   halfQuadObj  . (+offset) . hq_orbitNext $   hq)        
        hqtwin = Just . reorg214 $ (hqid, hqTwin, halfQuadObj  . (+offset) . hq_twin $ hq)
        hqhalflength = Nothing 
            -- Just $ (hqid, Dist, LengthTag . Length . halflength $ hq) 


-- hqToTrip :: Int -> TesselationHQ ->  TessShortHQtriples
hqToTrip :: forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Eq rel, Eq obj,   Ord obj)  => IDtype -> TesselationHQ -> HQtriples obj rel
hqToTrip offset teshq  = HQtriples
    { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
    -- , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
    -- are constructed later 
    , _FacesTrip = []
    , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQdatas teshq)
    } 


-- cat400 :: CatStoreTessShort
-- cat400 = catStoreEmpty
-- cat401 :: Store ObjPoint MorphTessShort
-- intoCat :: [(StoreTessShortElement)] -> CatStoreTessShort
intoCat :: (Eq rel, Eq obj) => [(Tup3 obj rel)] -> Store obj rel  -- CatCountry
intoCat ts = storeBatch (map wrapIns ts) $ storeEmpty -- cat400



-- makeCat :: 

makeCatFrom :: forall rel obj a . (ObjectsHQ obj, MorphsHQ rel, Eq rel, Eq obj,   Ord obj, ToHPoint2 a) =>
    IDtype -> [a] -> Store obj rel
makeCatFrom offset pnts = intoCat . getAllTrips . hqToTrip offset . toHq1 . delaunay2 $ pnts


-- forEdges2points = [lengthHQtriple, midpointHQtriple]  
-- forFaces = [circumcenter2triple, incenter2triple]
-- forFaces2 = [area2triples]
-- -- forqueries = [points12]
-- -- theCats = [tess41short, tess51short]
-- -- additinsPoints :: CatStoreTessShort -> [StoreTessShortElement]
-- additinsPoints :: Store ObjCountry MorphCountry -> [(MorphCountry, (ObjCountry, ObjCountry))]
-- additinsPoints cat =  concat [evalTrans4query2cat trans points12 cat | trans <-[lengthHQtriple, midpointHQtriple]  ] -- trans query cat
-- -- additinsAreas :: (MorphsHQ rel, Eq rel) => Store obj rel -> [(rel, (obj, obj))]
-- additinsAreas cat  =  concat [evalTrans4query2cat trans coords2faces cat | trans <-[area2triples] ] -- trans query cat
-- additinsCenters cat =  catMaybes . concat   $ [evalTrans4query2cat trans coords2faces cat | trans <-[circumcenter2triple, incenter2triple]  ] -- trans query cat
-- allAddins :: Store ObjCountry MorphCountry -> [(MorphCountry, (ObjCountry, ObjCountry))]
-- allAddins cat = concat [additinsPoints cat, additinsAreas cat, additinsCenters cat]
