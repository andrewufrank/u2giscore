 -----------------------------------------------------------------------------
--
-- Module      :  Test Naive Triple Store 
--          with a minimal Schema:
-- the tag of the sum type is the constructor for the node id 
--

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

module ExampleData.HQfaces_test
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
  
-- import Uniform.TripleRels
-- import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
import ExampleData.HQconstructionsEdges
import ExampleData.HQconstructionsFaces
import Uniform.TripleStore (CatStores(catStoreBatch))
import Control.Monad.RWS (MonadWriter(tell))
 
tess44short = makeCatFrom fourPnt2d 
tess55short = makeCatFrom fivePnt2d 


(coords2faces_4) = evalState coords2faces tess44short
(coords2faces_5) = evalState coords2faces tess55short

point1s :: [(ObjTessShort, (ObjTessShort, ObjTessShort))]
(point1s) = evalState points12 tess44short

lengthHQ = evalState distanceOfHQ tess44short

-- lengthHQ2Ins4 :: [StoreTessShortElement]
lengthHQ2Ins4 = evalState lengthHQasTriple tess44short
lengthHQ2Ins5 = evalState lengthHQasTriple tess55short

-- midpointHQ4 = evalState midpointHQ tess44short 
-- midpointHQasTriple4 = evalState midpointHQasTriple tess44short 

-- | evaluate a transformation to a queryresult against a catStore 
-- questionalbe shortcut - may be difficult to debug?? 
-- evalTrans4query2cat trans query cat = evalState ((fmap (map trans )) query) cat 

-- build the completion 
-- with length, midpoint 
-- with area incircle circumcircle 

-- trans which go for edges 
forEdges2points = [lengthHQtriple, midpointHQtriple]  
forFaces = [circumcenter2triple, incenter2triple]
forFaces2 = [area2triples]
forqueries = [points12]
theCats = [tess44short]
additinsPoints =  concat [evalTrans4query2cat trans points12 cat | trans <-[lengthHQtriple, midpointHQtriple] , cat <- theCats] -- trans query cat
additinsAreas =  concat [evalTrans4query2cat trans coords2faces cat | trans <-[area2triples] , cat <- theCats] -- trans query cat
additinsCenters =  catMaybes . concat   $ [evalTrans4query2cat trans coords2faces cat | trans <-[circumcenter2triple, incenter2triple] , cat <- theCats] -- trans query cat
allAddins = concat [additinsPoints, additinsAreas, additinsCenters]

cat45 = catStoreBatch (map Ins allAddins) tess44short

addinsFirst = evalTrans4query2cat midpointHQtriple points12 tess44short

hq3 = evalTrans4query2cat id hqTriangles cat45 

pageHQfaces_test3 :: ErrIO ()
pageHQfaces_test3 = do
    putIOwords ["the triples in cat45  \n ", shownice cat45]

    putIOwords ["the hq triangles  \n ", showAsLines $ hq3]
    putIOwords ["the hq triangles 1  \n ", showAsLines $ evalTrans4query2cat id hqTriangles1 cat45 ]
    putIOwords ["the hq triangles 2  \n ", showAsLines $ evalTrans4query2cat id hqTriangles2 cat45 ]
    putIOwords ["the hq triangles 3 \n ", showAsLines $ evalTrans4query2cat id hqTriangles3 cat45 ]
    -- putIOwords ["the midpoint of the hq \n ", showAsLines  $ evalState midpointHQ tess44short]
    -- putIOwords  ["first additions \n", showAsLines $ addinsFirst]


pageHQfaces_test2 :: ErrIO ()
pageHQfaces_test2 = do

    putIOwords ["the midpointHQtriple \n ", showAsLines $  evalTrans4query2cat midpointHQtriple points12 tess44short]
    -- putIOwords ["the midpoint of the hq \n ", showAsLines  $ evalState midpointHQ tess44short]
    putIOwords  ["first additions \n", showAsLines $ addinsFirst]
    putIOwords  ["all the additinsPoints \n", showAsLines  $ additinsPoints]
    putIOwords  ["all the additinsarea \n", showAsLines  $ additinsAreas]
    putIOwords  ["all the additinscenters \n", showAsLines  $ additinsCenters]
    putIOwords  ["all  \n", showAsLines  $ allAddins]
    putIOwords ["the new triplestore cat45", shownice cat45]

    return ()

pageHQfaces_test :: ErrIO ()
pageHQfaces_test = do
    putIOwords ["the tests for relations after storing four and five points"]
    -- putIOwords ["tess44short\n", showlong tess44short, "\n"    ]
    putIOwords ["the end coord of the hqs", showAsLines point1s]
 
    putIOwords ["the distances2 distanceOfHQ ", showAsLines lengthHQ]
    putIOwords ["the distances triples to insert \n ", showAsLines   lengthHQ2Ins4]
    putIOwords ["the distances triples to insert for five \n ", showAsLines   lengthHQ2Ins5]
    putIOwords ["the midpoint of the hq \n ", showAsLines  $ evalState midpointHQ tess44short]
    putIOwords ["the midpoint as triple \n ", showAsLines  $ evalState midpointHQasTriple tess44short]
    putIOwords ["the area \n ", showAsLines  $ evalState area2facesM tess44short]
    putIOwords ["the circum2facesM \n ", showAsLines  $ evalState circum2facesM tess44short]
    putIOwords ["the incenter2facesM \n ", showAsLines  $ evalState incenter2facesM tess44short]

    putIOwords ["the incenter2facesTriples \n ", showAsLines . catMaybes $ evalState incenter2facesTriples tess44short]
    putIOwords ["the incircumCenter2facesTriples \n ", showAsLines . catMaybes $ evalState incircumCenter2facesTriples tess44short]

    putIOwords ["the incircumCenter2facesTriples \n ", showAsLines . catMaybes $  evalTrans4query2cat circumcenter2triple coords2faces tess44short
        ]
  
    return () 