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
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
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
    -- putIOwords ["the circum2facesM \n ", showAsLines  $ evalState circum2facesM tess44short]
    putIOwords ["the incenter2facesM \n ", showAsLines  $ evalState incenter2facesM tess44short]


  
    return () 