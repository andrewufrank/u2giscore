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
import ExampleData.HQconstructions
 
tess44short = makeCatFrom fourPnt2d 
tess55short = makeCatFrom fivePnt2d 


(coords2faces_4) = evalState coords2faces tess44short
(coords2faces_5) = evalState coords2faces tess55short

point1s :: [(ObjTessShort, (ObjTessShort, ObjTessShort))]
(point1s) = evalState points12 tess44short

-- dist12 :: [(ObjTessShort, Double)]
-- dist12 = map dist12one point1s

-- dist12one :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> (ObjTessShort, Double) 
-- dist12one (a,(p1,p2)) = (a, (distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)))

-- distHQ :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> StoreTessShortElement
-- distHQ (a,(p1,p2)) = (a, Dist, LengthTag . Length $ d/2 )
--     where d = (distance (unName . unPointTag $ p1) (unName . unPointTag $ p2))
-- -- zipWith distance (map (unPointTag . snd) point1s) 
-- --         (map (unPointTag . snd) point2s)

-- distanceOfHQ = fmap (map dist12one) points12
lengthHQ = evalState distanceOfHQ tess44short
lengthHQ2Ins4 :: [StoreTessShortElement]
lengthHQ2Ins4 = evalState lengthHQasTriple tess44short
lengthHQ2Ins5 = evalState lengthHQasTriple tess55short


pageHQfaces_test :: ErrIO ()
pageHQfaces_test = do
    putIOwords ["the tests for relations after storing four and five points"]
    -- putIOwords ["tess44short\n", showlong tess44short, "\n"    ]
    putIOwords ["the end coord of the hqs", showAsLines point1s]
 
    -- putIOwords ["the distances2 ", showT dist12]
    putIOwords ["the distances triples to insert \n ", showAsLines   lengthHQ2Ins4]
    -- putIOwords ["the distances triples to insert for five \n ", showAsLines   lengthHQ2Ins5]


  
    return () 