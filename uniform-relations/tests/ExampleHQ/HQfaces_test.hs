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

module ExampleHQ.HQfaces_test
    where

import           Test.Framework hiding (scale)

import UniformBase
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort

-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.Point2d
import Uniform.TesselationHalfQuads
    ( delaunay2,
      fourV2,
      toHq1,
      FaceHQ(circumcenter),
      HQ(node, face, twin, halflength),
      NodeHQ(..),
      TesselationHQ(_Nodes, _Faces, _HQs) )
-- import Uniform.NaiveTripleStore
-- -- import Uniform.Object 
-- -- import Storable.Value
-- import Uniform.TripleStore
import Uniform.TripleRels

-- -- import  qualified         Algebra.Laws             as Law
-- import Data.List ( nub ) 

-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

--- example code  -- Minimal Schema

hqface = getRel tessShort HqFace 
hqnode = getRel tessShort HqNode
hqfaceInv = converseRel hqface
hqnodeInv = converseRel hqnode
xyPoint = getRel tessShort XY 

-- faceNode = compRel hqnode hqfaceInv
faceNode2 = hqfaceInv `semicolon` hqnode
-- facePoint = compRel xyPoint faceNode 
facePoint2 =  hqfaceInv `semicolon` hqnode `semicolon` xyPoint 
nodePoint2 =  hqnodeInv `semicolon` hqface `semicolon` xyPoint 
pointsFace400 = filter ((( (Face 400))==).fst) facePoint2
pointsFace401 = filter ((( (Face 401))==).fst) facePoint2

pointsDualFace400 = filter ((( (Node 400))==).fst) nodePoint2
-- pointsF400 :: [Double]
pointsF400 = map ( (scale 20) . toV2 .  unPointTag . snd) pointsFace400 
-- [[0.0,0.0],[2.0,0.0],[1.5,1.5]]
pointsF401 = map ( (scale 20) . toV2 .  unPointTag . snd) pointsFace401 
pointsD400 = map ( (scale 20) . toV2 .  unPointTag . snd) pointsDualFace400 

pageHQfaces_test :: ErrIO ()
pageHQfaces_test = do
    putIOwords ["\n [pageTriple4cat"]
    putIOwords ["pointsF400", shownice pointsF400]
    putIOwords ["pointsF401", shownice pointsF401]
    putIOwords ["pointsD400", shownice pointsD400]
--     putIOwords ["ts one", showT x1]

    -- putIOwords ["CatStore empty", shownice v0]
    -- putIOwords ["CatStore v1 with cp1", shownice v1]
    -- putIOwords ["CatStore v2 added cp2, deleted cp1", shownice v2]
    -- putIOwords ["CatStore a1x added batch cp1 cp2", shownice a1x]
    -- putIOwords ["CatStore  a2x", shownice a2x]
    -- putIOwords ["CatStore  v2a", shownice v2a]
    -- page2

test_x1 = do
        res <- runErr $  pageHQfaces_test 
                -- return True
        assertEqual (Right ()) res  -- does not produce output
