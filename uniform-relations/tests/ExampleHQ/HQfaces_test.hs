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

import           Test.Framework

import UniformBase
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort

-- import Control.Exception
-- import Uniform.GeometryFunctions
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
hqfaceInv = converseRel hqface
hqNode = getRel tessShort HqNode 
xyPoint = getRel tessShort XY 

faceNode = compRel hqNode hqfaceInv
facePoint = compRel xyPoint faceNode 
pointsFace400 = filter ((( (Face 400))==).fst) facePoint

pointsF400 :: [Double]
pointsF400 = map (v2toList2  . (scale 20) . p2dToV2 .  unPointTag . snd) pointsFace400 
-- [[0.0,0.0],[2.0,0.0],[1.5,1.5]]