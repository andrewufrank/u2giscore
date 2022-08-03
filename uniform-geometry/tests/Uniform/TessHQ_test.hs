-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pretty_test
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
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

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.TessHQ_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import UniformBase
import Uniform.TesselationHalfQuads
-- import Control.Exception
import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 

mainMakeTess = do 
    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    tess <-   delaunay2 fourV2    
    let trips =  toHq1 $ tess   --hqToTrip 400 .
    -- putIOwords ["\nthe hq for faces\n", showT ]
    return  trips 

test_toHq1 = (do 
                res <- mainMakeTess
                assertEqual tripsRes res )

tripsRes = TesselationHQ {_Nodes = [NodeHQ (V2 0.0 0.0),NodeHQ (V2 1.5 1.5),NodeHQ (V2 0.0 2.0),NodeHQ (V2 2.0 0.0)], _Faces = [FaceHQ {circumcenter = V2 1.0 0.5},FaceHQ {circumcenter = V2 0.5 1.0}], _HQs = [HQ {node = 1, face = Nothing, twin = 5, halflength = 0.7905694150420949},HQ {node = 0, face = Just 0, twin = 6, halflength = 1.0},HQ {node = 0, face = Just 1, twin = 7, halflength = 1.0606601717798212},HQ {node = 1, face = Just 1, twin = 8, halflength = 0.7905694150420949},HQ {node = 0, face = Nothing, twin = 9, halflength = 1.0},HQ {node = 3, face = Just 0, twin = 0, halflength = 0.7905694150420949},HQ {node = 3, face = Nothing, twin = 1, halflength = 1.0},HQ {node = 1, face = Just 0, twin = 2, halflength = 1.0606601717798212},HQ {node = 2, face = Nothing, twin = 3, halflength = 0.7905694150420949},HQ {node = 2, face = Just 1, twin = 4, halflength = 1.0}]} 