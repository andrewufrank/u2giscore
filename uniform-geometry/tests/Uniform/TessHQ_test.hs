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

mainMakeTess v2s= do 
    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    tess <-   delaunay2 v2s 
    putIOwords ["\nthe tess\n", showT tess]

    let trips =  toHq1 $ tess   --hqToTrip 400 .
    putIOwords ["\nthe hq for faces\n", showT trips]
    return  trips 

test_toHqFour = (do 
                res <- mainMakeTess fourV2
                assertEqual tripsResfour res )
test_toHqFive = (do 
                res <- mainMakeTess fiveV2
                assertEqual tripsResfive res )

test_toHqFive31 = (do 
                res <- mainMakeTess fiveV2_31
                assertEqual tripsResfive_31 res )

tripsResfour = TesselationHQ {_Nodes = [NodeHQ (V2 0.0 0.0),NodeHQ (V2 1.5 1.5),NodeHQ (V2 0.0 2.0),NodeHQ (V2 2.0 0.0)], _Faces = [FaceHQ {circumcenter = V2 1.0 0.5},FaceHQ {circumcenter = V2 0.5 1.0}], _HQs = [HQ {node = 1, face = Nothing, twin = 5, halflength = 0.7905694150420949},HQ {node = 0, face = Just 0, twin = 6, halflength = 1.0},HQ {node = 0, face = Just 1, twin = 7, halflength = 1.0606601717798212},HQ {node = 1, face = Just 1, twin = 8, halflength = 0.7905694150420949},HQ {node = 0, face = Nothing, twin = 9, halflength = 1.0},HQ {node = 3, face = Just 0, twin = 0, halflength = 0.7905694150420949},HQ {node = 3, face = Nothing, twin = 1, halflength = 1.0},HQ {node = 1, face = Just 0, twin = 2, halflength = 1.0606601717798212},HQ {node = 2, face = Nothing, twin = 3, halflength = 0.7905694150420949},HQ {node = 2, face = Just 1, twin = 4, halflength = 1.0}]} 

tripsResfive = TesselationHQ {_Nodes = [NodeHQ (V2 0.0 0.0),NodeHQ (V2 3.0 0.0),NodeHQ (V2 4.0 2.0),NodeHQ (V2 3.0 5.0),NodeHQ (V2 0.0 3.0)], _Faces = [FaceHQ {circumcenter = V2 2.1363636363636362 3.0454545454545454},FaceHQ {circumcenter = V2 1.5 1.5},FaceHQ {circumcenter = V2 1.8333333333333335 1.8333333333333333}], _HQs = [HQ {node = 3, face = Just 0, twin = 7, halflength = 1.8027756377319946},HQ {node = 2, face = Just 2, twin = 8, halflength = 2.0615528128088303},HQ {node = 2, face = Just 0, twin = 9, halflength = 1.5811388300841898},HQ {node = 1, face = Nothing, twin = 10, halflength = 2.1213203435596424},HQ {node = 0, face = Nothing, twin = 11, halflength = 1.5},HQ {node = 0, face = Just 1, twin = 12, halflength = 1.5},HQ {node = 1, face = Just 2, twin = 13, halflength = 1.118033988749895},HQ {node = 4, face = Nothing, twin = 0, halflength = 1.8027756377319946},HQ {node = 4, face = Just 0, twin = 1, halflength = 2.0615528128088303},HQ {node = 3, face = Nothing, twin = 2, halflength = 1.5811388300841898},HQ {node = 4, face = Just 2, twin = 3, halflength = 2.1213203435596424},HQ {node = 4, face = Just 1, twin = 4, halflength = 1.5},HQ {node = 1, face = Nothing, twin = 5, halflength = 1.5},HQ {node = 2, face = Nothing, twin = 6, halflength = 1.118033988749895}]}

tripsResfive_31 = TesselationHQ {_Nodes = [NodeHQ (V2 0.0 0.0),NodeHQ (V2 3.1 0.0),NodeHQ (V2 4.0 2.0),NodeHQ (V2 3.0 5.0),NodeHQ (V2 0.0 3.0)], _Faces = [FaceHQ {circumcenter = V2 2.1363636363636362 3.0454545454545454},FaceHQ {circumcenter = V2 1.55 1.4999999999999998},FaceHQ {circumcenter = V2 1.8196629213483149 1.7786516853932581}], _HQs = [HQ {node = 3, face = Just 0, twin = 7, halflength = 1.8027756377319946},HQ {node = 2, face = Just 2, twin = 8, halflength = 2.0615528128088303},HQ {node = 2, face = Just 0, twin = 9, halflength = 1.5811388300841898},HQ {node = 1, face = Just 1, twin = 10, halflength = 2.156965461012299},HQ {node = 0, face = Nothing, twin = 11, halflength = 1.5},HQ {node = 0, face = Just 1, twin = 12, halflength = 1.55},HQ {node = 1, face = Just 2, twin = 13, halflength = 1.0965856099730653},HQ {node = 4, face = Nothing, twin = 0, halflength = 1.8027756377319946},HQ {node = 4, face = Just 0, twin = 1, halflength = 2.0615528128088303},HQ {node = 3, face = Nothing, twin = 2, halflength = 1.5811388300841898},HQ {node = 4, face = Just 2, twin = 3, halflength = 2.156965461012299},HQ {node = 4, face = Just 1, twin = 4, halflength = 1.5},HQ {node = 1, face = Nothing, twin = 5, halflength = 1.55},HQ {node = 2, face = Nothing, twin = 6, halflength = 1.0965856099730653}]}