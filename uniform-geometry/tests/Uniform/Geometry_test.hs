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


module Uniform.Geometry_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import Uniform.Point2d
import UniformBase

-- import Control.Exception
import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 

p1 = P2d 1 (V2 1 2):: P2

test_p1 = assertEqual "P2d {_p2id = 1, _v2 = V2 1.0 2.0}" (showT p1)

test_v2zero = assertEqual "P2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::P2))

test_id :: IO ()
test_id = assertEqual (1::Integer) (p1 ^. p2id)
test_v2xy :: IO ()
test_v2xy = assertEqual (V2 1.0 2.0) ((p1 ^. v2 . _xy))
test_v2y :: IO ()
test_v2y = assertEqual (2.0) ((p1 ^. v2 . _y))

test_toP2 :: IO ()
test_toP2 = assertEqual p1 (tup2P2 . p2_tup_id $ p1)
-- use inverse test 

-- for tests five points 
fiveV2 :: [V2d]
fiveV2 = map p2toV2 fiveP2

test_ccw_t1 :: IO ()
test_ccw_t1 = assertBool (ccw_test (fiveV2 !! 0) (fiveV2 !! 1) (fiveV2 !! 2))
test_ccw_t2 = assertEqual False (ccw_test (fiveV2 !! 1) (fiveV2 !! 0) (fiveV2 !! 2))
test_ccw_t1a = assertBool (ccw_test (fiveP2 !! 0) (fiveP2 !! 1) (fiveP2 !! 2))
test_ccw_t3 = assertBool (ccw_test  ([0,0]::[Double]) ([5,0]::[Double]) ([10,10]::[Double]))

test_scale1 = assertEqual (V2 4.0 8.0::V2d) (scale 4 (V2 1 2))