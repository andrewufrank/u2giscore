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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.Geometry_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.Point2dData
import UniformBase

import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 

p1 = Pnt2d 99 (V2 1 2):: Pnt2

test_p1 = assertEqual "Pnt2d {_p2id = 99, _v2 = V2 1.0 2.0}" (showT p1)

test_v2zero = assertEqual "Pnt2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::Pnt2))

-- for tests five points 
-- fiveV2 :: [V2D]
-- fiveV2 = map pnt2d_v2 fivePnt2d

test_ccw_t1 :: IO ()
test_ccw_t1 = assertBool (ccw_test (fiveV2 !! 0) (fiveV2 !! 1) (fiveV2 !! 2))
test_ccw_t2 = assertEqual False (ccw_test (fiveV2 !! 1) (fiveV2 !! 0) (fiveV2 !! 2))
test_ccw_t1a = assertBool (ccw_test (fivePnt2d !! 0) (fivePnt2d !! 1) (fivePnt2d !! 2))
-- test_ccw_t3 = assertBool (ccw_test  ([0,0]::[Double]) ([5,0]::[Double]) ([10,10]::[Double]))
-- conversion for Double not available anymore 

test_scale1 = assertEqual (V2 4.0 8.0::V2D) ((4 *) (V2 1 2))

test-distance = assertEqual 0 (distance (fiveV2 !! 0) (fiveV2 !! 1))

test_incenter = assertEqual (V2 2.0 1.4142135623730951) 
            (incenter (V2 2 2) (V2 1 1) (V2 3 1))
test_incenter2 = assertEqual (V2 1.9109272075633474 2.2150407435390105) 
        (incenter (V2 3 3) (V2 1 2) (V2 2 2))
-- Input: A(2, 2), B(1, 1), C(3, 1) 
--         and AB = 2, BC = 1, AC = 1
-- Output: (2, 1.5)-- not accurate!

-- Input: A(3, 3), B(1, 2), C(2, 2) 
--         and AB = 3, BC = 2, AC = 2
-- Output: (2.5, 2.83) -- badly rounded

test_circum1 = assertEqual (V2 2 1) (circumCenter (V2 2 2) (V2 1 1) (V2 3 1) :: V2D)