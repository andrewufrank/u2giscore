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

import           Test.Framework
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import   Uniform.Point
import Uniform.PointData
import UniformBase

import Control.Exception

p1 = Point2d 1 (V2 1 1):: P2

test_p1 = assertEqual "Point2d {_p2id = 1, _v2 = V2 1.0 1.0}" (showT p1)

test_v2zero = assertEqual "Point2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::P2))

test_id = assertEqual (1::Int) (p1 ^. p2id)
test_v2xy = assertEqual (V2 1.0 1.0) ((p1 ^. v2 . _xy))
test_v2y = assertEqual (1.0) ((p1 ^. v2 . _y))

