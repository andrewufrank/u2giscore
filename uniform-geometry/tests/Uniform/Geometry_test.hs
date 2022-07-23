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
import UniformBase

import Control.Exception

p1 = Point2d 1 (V2 1 1):: P2

test_p1 = assertEqual "Point2d {_id = 1, _v2 = V2 1.0 1.0}" (showT p1)

test_v2zero = assertEqual "Point2d {_id = 0, _v2 = V2 0.0 0.0}" (showT (zero::P2))

