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


module Uniform.Point2d_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import Uniform.Point2d
import UniformBase

-- import Control.Exception
import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 

p1 = Pnt2d "A" (V2 1 2):: Pnt2

test_p1 = assertEqual "Pnt2d {_p2id = \"A\", _v2 = V2 1.0 2.0}" (showT p1)

test_v2zero = assertEqual "Pnt2d {_p2id = \"\", _v2 = V2 0.0 0.0}" (showT (zero::Pnt2))

test_HPointText = assertEqual "Pnt2d {_p2id = \"11\", _v2 = V2 0.0 0.0}" (show . head $ fourPnt2d)
test_HPointInt = assertEqual "Pnt2d {_p2id = 11, _v2 = V2 0.0 0.0}" (show . head $ fourPnt2dInt)
test_HPoint = assertEqual "V2 0.0 0.0" (show . head $ fourV2)