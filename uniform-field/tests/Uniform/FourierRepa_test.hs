-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Field_test
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


module Uniform.FourierRepa_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import ExampleData.Point2d
import Uniform.Point2d
import UniformBase
-- import Control.Lens
-- import Control.Exception
-- import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 
-- import Uniform.HQfaces_test

p1 = Pnt2d 77 (V2 1 2):: Pnt2
v1 = V2 3 4
-- test construction of pnt2d
test_p1 = assertEqual "Pnt2d {_p2id = 77, _v2 = V2 1.0 2.0}" (showT p1)
test_v2zero = assertEqual "Pnt2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::Pnt2))
 