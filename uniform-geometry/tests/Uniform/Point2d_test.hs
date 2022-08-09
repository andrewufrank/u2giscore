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
import Uniform.Point2d 
-- import qualified Data.Geometry.Point as HP 

p1 = Pnt2d 77 (V2 1 2):: Pnt2
v1 = V2 3 4
-- test construction of pnt2d
test_p1 = assertEqual "Pnt2d {_p2id = 77, _v2 = V2 1.0 2.0}" (showT p1)
test_v2zero = assertEqual "Pnt2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::Pnt2))
-- test access Pnt2
test_lensName = assertEqual (_p2id p1) (p1 ^. p2id)
test_lensPV2 = assertEqual (_v2 p1) (p1 ^. v2)
test_lensPV2x = assertEqual (1) (p1 ^. v2 . _x)
test_lensV2x = assertEqual (3) (v1 ^. _x)
test_lensV2y = assertEqual (4) (v1 ^. _y )

-- test_HPointText = assertEqual "Point2 0.0 0.0 :+ \"11\"" (showT . toHPointText
--  . head $ fourPnt2d)
test_HPointInt = assertEqual "Point2 0.0 0.0 :+ 11" (showT . toHPointInt . head $ fourPnt2d)
test_HPoint_V2 = assertEqual "Point2 0.0 0.0" (showT . toHPoint . head $ fourV2)

-- -- i think the following could be reasonably defaults - but I see not how 
-- -- to pass the type info 
-- test_HPoint_Pnt2d_def = assertEqual "Point2 0.0 0.0" (showT . toHPoint . head $ fourPnt2d)
test_HPoint_Pnt2d_toInt = assertEqual "Point2 0.0 0.0 :+ 11" (showT . toHPointInt . head $ fourPnt2d)
-- test_HPoint_V2_toText = assertEqual "Point2 0.0 0.0 :+ \"11\"" (showT . toHPointText . head $ fourPnt2d)

-- test_V2_x = assertEqual "V2 0.0 0.0" (showT . head $ fourV2)