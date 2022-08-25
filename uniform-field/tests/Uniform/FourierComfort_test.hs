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


module Uniform.FourierComfort_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import ExampleData.Point2d
import ExampleData.TerrainLike

import Uniform.Point2d
-- import Uniform.FourierTextBook(defuzz')
import Uniform.FourierComfort
import UniformBase
import Data.Complex
import qualified Data.Array.Comfort.Boxed as C
import  Data.Array.Comfort.Boxed (fromList, toList)
import Data.Array.Comfort.Shape-- import Control.Lens
-- import Control.Exception
-- import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 
-- import Uniform.HQfaces_test

grid88 :: [Complex Double]
grid88 = map (:+ 0) . concat $ map (take 8) grid8_11

a88 = fromList (ZeroBased 8, ZeroBased 8) grid88 
test_fromtolist1 = assertEqual grid88 (toList a88)
test_shape = assertEqual (ZeroBased 8, ZeroBased 8) (C.shape a88)
test_v2zero = assertEqual "Pnt2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::Pnt2))



test_inv44 = assertEqual h44 $ idfttw2d . dfttw2d 4 4 $ h44
-- test_inv88 = assertEqual grid8_11 $ idfttw2d . dfttw2d 8 11 $ grid8_11
-- fails for numerical issues

testinv88' = assertEqual (replicate (8*11) 0) $ map defuzzR $ zipWith (-)
    (concat grid8_11) (concat . idfttw2d . dfttw2d 8 11 $ grid8_11)
    where
        defuzzR x = if abs x < 1.0E-6 then 0 else x
