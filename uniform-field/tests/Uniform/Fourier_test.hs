-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Fourier_test -the common interface
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


module Uniform.Fourier_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import ExampleData.Point2d
import ExampleData.TerrainLike

import Uniform.Point2d
-- import Uniform.FourierTextBook(defuzz')
-- import Uniform.FourierComfort
-- import Uniform.FourierTextBook
import Uniform.Fourier 
import UniformBase
import Data.Complex
-- import qualified Data.Array.Comfort.Boxed as C
-- import  Data.Array.Comfort.Boxed (fromList, toList)
-- import Data.Array.Comfort.Shape-- import Control.Lens
-- import Control.Exception
-- import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 
-- import Uniform.HQfaces_test

grid88 :: [Complex Double]
grid88 = map (:+ 0) . concat $ map (take 8) grid8_11

left = 400 
bott= 800
wid = 100
hei = 50 
grid4 = Grid 4 6 left bott wid hei
g00 = (0,0) :: (Int,Int)
g23 = (2,3):: (Int,Int)

test_g00m = assertEqual (V2 left bott) $ rowCol2world grid4 g00
test_g23m = assertEqual (V2 450.0 825) $ rowCol2world grid4 g23

-- test_g00r = assertEqual g00 $ world2rowCol grid4 (V2 left bott)

test_g00t1 = assertEqual g00 $ world2rowCol grid4 (V2 left bott)
test_g23t1 = assertEqual g23 $ world2rowCol grid4 (V2 450.0 825)
 
-- test_inv44 :: IO ()
-- test_inv44 = assertEqual h44 $ idfttw2d 4 4 . dfttw2d 4 4 $ h44
-- -- test_inv88 = assertEqual grid8_11 $ idfttw2d . dfttw2d 8 11 $ grid8_11
-- -- fails for numerical issues

-- testinv88' = assertEqual (replicate (8*11) 0) $ map defuzzR $ zipWith (-)
--     (concat grid8_11) (concat . idfttw2d 8 11 . dfttw2d 8 11 $ grid8_11)
--     where
--         defuzzR x = if abs x < 1.0E-6 then 0 else x
