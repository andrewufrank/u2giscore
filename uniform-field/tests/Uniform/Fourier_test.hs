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
-- import ExampleData.Point2d
import ExampleData.TerrainLike

import Uniform.Point2d
-- import Uniform.FourierTextBook(defuzz')
-- import Uniform.FourierComfort
import Uniform.FourierTextBook ( defuzzR )
import Uniform.Fourier 
import Uniform.Raster
import UniformBase
import Data.Complex
 

grid88 :: [Complex Double]
grid88 = map (:+ 0) . concat $ map (take 8) grid8_11


raster44 = Raster (V2 500 1000) (V2 40 40) 
raster811 = Raster (V2 1000 2000) (V2 110 80) 
f44 = fourier raster44  4 4 h44 

-- test_inv44 :: IO ()
test_inv44 = assertEqual h44 $ fourierInv . fourier raster44 4 4   $ h44
-- test_inv44 = assertEqual h44 $ idfttw2d 4 4 . dfttw2d 4 4 $ h44

-- test_inv88 = assertEqual grid8_11 $ fourierInv . fourier raster811 (8,11) $ grid8_11
-- -- fails for numerical issues

testinv88' = assertEqual (replicate (8*11) 0) $ map defuzzR $ zipWith (-)
    (concat grid8_11) (concat . fourierInv . fourier raster811  8 11  $ grid8_11)
    where
        defuzzR x = if abs x < 1.0E-6 then 0 else x

