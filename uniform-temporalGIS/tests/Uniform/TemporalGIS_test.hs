-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TemporalGIS_test
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


module Uniform.TemporalGIS_test where

import           Test.Framework hiding (scale)
import ExampleData.Point2d
import Uniform.Point2d
import UniformBase
 
-- import Uniform.Field
-- import Uniform.Fourier
-- import Uniform.Raster
-- import Data.Complex
-- import Uniform.FourierTextBook ( defuzzR )
-- import ExampleData.TerrainLike



-- grid88 :: [Complex Double]
-- grid88 = map (:+ 0) . concat $ map (take 8) grid8_11

-- raster44 :: Raster Double
-- raster44 = Raster (V2 500 1000) (V2 40 40) 
-- raster811 :: Raster Double
-- raster811 = Raster (V2 1000 2000) (V2 110 80) 



-- ft811 = fourier raster811 8 11   grid8_11
-- ft811tf = fourierInv ft811

-- g39 = grid8_11 !! 3 !! 9 
-- v39 = rowCol2world (8,11) raster811 (3,9)
-- v39back =  world2rowCol (8,11) raster811 v39
-- test_get39 = assertEqual zero $ defuzzR $ g39 - getValueAt ft811 v39