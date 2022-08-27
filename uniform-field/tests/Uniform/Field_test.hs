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


module Uniform.Field_test where

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
import Uniform.Field
import Uniform.Fourier
import Uniform.Raster
import Uniform.Raster_test
import Data.Complex
import Uniform.FourierTextBook ( defuzzR )
import ExampleData.TerrainLike



grid88 :: [Complex Double]
grid88 = map (:+ 0) . concat $ map (take 8) grid8_11



ft811 = fourier raster811    grid8_11
ft811tf = fourierInv ft811

g39 = grid8_11 !! 3 !! 9 
v39 = rowCol2world (8,11) raster811 (3,9)
v39back =  world2rowCol (8,11) raster811 v39
test_get39 = assertEqual zero $ defuzzR $ g39 - getValueAt ft811 v39