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


module Uniform.Raster_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import ExampleData.Point2d
-- import Uniform.Point2d
import UniformBase
-- import Control.Lens
-- import Control.Exception
-- import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 
-- import Uniform.Field
-- import Uniform.Fourier
-- import Uniform.Raster
import Data.Complex
import Uniform.FourierTextBook ( defuzzR )
import Uniform.Raster
-- import ExampleData.TerrainLike
 
left = 400 
bott= 800
wid = 100
hei = 50 
grid4 = Raster  (V2 left bott) (V2 wid hei)
g00 = (0,0) :: (Int,Int)
g23 = (2,3):: (Int,Int)

test_g00m = assertEqual (V2 left bott) $ rowCol2world (4, 6) grid4 g00
test_g23m = assertEqual (V2 450.0 825) $ rowCol2world (4, 6) grid4 g23

test_xr00 = assertEqual (V2 400 800) $ rowCol2world (4, 6) grid4 g00  -- 400 800
test_yr02 = assertEqual (V2 425 825) $ rowCol2world (4, 6) grid4 (1,3) -- 425 825

test_r01 = assertEqual (V2 425 800) $ rowCol2world (4, 6) grid4 (1,0) -- 425 800
test_r04 = assertEqual (V2 500 850) $ rowCol2world (4, 6) grid4 (4,6) -- 500 850

xx1 = (V2 4 6) ^/^ (V2 4 6)

test_t00 = assertEqual (1,6) $ world2rowCol (4,6) grid4 (V2 425 850)

test_g00t1 = assertEqual g00 $ world2rowCol (4, 6) grid4 (V2 left bott)
test_g23t1 = assertEqual g23 $ world2rowCol (4, 6) grid4 (V2 450.0 825)


