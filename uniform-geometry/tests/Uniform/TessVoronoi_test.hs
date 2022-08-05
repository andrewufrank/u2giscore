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


module Uniform.TessVoronoi_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import UniformBase
import Uniform.TesselationHalfQuads
-- import Control.Exception
import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 

mainMakeTess v2s= do 
    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    tess <-   delaunay2 v2s 
    let vor = voronoi2d tess
    putIOwords ["\nthe tess\n", showT tess]
    putIOwords ["\nthe voronoi\n", showT vor]

    return  vor 

test_toHqFour = (do 
                res <- mainMakeTess fourV2
                assertEqual [] res )
-- test_toHqFive = (do 
--                 res <- mainMakeTess fiveV2
--                 assertEqual tripsResfive res )

-- test_toHqFive31 = (do 
--                 res <- mainMakeTess fiveV2_31
--                 assertEqual tripsResfive_31 res )
