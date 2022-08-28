-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Fourier

-- | common interface to fourier transformations 
-- packs the descriptive information necessary to transform and back
-- together with the and then calls the transformation 

-----------------------------------------------------------------------------
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
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
-- {-# LANGUAGE TypeApplications     #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Fourier
         where

import UniformBase
-- import Uniform.Point2d 

-- import Data.Complex ()
 
-- import ExampleData.TerrainLike
-- import GHC.Float (int2Double)
import Uniform.FourierComfort ( dfttw2d, idfttw2d )
-- import Uniform.FourierTextBook
import Uniform.Raster



fourier :: RasterD -> Int -> Int ->  [[Double]] -> Field Double
-- converts to the fourier transformed and stores the descriptor
fourier raster rows cols mat = Field raster rows cols 
        . dfttw2d rows cols $ mat

fourierInv :: Field Double -> [[Double]]
fourierInv ft = idfttw2d (rows ft) (cols ft) (mat ft)
-- inverts the fourier transformation and produces the original array (real!)
-- data Raster = Raster
--         {  x,y :: Double   -- the lower left corner 
--         , rowwidth, colheight :: Double -- the size of the viewport
--         }
--     deriving (Show, Read, Ord, Eq, Generic, Zeros)


pageFourier3 :: ErrIO ()
pageFourier3 = do 
    putIOwords ["start pageFourier3 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

