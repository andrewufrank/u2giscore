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
import Uniform.Point2d 
-- import ExampleData.Point2d
-- -- import Vector
-- import Linear.Vector ((*^))
-- import Linear.V2
-- -- import Linear.Vector 
-- import qualified Linear.Metric as Metric
-- import qualified Linear.Vector as Lin
-- import Control.Lens 
-- import GHC.Generics

import Data.Complex
-- import Extra
-- import qualified Data.Array.Comfort.Boxed as C
-- import  Data.Array.Comfort.Boxed (fromList, toList)
-- import Data.Array.Comfort.Shape
-- import Numeric.FFTW.Rank2
-- import qualified Data.Array.Comfort.Boxed.Unchecked
-- import qualified Data.Array.Comfort.Storable.Private as Pr
-- import Data.Array.Repa hiding (map)
-- import Data.Array.Repa.Eval
-- import Data.Array.Repa.Repr.ForeignPtr
-- import Data.Array.Repa.FFTW
-- -- import Data.Repa.Array
import ExampleData.TerrainLike
import GHC.Float (int2Double)
import Uniform.FourierComfort
import Uniform.FourierTextBook

-- storable frequency domain Fourier transformed 
-- the size of the 2d array of the transforms
-- the world coordinates of the grid originally
data FourierTransformed = FourierTransformed 
    {  raster :: Raster  
            -- ^ the real world coords of the original raster 
    , rows, cols :: Int     -- ^ the size of the array used for the transformations 
    , mat :: [Complex Double] -- the Fourier transform in frequncy space
    }
    deriving (Show, Read,   Eq, Generic)

fourier :: Raster -> (Int,Int) -> [[Double]] -> FourierTransformed 
fourier raster (rows,cols) mat = FourierTransformed raster rows cols 
        . dfttw2d rows cols $ mat

fourierInv :: FourierTransformed -> [[Double]]
fourierInv ft= idfttw2d (rows ft) (cols ft) (mat ft)
-- Raster describes the original raster in world coordinates
data Raster = Raster
        {  x,y :: Double   -- the lower left corner 
        , rowwidth, colheight :: Double -- the size of the viewport
        }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- rowCol2world :: ows cols Grid -> (Int, Int) -> V2 Double
rowCol2world rows cols (Raster  x y rw ch) (r,c) = V2 xw yw
    where 
            xw = x + (fromIntegral r * rw / fromIntegral rows)
            yw = y +  (fromIntegral c * ch / fromIntegral cols)

-- world2rowCol :: Grid -> V2 Double -> (Int,Int)
world2rowCol rows cols (Raster  x y rw ch) (V2 xw yw) = (floor r, floor c) 
    where 
            r = (xw - x) * fromIntegral rows / rw 
            c = (yw -y) * fromIntegral cols / ch 

-- viewport :: StateVar (Position, Size)

-- Controls the affine transformation from normalized device coordinates to window coordinates. The viewport state variable consists of the coordinates (x, y) of the lower left corner of the viewport rectangle, (in pixels, initial value (0,0)), and the size (width, height) of the viewport. When a GL context is first attached to a window, width and height are set to the dimensions of that window.

-- Let (xnd, ynd) be normalized device coordinates. Then the window coordinates (xw, yw) are computed as follows:

-- xw = (xnd + 1) (width / 2) + x

-- yw = (ynd + 1) (heigth / 2) + y

-- Viewport width and height are silently clamped to a range that depends on the implementation, see maxViewportDims.

-- better use resize from https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-Processing.html







pageFourier3 :: ErrIO ()
pageFourier3 = do 
    putIOwords ["start pageFourier3 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

-- grid88 :: [Complex Double]
-- grid88 = map (:+ 0) . concat $ map (take 8) grid8_11
