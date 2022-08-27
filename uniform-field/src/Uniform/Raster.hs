-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Raster

-- | helpers for fields and the fourier transforms

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


module Uniform.Raster

         where

import UniformBase
import Uniform.Point2d (Point2 (..), V2(..), V2D ) 

import Data.Complex
 
-- import ExampleData.TerrainLike
-- import GHC.Float (int2Double)
-- import Uniform.Fourier 


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


class Rasters r pt where 
    rasterDescriptor :: pt -> pt -> r

instance Rasters Raster V2D where
    rasterDescriptor ll@(V2 x1 y1) tr@(V2 x2 y2) = Raster x1 y1 (x w) (y w)
        where w = tr - ll 

data Raster = Raster
        {  xorigin,yorigin :: Double   -- the lower left corner 
        , xwidths, yheight :: Double -- the size of the raster
        }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

 

-- rowCol2world :: ows cols Grid -> (Int, Int) -> V2 Double
rowCol2world ::  (Int, Int) -> Raster -> (Int,Int) -> V2 Double
-- convert from the row col to the world xy - with paramters of number of rows and colums in the image the raster descriptor 
rowCol2world (rows, cols) (Raster  x y xr yc) (r,c) = V2 xw yw
    where 
            xw = x + (fromIntegral r * xr / fromIntegral rows)
            yw = y +  (fromIntegral c * yc / fromIntegral cols)

-- world2rowCol :: Grid -> V2 Double -> (Int,Int)
world2rowCol ::  (Int, Int) -> Raster -> V2 Double -> (Int, Int)
-- convert back to the row col to the world xy - with paramters of number of rows and colums in the image the raster descriptor 
world2rowCol (rows, cols) (Raster  x y xr yc) (V2 xw yw) = (floor r, floor c) 
    where 
            r = (xw - x) * fromIntegral rows / xr 
            c = (yw -y) * fromIntegral cols / yc 

-- viewport :: StateVar (Position, Size)

-- Controls the affine transformation from normalized device coordinates to window coordinates. The viewport state variable consists of the coordinates (x, y) of the lower left corner of the viewport rectangle, (in pixels, initial value (0,0)), and the size (width, height) of the viewport. When a GL context is first attayced to a window, width and height are set to the dimensions of that window.

-- Let (xnd, ynd) be normalized device coordinates. Then the window coordinates (xw, yw) are computed as follows:

-- xw = (xnd + 1) (width / 2) + x

-- yw = (ynd + 1) (heigth / 2) + y

-- Viewport width and height are silently clamped to a range that depends on the implementation, see maxViewportDims.

-- better use resize from https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-Processing.html



 