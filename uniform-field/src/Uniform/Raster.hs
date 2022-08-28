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
import Linear.Vector
import Data.Complex
-- import Data.Array.Repa (Shape(sizeIsValid))
 
-- import ExampleData.TerrainLike
-- import GHC.Float (int2Double)
-- import Uniform.Fourier 


-- storable frequency domain Fourier transformed 
-- the size of the 2d array of the transforms
-- the world coordinates of the grid originally
data Field d = Field 
    {  raster :: Raster d 
            -- ^ the real world coords of the original raster 
    , rows, cols :: Int     -- ^ the size of the array used for the transformations 
    , mat :: [Complex d] -- the Fourier transform in frequncy space
    }
    deriving (Show, Read,   Eq, Generic)


class Rasters d where 
    rasterDescriptor :: V2 d -> V2 d -> Raster d

instance Rasters Double where
    rasterDescriptor ll@(V2 x1 y1) tr@(V2 x2 y2) = Raster ll w
        where w = tr - ll 

-- todo add better functions once clear what is needed 

data Raster d = Raster
        {  rasterorigin :: V2 d   -- the lower left corner 
        , rastersize :: V2 d -- the size of the raster
        }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

type RasterD = Raster Double 

-- rowCol2world :: ows cols Grid -> (Int, Int) -> V2 Double
rowCol2world ::  (Int, Int) -> RasterD -> (Int,Int) -> V2 Double
-- convert from the row col to the world xy - with paramters of number of rows and colums in the image the raster descriptor 
rowCol2world (rows, cols) (Raster  orig size) (r,c) = res
--  rowCol2world (rows, cols) (Raster  x y xr yc) (r,c) = V2 xw yw
   where 
            res = orig ^+^ size ^*^ (v2int r c ^/^ (v2int rows cols))
            -- xw = x + (fromIntegral r * xr / fromIntegral rows)
            -- -- w = x + [r/rows, c /cols]
            -- yw = y +  (fromIntegral c * yc / fromIntegral cols)
            -- rc = V2 (fromIntegral r) (fromIntegral c)
            -- rowscols = V2 (fromIntegral rows) (fromIntegral cols)

(^*^) :: V2 Double -> V2 Double -> V2 Double
(^*^) = liftI2 (*)
(^/^) :: V2 Double -> V2 Double -> V2 Double
(^/^) = liftI2 (/)

v2int :: Int -> Int -> V2 Double
v2int a b = V2 (fromIntegral a) (fromIntegral b)

-- world2rowCol :: Grid -> V2 Double -> (Int,Int)
world2rowCol ::  (Int, Int) -> RasterD -> V2 Double -> (Int, Int)
-- convert back to the row col to the world xy - with paramters of number of rows and colums in the image the raster descriptor 
world2rowCol (rows, cols) (Raster  orig size) (pt) = (floor r, floor c) 
-- orld2rowCol (rows, cols) (Raster  x y xr yc) (V2 xw yw) = (floor r, floor c) 
    where 
        V2 r c = res  
        res = (pt ^-^  orig) ^*^ (v2int rows cols)  ^/^ size 
            -- r = (xw - x) * fromIntegral rows / xr 
            -- -- rc = w - xh * [rows/xr, cols/yc]
            -- c = (yw -y) * fromIntegral cols / yc 
        -- rowscols = V2 (fromIntegral rows) (fromIntegral cols)

-- viewport :: StateVar (Position, Size)

-- Controls the affine transformation from normalized device coordinates to window coordinates. The viewport state variable consists of the coordinates (x, y) of the lower left corner of the viewport rectangle, (in pixels, initial value (0,0)), and the size (width, height) of the viewport. When a GL context is first attayced to a window, width and height are set to the dimensions of that window.

-- Let (xnd, ynd) be normalized device coordinates. Then the window coordinates (xw, yw) are computed as follows:

-- xw = (xnd + 1) (width / 2) + x

-- yw = (ynd + 1) (heigth / 2) + y

-- Viewport width and height are silently clamped to a range that depends on the implementation, see maxViewportDims.

-- better use resize from https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-Processing.html



 