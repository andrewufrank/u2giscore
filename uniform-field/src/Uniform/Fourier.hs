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

import Data.Complex
 
import ExampleData.TerrainLike
import GHC.Float (int2Double)
import Uniform.FourierComfort
import Uniform.FourierTextBook
import Uniform.Raster

-- storable frequency domain Fourier transformed 
-- the size of the 2d array of the transforms
-- the world coordinates of the grid originally
-- data FourierTransformed = FourierTransformed 
--     {  raster :: Raster  
--             -- ^ the real world coords of the original raster 
--     , rows, cols :: Int     -- ^ the size of the array used for the transformations 
--     , mat :: [Complex Double] -- the Fourier transform in frequncy space
--     }
--     deriving (Show, Read,   Eq, Generic)

fourier :: Raster ->   [[Double]] -> FourierTransformed 
-- converts to the fourier transformed and stores the descriptor
fourier raster  mat = FourierTransformed raster rows cols 
        . dfttw2d rows cols $ mat
    where 
            rows = length mat 
            cols = length . head $ mat 

fourierInv :: FourierTransformed -> [[Double]]
fourierInv ft = idfttw2d (rows ft) (cols ft) (mat ft)
-- inverts the fourier transformation and produces the original array (real!)
-- data Raster = Raster
--         {  x,y :: Double   -- the lower left corner 
--         , rowwidth, colheight :: Double -- the size of the viewport
--         }
--     deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- getValueAt :: FourierTransformed -> V2D ->  Double 
-- -- a naive and not to be used demonstration how to obtain a single value 
-- -- back 
-- getValueAt ft v@(V2 x y) = (matTF) !! r !! c 

--     where 
--         matTF = fourierInv ft
--         (r,c) = world2rowCol (rows ft, cols ft) (raster ft) v

-- -- rowCol2world :: ows cols Grid -> (Int, Int) -> V2 Double
-- rowCol2world ::  (Int, Int) -> Raster -> (Int,Int) -> V2 Double
-- -- convert from the row col to the world xy - with paramters of number of rows and colums in the image the raster descriptor 
-- rowCol2world (rows, cols) (Raster  x y rw ch) (r,c) = V2 xw yw
--     where 
--             xw = x + (fromIntegral r * rw / fromIntegral rows)
--             yw = y +  (fromIntegral c * ch / fromIntegral cols)

-- -- world2rowCol :: Grid -> V2 Double -> (Int,Int)
-- world2rowCol ::  (Int, Int) -> Raster -> V2 Double -> (Int, Int)
-- -- convert back to the row col to the world xy - with paramters of number of rows and colums in the image the raster descriptor 
-- world2rowCol (rows, cols) (Raster  x y rw ch) (V2 xw yw) = (floor r, floor c) 
--     where 
--             r = (xw - x) * fromIntegral rows / rw 
--             c = (yw -y) * fromIntegral cols / ch 

-- -- viewport :: StateVar (Position, Size)

-- -- Controls the affine transformation from normalized device coordinates to window coordinates. The viewport state variable consists of the coordinates (x, y) of the lower left corner of the viewport rectangle, (in pixels, initial value (0,0)), and the size (width, height) of the viewport. When a GL context is first attached to a window, width and height are set to the dimensions of that window.

-- -- Let (xnd, ynd) be normalized device coordinates. Then the window coordinates (xw, yw) are computed as follows:

-- -- xw = (xnd + 1) (width / 2) + x

-- -- yw = (ynd + 1) (heigth / 2) + y

-- -- Viewport width and height are silently clamped to a range that depends on the implementation, see maxViewportDims.

-- -- better use resize from https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-Processing.html



pageFourier3 :: ErrIO ()
pageFourier3 = do 
    putIOwords ["start pageFourier3 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

