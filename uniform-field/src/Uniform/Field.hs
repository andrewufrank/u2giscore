-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Field

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


module Uniform.Field
         where

import UniformBase
import Uniform.Point2d 

import Data.Complex
 
import ExampleData.TerrainLike
import GHC.Float (int2Double)
import Uniform.Fourier 
import Uniform.Raster

type Field = FourierTransformed 

class Fields d where 
-- ^ a continuou changing value in a 2d domain f x y -> v 
    createField :: Raster d -> [[d]] -> Field
    
    -- | a proof of concept access to a value at a point 
    -- do not use - performance penalty!
    getValueAt :: Field -> V2 d -> d 

 
instance Fields Double where 
    createField raster mat = fourier raster rows cols mat
        where 
            rows = length mat 
            cols = length . head $ mat 
    getValueAt ft v@(V2 x y) = (matTF) !! r !! c 
        where 
            matTF = fourierInv ft
            (r,c) = world2rowCol (rows ft, cols ft) (raster ft) v


-- -- storable frequency domain Fourier transformed 
-- -- the size of the 2d array of the transforms
-- -- the world coordinates of the grid originally
-- data FourierTransformed = FourierTransformed 
--     {  raster :: Raster  
--             -- ^ the real world coords of the original raster 
--     , rows, cols :: Int     -- ^ the size of the array used for the transformations 
--     , mat :: [Complex Double] -- the Fourier transform in frequncy space
--     }
--     deriving (Show, Read,   Eq, Generic)

-- fourier :: Raster ->   [[Double]] -> FourierTransformed 
-- -- converts to the fourier transformed and stores the descriptor
-- fourier raster  mat = FourierTransformed raster rows cols 
--         . dfttw2d rows cols $ mat
--     where 
--             rows = length mat 
--             cols = length . head $ mat 

-- fourierInv :: FourierTransformed -> [[Double]]
-- fourierInv ft = idfttw2d (rows ft) (cols ft) (mat ft)
-- -- inverts the fourier transformation and produces the original array (real!)
-- -- TODO - these should be integers (raster cells)
-- -- where to convert to world coord?
 

-- getValueAt :: FourierTransformed -> V2D ->  Double 
-- -- a naive and not to be used demonstration how to obtain a single value 
-- -- back 
-- getValueAt ft v@(V2 x y) = (matTF) !! r !! c 

--     where 
--         matTF = fourierInv ft
--         (r,c) = world2rowCol (rows ft, cols ft) (raster ft) v


-- viewport :: StateVar (Position, Size)

-- Controls the affine transformation from normalized device coordinates to window coordinates. The viewport state variable consists of the coordinates (x, y) of the lower left corner of the viewport rectangle, (in pixels, initial value (0,0)), and the size (width, height) of the viewport. When a GL context is first attayced to a window, width and height are set to the dimensions of that window.

-- Let (xnd, ynd) be normalized device coordinates. Then the window coordinates (xw, yw) are computed as follows:

-- xw = (xnd + 1) (width / 2) + x

-- yw = (ynd + 1) (heigth / 2) + y

-- Viewport width and height are silently clamped to a range that depends on the implementation, see maxViewportDims.

-- better use resize from https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-Processing.html



pageField3 :: ErrIO ()
pageField3 = do 
    putIOwords ["start pageField3 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

