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
import Extra
import qualified Data.Array.Comfort.Boxed as C
import  Data.Array.Comfort.Boxed (fromList, toList)
import Data.Array.Comfort.Shape
import Numeric.FFTW.Rank2
import qualified Data.Array.Comfort.Boxed.Unchecked
import qualified Data.Array.Comfort.Storable.Private as Pr
-- import Data.Array.Repa hiding (map)
-- import Data.Array.Repa.Eval
-- import Data.Array.Repa.Repr.ForeignPtr
-- import Data.Array.Repa.FFTW
-- -- import Data.Repa.Array
import ExampleData.TerrainLike
import GHC.Float (int2Double)
import Uniform.FourierTextBook
import Uniform.FourierTextBook

-- storable frequency amplitude Fourier transformed 
data FourierTransformed = FourierTransformed 
    {  vpmap :: ViewPortMap   
            -- ^ the mapping from matric indices to real world coords 
    , mat :: [(Complex Double)] -- the Fourier transform in frequncy space
    }
    deriving (Show, Read,   Eq, Generic)

data ViewPortMap = ViewPortMap
        { rows, cols :: Int                -- the number of rows and colums
        , x,y :: Double   -- the lower left corner 
        , width, height :: Double -- the size of the viewport
        }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- rowCol2viewport (ViewPortMap rows cols x y w h) (r,c) = V2 xw yw
--     where 
--             xw = x + (r * w /rows)
--             yw = y +  (c + h/cols)
-- viewport2rowCol (ViewPortMap rows cols x y w h) (V2 xw yw) = (r,c) 
--     where 
--             r = (xw - x) * rows / r 
--             c = (yw -y) * cols / c 

-- viewport :: StateVar (Position, Size)

-- Controls the affine transformation from normalized device coordinates to window coordinates. The viewport state variable consists of the coordinates (x, y) of the lower left corner of the viewport rectangle, (in pixels, initial value (0,0)), and the size (width, height) of the viewport. When a GL context is first attached to a window, width and height are set to the dimensions of that window.

-- Let (xnd, ynd) be normalized device coordinates. Then the window coordinates (xw, yw) are computed as follows:

-- xw = (xnd + 1) (width / 2) + x

-- yw = (ynd + 1) (heigth / 2) + y

-- Viewport width and height are silently clamped to a range that depends on the implementation, see maxViewportDims.

-- better use resize from https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-Processing.html



-- helper 
createMatrix :: Int -> [a] -> [[a]]
createMatrix _ [] = []
createMatrix n xs = take n xs : createMatrix n (drop n xs)




pageFourier3 :: ErrIO ()
pageFourier3 = do 
    putIOwords ["start pageFourier3 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

-- grid88 :: [Complex Double]
-- grid88 = map (:+ 0) . concat $ map (take 8) grid8_11
