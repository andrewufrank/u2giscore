-----------------------------------------------------------------------------
--
-- Module      :  Uniform.FourierComfort
-- | Highly optimized interface to FFTW 
-- using comfort-array

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


module Uniform.FourierComfort
         where

import UniformBase
-- import Uniform.Point2d 
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
-- import Numeric.Extra
-- import qualified Data.Array.Comfort.Boxed as C
import  Data.Array.Comfort.Boxed (fromList, toList)
import Data.Array.Comfort.Shape
import Numeric.FFTW.Rank2
-- import qualified Data.Array.Comfort.Boxed.Unchecked
import qualified Data.Array.Comfort.Storable.Private as Pr
-- import Data.Array.Repa hiding (map)
-- import Data.Array.Repa.Eval
-- import Data.Array.Repa.Repr.ForeignPtr
-- import Data.Array.Repa.FFTW
-- -- import Data.Repa.Array
-- import ExampleData.TerrainLike
-- import GHC.Float (int2Double)


-- forward Fourier transformation of a 2d matrix 
-- input of size of array (could be extracted, but required later for inverse)
dfttw2d :: Int -> Int -> [[Double]] -> [Complex Double]
dfttw2d m n mat =    Pr.toList 
        . fourier Forward 
        . Pr.fromList (Cyclic m, Cyclic n) . map (:+ 0). concat $ mat

-- inverse Fourier transformation restoring a 2d matrix 
-- needs same size input as in forward transform
idfttw2d :: Int -> Int -> [Complex Double] -> [[Double]]
idfttw2d   m n mat = 
        createMatrix n  
            . map (* scale) 
            . map ( realPart)  --(/(fromIntegral m*n)) .
            . Pr.toList . fourier Backward 
            . Pr.fromList (Cyclic m, Cyclic n) $ mat 
    where   scale :: Double 
            scale = 1/(fromIntegral (m*n))

-- helper -- create the original matrix sizes 
createMatrix :: Int -> [a] -> [[a]]
createMatrix _ [] = []
createMatrix n xs = take n xs : createMatrix n (drop n xs)



pageComfort1 :: ErrIO ()
pageComfort1 = do 
    putIOwords ["start pageComfort1 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()
