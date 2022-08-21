-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Fourier
-- | A simple implementation of fourier transform
-- following https://www.skybluetrades.net/blog/2013/11/2013-11-13-data-analysis-fft-1.html
-- probably essentially used to learn DFFT 

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
-- import ExampleData.Point2d
-- -- import Vector
-- import Linear.Vector ((*^))
-- import Linear.V2
-- -- import Linear.Vector 
-- import qualified Linear.Metric as Metric
-- import qualified Linear.Vector as Lin
-- import Control.Lens 
-- import GHC.Generics

import Prelude hiding (length, sum, map, zipWith, (++))
import Data.Complex
import Data.Vector

i :: Complex Double
i = 0 :+ 1

omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

dft, idft :: Vector (Complex Double) -> Vector (Complex Double)
dft = dft' 1 1
idft v = dft' (-1) (1.0 / (fromIntegral $ length v)) v

dft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
dft' sign scale h = generate bigN (((scale :+ 0) *) . doone)
  where bigN = length h
        w = omega bigN
        doone n = sum $
                  zipWith (*) h $ generate bigN (\k -> w^^(sign*n*k))

defuzz :: Vector (Complex Double) -> Vector (Complex Double)
defuzz = map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x

pageFourier :: ErrIO ()
pageFourier = do 
    putIOwords ["start pageFourier experiment"]


    return ()