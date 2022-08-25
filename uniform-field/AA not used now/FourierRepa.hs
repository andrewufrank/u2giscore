-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Field
-- | A simple field
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


module Uniform.FourierRepa
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
import Data.Array.Repa hiding (map)
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.FFTW
-- import Data.Repa.Array

a :: Array F DIM1 (Complex Double)
a = fromList (Z :. 4) [i :+ 0 | i <- [0..3]]
a8 = fromList (Z :. 8) [i :+ 0 | i <- [0..7]]
f = fft a 
f8 = fft a8
b :: Array F DIM1 (Complex Double)
b = ifft f 
b8 :: Array F DIM1 (Complex Double)
b8 = ifft f8
blist :: [Complex Double]
blist = toList b
b8list = toList b8
f8list = toList f8
f8_4list = take 4 . toList $ f8
f8_4fromList = fromList (Z :.4) f8_4list 
b8_4 :: Array F DIM1 (Complex Double)
b8_4 = ifft f8_4fromList 
b8_4res :: [Complex Double]
b8_4res=toList b8_4

g88:: Array F DIM2 (Complex Double)
g88 = fromList (Z :. 8 :. 11) grid88

g88f :: Array F DIM2 (Complex Double)
g88f = fft2d g88 
g88list :: [Complex Double]
g88list = toList g88f
-- g88list2 :: [[Complex Double]]
-- g88list2 = toList g88f
g88list3 = toList g88f
-- >>> toList a
-- [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]
-- >>> toList $ fft a
-- [6.0 :+ 0.0,(-2.0) :+ 2.0,(-2.0) :+ 0.0,(-2.0) :+ (-2.0)]
-- >>> toList $ ifft $ fft a
-- [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3


pageF1 :: ErrIO ()
pageF1 = do 
    putIOwords ["start F1 experiment"]
    putIOwords ["g88", showT . toList $ g88]

    return ()

grid88 = map (:+ 0) . concat $ map (take 8) grid8_11

