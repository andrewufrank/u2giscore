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


module Uniform.Field
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


-- >>> toList a
-- [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]
-- >>> toList $ fft a
-- [6.0 :+ 0.0,(-2.0) :+ 2.0,(-2.0) :+ 0.0,(-2.0) :+ (-2.0)]
-- >>> toList $ ifft $ fft a
-- [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3


pageF1 :: ErrIO ()
pageF1 = do 
    putIOwords ["start F1 experiment"]


    return ()

grid88 = map (take 8) grid8_11

grid8_11 :: [[Double]]
grid8_11 = [[385,382.5,380,378.75,380.714285714286,381.785714285714,385.384615384615,390.666666666667,397,403.333333333333,407.5],
    [387.5,385,382.5,380,380.357142857143,382.692307692308,386.923076923077,392.555555555556,398.888888888889,403.015873015873,407.5],
    [390,387.5,385,382.5,380,384.230769230769,388.461538461538,394.444444444444,398.571428571429,402.698412698413,407.5],
    [392.871287128713,391.485148514851,390.09900990099,388.712871287129,387.647058823529,388.823529411765,390,394.126984126984,398.253968253968,402.5,407.5],
    [400.19801980198,398.811881188119,397.425742574257,396.039603960396,395.294117647059,396.470588235294,395.384615384615,396.666666666667,398.888888888889,402.5,407.5],
    [406.785714285714,406.071428571429,404.752475247525,403.366336633663,402.941176470588,402.307692307692,400.769230769231,401.047619047619,403.142857142857,405.575757575758,409.69696969697],
    [412.5,411.785714285714,411.071428571429,410.357142857143,409,407,406.530612244898,406.367346938776,407.333333333333,409.428571428571,413.212121212121],
    [417.727272727273,416.818181818182,415.909090909091,415,413.30612244898,413.142857142857,412.979591836735,412.816326530612,412.65306122449,413.619047619048,416.727272727273]]