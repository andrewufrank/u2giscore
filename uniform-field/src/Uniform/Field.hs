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
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.FFTW

a :: Array F DIM1 (Complex Double)
a = fromList (Z :. 4) [i :+ 0 | i <- [0..3]]

f = fft a 
b = ifft f 
blist = toList b


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