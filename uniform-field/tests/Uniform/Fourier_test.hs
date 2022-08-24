-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Field_test
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.Fourier_test where

import           Test.Framework hiding (scale, generate)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
-- import ExampleData.Point2d
-- import Uniform.Point2d
import Prelude hiding (length, sum, map, zipWith, (++), null)
import qualified Prelude as P
import Data.Complex
import Data.Vector
import UniformBase
import Uniform.Fourier 

v0 :: Vector (Complex Double)
v0 = fromList  [0 :+ 0]

spike8 = fromList [1,0,0,0,0,0,0,0]
spike8Ft = fromList [1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0,
 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0]
 -- a constant function, as expected from spike
spike4 = fromList [1,0,0,0]
spike4Ft = fromList [1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0]

test_spike8 = assertEqual spike8Ft $ dft spike8 
test_spike4 = assertEqual spike4Ft $ dft spike4 

sin8 = generate 8 (\i -> sin (2 * pi * fromIntegral i / 8))
sin4 = generate 4 (\i -> sin (2 * pi * fromIntegral i / 4))

test_sine8 = assertEqual sin8ft $ defuzz $ dft sin8 
test_sine4 = assertEqual sin4ft $ defuzz $ dft sin4 

sin8ft = fromList [
    0.0 :+ 0.0, 0.0 :+ 4.0,
     0.0 :+ 0.0, 0.0 :+ 0.0,
      0.0 :+ 0.0, 0.0 :+ 0.0,
       0.0 :+ 0.0, 0.0 :+ (- 3.9999999999999987)]

sin4ft = fromList [0.0 :+ 0.0, 0.0 :+ 2.0,
                     0.0 :+ 0.0, 0.0 :+ (- 2.0)]
-- this is the same as sin8ft halfed, dropping middle lines

peak8 = fromList [0,3,6,5,4,3,2,1]
peak8ft = fromList [24.0 :+ 0.0,
     (- 6.828427124746191) :+ 6.828427124746192,
    (- 4.000000000000001) :+ 0.0,
    (- 1.1715728752538106) :+ (- 1.1715728752538095),
      0.0 :+ 0.0,
    (- 1.1715728752538088) :+ 1.1715728752538106,
    (- 3.9999999999999973) :+ 0.0,
    (- 6.828427124746188) :+ (- 6.828427124746166)]

zero8 = fromList [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0] 
peak4 = fromList [0,6,4,2]
peak4ft = fromList [12.0 :+ 0.0,
     (- 4.0) :+ 4.000000000000001,
      (- 4.0) :+ 0.0,
       (- 4.0) :+ (- 3.9999999999999982)]
test_peak8 = assertEqual peak8ft $ defuzz $ dft peak8
test_peak8r = assertEqual zero8 $ defuzz $ zipWith (-) peak8 (idft peak8ft)
test_peak4 = assertEqual peak4ft $ defuzz $ dft peak4
testpeak4r = assertEqual peak4 $ defuzz $ idft peak4ft

-- p1 = Pnt2d 77 (V2 1 2):: Pnt2
-- v1 = V2 3 4
-- -- test construction of pnt2d
-- test_p1 = assertEqual "Pnt2d {_p2id = 77, _v2 = V2 1.0 2.0}" (showT p1)
-- test_v2zero = assertEqual "Pnt2d {_p2id = 0, _v2 = V2 0.0 0.0}" (showT (zero::Pnt2))
 
test_transp2 = assertEqual e22 (transp . transp $ e22)
test_transp8 = assertEqual g88' (P.map toList . transp . transp . P.map fromList $ g88')

-- test if fourier transform first row then column same as other order 
test_order44 = assertEqual (dft2d h44') (dft2dtest h44') 
-- imprecis numeric! 


test_four2d = assertEqual e22' (P.map (defuzz' . P.map ( (/4) )) . dft2d . dft2d $ e22')
-- test_four2d_88 = assertEqual g88' (P.map (defuzz' . P.map ( (/64) )) . dft2d . dft2d $ g88')