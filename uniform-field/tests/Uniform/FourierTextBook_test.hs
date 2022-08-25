-----------------------------------------------------------------------------
--
-- Module      :  Uniform.FourierTextBook_test
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


module Uniform.FourierTextBook_test where

import           Test.Framework hiding (scale, generate)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
-- import ExampleData.Point2d
import ExampleData.TerrainLike
-- import Uniform.Point2d
import Prelude hiding (length, sum, map, zipWith, (++), null)
import qualified Prelude as P
import Data.Complex
import Data.Vector (Vector(..), toList, fromList)
import qualified Data.Vector as V
import GHC.Base

import UniformBase
import Uniform.FourierTextBook 

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

sin8 = V.generate 8 (\i -> sin (2 * pi * fromIntegral i / 8))
sin4 = V.generate 4 (\i -> sin (2 * pi * fromIntegral i / 4))

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

-- zero8 = fromList [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
--  0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0] 
peak4 = fromList [0,6,4,2]
peak4ft = fromList [12.0 :+ 0.0,
     (- 4.0) :+ 4.000000000000001,
      (- 4.0) :+ 0.0,
       (- 4.0) :+ (- 3.9999999999999982)]
test_peak8 = assertEqual peak8ft $ defuzz $ dft peak8
test_peak8r = assertEqual (V.fromList . replicate 8 $ (0 :+ 0)) $ defuzz $ V.zipWith (-) peak8 (idft peak8ft)
test_peak4 = assertEqual peak4ft $ defuzz $ dft peak4
testpeak4r = assertEqual peak4 $ defuzz $ idft peak4ft

e22vv :: [Vector (Complex Double)]
e22vv =  fromList2d e22'

test_transp2 = assertEqual e22vv (vecTransp . vecTransp $ e22vv)
test_transp8 = assertEqual g88' (toList2d . vecTransp . vecTransp . fromList2d $ g88')

-- -- test if fourier transform first row then column same as other order 
dft2dtest :: [[Complex Double]] -> [[Complex Double]]
dft2dtest = toList2d . fmap dft . vecTransp . fmap dft . vecTransp .  fromList2d 
-- gives the same values as the other order
 
diff :: [[Complex Double]] -> [Complex Double]
-- | difference between to results
diff m =defuzz' $  ((P.zipWith (-))) (concat $ dft2d m) (concat $  dft2dtest m) 
-- d44 :: [Complex Double]
-- d44 = diff h44'
 
 
test_order44 = assertEqual (replicate 16 (0 :+ 0)) (diff h44')
test_order = assertEqual (replicate 64 (0 :+ 0)) (diff g88')
-- -- imprecis numeric! 


-- test_four2d = assertEqual e22' (P.map (defuzz' . P.map ( (/4) )) . dft2d . dft2d $ e22')
-- -- test_four2d_88 = assertEqual g88' (P.map (defuzz' . P.map ( (/64) )) . dft2d . dft2d $ g88')