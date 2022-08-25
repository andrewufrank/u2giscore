-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Fourier
-- | A simple outof the book  implementation of fourier transform
-- following https://www.skybluetrades.net/blog/2013/11/2013-11-13-data-analysis-fft-1.html
-- from Copyright (2013) Ian Ross
-- probably essentially to learn DFFT 
-- because no optimizations
-- an optimized version (starting from the same code)
-- https://hackage.haskell.org/package/arb-fft

-- extended to include 2d transformations 
-- as combinations of twice applyint 1d transformations

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


module Uniform.FourierTextBook
    
         where

import UniformBase
import GHC.Base
import Prelude hiding (length, sum, map, zipWith, (++))
import qualified Prelude as P
import Data.Complex
import Data.List (transpose)

-- import Data.Vector hiding (map)
import Data.Vector (Vector(..), toList, fromList)
import qualified Data.Vector as V

-- one dimensional - first the base formula 'out of the book'
 
omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

dft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
dft' sign scale h = V.generate bigN (((scale :+ 0) *) . doone)
  where bigN = V.length h
        w = omega bigN
        doone n = V.sum $
-- import Data.Vector hiding (map)
                  V.zipWith (*) h $ V.generate bigN (\k -> w^^(sign*n*k))

dft, idft :: Vector (Complex Double) -> Vector (Complex Double)
dft = dft' 1 1
-- ^ the transformation from space domain to frequency domain
idft v = dft' (-1) (1.0 / (fromIntegral $ V.length v)) v
-- ^ inverst transformation, from frequency domain to space domain


-- for two dimensions - a list of vectors 
-- could be a Vector (Vector) -- but this was simpler to code 

-- helpers:

fromList2d :: [[Complex Double]] -> [Vector (Complex Double)]
-- fromList2d :: Vector [a] -> Vector (Vector a)
fromList2d ms = fmap fromList  ms 
toList2d ::  [Vector (Complex Double)] -> [[Complex Double]]
-- convert [Vector (Complex Double)] -> [[Complex Double]]
toList2d ms = fmap toList ms 

defuzz :: Vector (Complex Double) -> Vector (Complex Double)
-- | makes close to zero to be 0 (but not other near integers)
defuzz = V.map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x   
defuzz' :: [Complex Double] -> [Complex Double]
defuzz' = fmap (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x   


dft2d :: [[Complex Double]] -> [[Complex Double]]
-- forward fourier transformation
dft2d = toList2d . vecTransp . fmap dft . vecTransp . fmap dft . fromList2d 



idft2d :: [[Complex Double]] -> [[Complex Double]]
-- | inverse 2d fourier transformation
idft2d = toList2d . vecTransp . fmap idft . vecTransp . fmap idft . fromList2d 

vecTransp :: [Vector (Complex Double)] -> [Vector (Complex Double)]
-- | transpose a Vector (by toList, fromList)
vecTransp = fromList2d . transpose . toList2d 


-- diff_e_x =  fzipWith (f-)  (fconcat x22tptp') (fconcat e22tptp')

pageFourier :: ErrIO ()
pageFourier = do 
    putIOwords ["start pageFourier experiment"]

    return ()
