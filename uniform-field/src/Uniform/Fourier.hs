-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Fourier
-- | A simple outof the book  implementation of fourier transform
-- following https://www.skybluetrades.net/blog/2013/11/2013-11-13-data-analysis-fft-1.html
-- probably essentially to learn DFFT 

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


module Uniform.Fourier
    
         where

import UniformBase
import GHC.Base
import Prelude hiding (length, sum, map, zipWith, (++))
import qualified Prelude as P
import Data.Complex
import Data.List (transpose)

import Data.Vector hiding (map)
import qualified Data.Vector as V

-- one dimensional 


-- i :: Complex Double
-- i = 0 :+ 1
 
omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)



dft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
dft' sign scale h = generate bigN (((scale :+ 0) *) . doone)
  where bigN = length h
        w = omega bigN
        doone n = sum $
                  zipWith (*) h $ generate bigN (\k -> w^^(sign*n*k))

dft, idft :: Vector (Complex Double) -> Vector (Complex Double)
dft = dft' 1 1
idft v = dft' (-1) (1.0 / (fromIntegral $ length v)) v

-- for two dimensions

fromList2d :: [[Complex Double]] -> [Vector (Complex Double)]
-- fromList2d :: Vector [a] -> Vector (Vector a)
fromList2d ms = P.map fromList ms 

defuzz :: Vector (Complex Double) -> Vector (Complex Double)
-- makes close to zero to be 0 (but not other near integers)
defuzz = V.map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x   
defuzz' = P.map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x   



dft2d :: [[Complex Double]] -> [[Complex Double]]
dft2d = P.map toList . transp . P.map dft . transp . P.map dft . P.map fromList 

dft2dtest :: [[Complex Double]] -> [[Complex Double]]
dft2dtest = P.map toList . P.map dft . transp . P.map dft . transp.  P.map fromList 
-- gives the same values as the other order
idft2d :: [[Complex Double]] -> [[Complex Double]]
idft2d = P.map toList . transp . P.map idft . transp . P.map idft . P.map fromList 

transp :: [Vector a] -> [Vector a]
transp = P.map fromList . transpose . P.map toList 


-- diff_e_x =  P.zipWith (P.-)  (P.concat x22tptp') (P.concat e22tptp')

pageFourier :: ErrIO ()
pageFourier = do 
    putIOwords ["start pageFourier experiment"]

    return ()
