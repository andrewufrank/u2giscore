-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Fourier
-- | A simple outof the book  implementation of fourier transform
-- following https://www.skybluetrades.net/blog/2013/11/2013-11-13-data-analysis-fft-1.html
-- probably essentially to learn DFFT 

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

import Prelude hiding (length, sum, map, zipWith, (++))
import qualified Prelude as P
import Data.Complex
import Data.List (transpose)

import Data.Vector hiding (map)
import qualified Data.Vector as V
import Uniform.FourierRepa ( grid8_11 ) 

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

-- for two dimensions

fromList2d :: [[Complex Double]] -> [Vector (Complex Double)]
-- fromList2d :: Vector [a] -> Vector (Vector a)
fromList2d ms = P.map fromList ms 

defuzz :: Vector (Complex Double) -> Vector (Complex Double)
-- makes close to zero to be 0 (but not other near integers)
defuzz = V.map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x   

---- simple 2d example , primed in [[a]]

e22' :: [[Complex Double]]
e22' = P.map (P.map (:+ 0)) [[3,2], [1,0]]
e22 :: [Vector (Complex Double)]
e22 = P.map fromList e22'
e22t :: [Vector (Complex Double)]
e22t = P.map dft e22  -- here the transformation, later
e22t' :: [[Complex Double]]
e22t' = P.map toList e22t
e22tp' :: [[Complex Double]]
e22tp' = transpose e22t'
e22tp :: [Vector (Complex Double)]
e22tp = P.map fromList e22tp'
e22tpt = P.map dft e22tp  -- here the transformation  later
e22tptp' = transpose . P.map toList $ e22tpt

dft2d :: [[Complex Double]] -> [[Complex Double]]
dft2d = P.map toList . transp . P.map dft . transp . P.map dft . P.map fromList 
transp :: [Vector a] -> [Vector a]
transp = P.map fromList . transpose . P.map toList 

x22tptp' :: [[Complex Double]]
x22tptp' = dft2d e22'
y22' = dft2d x22tptp'


diff_e_x =  P.zipWith (P.-)  (P.concat x22tptp') (P.concat e22tptp')

pageFourier :: ErrIO ()
pageFourier = do 
    putIOwords ["start pageFourier experiment"]
    putIOwords ["e22'", showT e22']
    putIOwords ["e22", showT e22]
    putIOwords ["e22t'", showT e22t']
    putIOwords ["e22tp'", showT e22tp']
    putIOwords ["e22tp", showT e22tp]
    putIOwords ["e22tpt", showT e22tpt]
    putIOwords ["e22tptp'", showT e22tptp']
    putIOwords ["equal input without transformation ", showT (e22' == e22tptp')]
    putIOwords ["x22tptp'", showT x22tptp']

    putIOwords ["equal with transformation ", showT (x22tptp' == e22tptp')]
    putIOwords ["difference with transformation e or x", showT diff_e_x]

    putIOwords ["back", showT y22']

    -- putIOwords ["grid88", showT grid8_11]
    -- putIOwords ["j88", showT j88]
    -- putIOwords ["j88t mapped dft", showT $ P.map toList j88t]
    return ()

-- g88' ::  [[Complex Double]]
-- g88' = P.map (P.map (:+ 0) . P.take 8) $ grid8_11 
-- j88 :: [(Vector (Complex Double))]
-- j88 = fromList2d g88'
-- j88t :: [Vector (Complex Double)]
-- j88t= P.map dft j88
-- j88tu = P.map toList j88t
-- j88tup = transpose j88tu