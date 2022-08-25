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
import Extra
import qualified Data.Array.Comfort.Boxed as C
import  Data.Array.Comfort.Boxed (fromList, toList)
import Data.Array.Comfort.Shape
import Numeric.FFTW.Rank2
import qualified Data.Array.Comfort.Boxed.Unchecked
import qualified Data.Array.Comfort.Storable.Private as Pr
-- import Data.Array.Repa hiding (map)
-- import Data.Array.Repa.Eval
-- import Data.Array.Repa.Repr.ForeignPtr
-- import Data.Array.Repa.FFTW
-- -- import Data.Repa.Array
import ExampleData.TerrainLike
import GHC.Float (int2Double)

-- storable frequency amplitude Fourier transformed 
data FourierTransformed = FourierTransformed 
    {  rows, cols :: Int
    , mat :: [(Complex Double)]
    }
    deriving (Show, Read,   Eq, Generic)

-- forward Fourier transformation of a 2d matrix 
dfttw2d :: Int -> Int -> [[ Double]] -> FourierTransformed
    -- Pr.Array (Cyclic Integer, Cyclic Integer) (Complex Double)
dfttw2d m n mat =  FourierTransformed m n . Pr.toList 
        . fourier Forward 
        . Pr.fromList (Cyclic m, Cyclic n) . map (:+ 0). concat $ mat

-- inverse Fourier transformation restoring a 2d matrix 
idfttw2d :: FourierTransformed -> [[Double]]
idfttw2d (FourierTransformed m n mat) = 
        createMatrix m  
            . map (* scale) 
            . map ( realPart)  --(/(fromIntegral m*n)) .
            . Pr.toList . fourier Backward 
            . Pr.fromList (Cyclic m, Cyclic n) $ mat 
    where   scale :: Double 
            scale = 1/(int2Double (m*n))

-- helper 
createMatrix :: Int -> [a] -> [[a]]
createMatrix _ [] = []
createMatrix n xs = take n xs : createMatrix n (drop n xs)


-- a = fromList (ZeroBased 8, ZeroBased 8) grid88 
-- al :: [Complex Double]
-- al = toList a 
-- ash = C.shape a
-- shape88 :: (ZeroBased Int,  ZeroBased Int)
-- shape88 = (ZeroBased 8, ZeroBased 8)
-- shapeCyc =   (Cyclic 8, Cyclic 8)
-- -- ac :: C.Array (Cyclic Integer, Cyclic Integer) (Complex Double)
-- ac
--   :: Data.Array.Comfort.Boxed.Unchecked.Array
--        (Data.Array.Comfort.Shape.Cyclic Integer,
--         Data.Array.Comfort.Shape.Cyclic Integer)
--        (Data.Complex.Complex Double)
-- ac = C.reshape shapeCyc a 
-- -- acf = Pr.freeze a  -- gives error

-- -- versuch mit direktem einlesen 
-- p88 :: Pr.Array (Cyclic Integer, Cyclic Integer) (Complex Double)
-- p88 = Pr.fromList shapeCyc grid88

        
-- q44 = dfttw2d 4 4 h44
-- r44 = idfttw2d q44

-- p88t = fourier Forward p88
-- q88t = dfttw2d 8 8 grid8_11
-- p88tt = fourier Backward p88t
-- p88tt' = Pr.toList p88tt
-- p88tts' = map (/64) p88tt'
-- p88s = createMatrix 8 p88tts'

-- r88 = fromList shape88 p88tt'
-- not helping, single list (with indication of size in head)

-- matrixEvery :: Int -> [a] -> [[a]]
-- matrixEvery _ [] = []
-- matrixEvery n xs = as : matrixEvery n bs
--   where (as,bs) = matrixEvery n xs


-- a :: Array F DIM1 (Complex Double)
-- a = fromList (Z :. 4) [i :+ 0 | i <- [0..3]]
-- a8 = fromList (Z :. 8) [i :+ 0 | i <- [0..7]]
-- f = fft a 
-- f8 = fft a8
-- b :: Array F DIM1 (Complex Double)
-- b = ifft f 
-- b8 :: Array F DIM1 (Complex Double)
-- b8 = ifft f8
-- blist :: [Complex Double]
-- blist = toList b
-- b8list = toList b8
-- f8list = toList f8
-- f8_4list = take 4 . toList $ f8
-- f8_4fromList = fromList (Z :.4) f8_4list 
-- b8_4 :: Array F DIM1 (Complex Double)
-- b8_4 = ifft f8_4fromList 
-- b8_4res :: [Complex Double]
-- b8_4res=toList b8_4

-- g88:: Array F DIM2 (Complex Double)
-- g88 = fromList (Z :. 8 :. 11) grid88

-- g88f :: Array F DIM2 (Complex Double)
-- g88f = fft2d g88 
-- g88list :: [Complex Double]
-- g88list = toList g88f
-- -- g88list2 :: [[Complex Double]]
-- -- g88list2 = toList g88f
-- g88list3 = toList g88f
-- >>> toList a
-- [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]
-- >>> toList $ fft a
-- [6.0 :+ 0.0,(-2.0) :+ 2.0,(-2.0) :+ 0.0,(-2.0) :+ (-2.0)]
-- >>> toList $ ifft $ fft a
-- [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3


pageComfort1 :: ErrIO ()
pageComfort1 = do 
    putIOwords ["start pageComfort1 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

-- grid88 :: [Complex Double]
-- grid88 = map (:+ 0) . concat $ map (take 8) grid8_11
