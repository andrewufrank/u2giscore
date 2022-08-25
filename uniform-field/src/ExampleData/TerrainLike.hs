-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TerrainLike

-- datasets which are higly simplified somewhat terrain like 
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


module ExampleData.TerrainLike
    
         where

import UniformBase
import GHC.Base
import Prelude hiding (length, sum, map, zipWith, (++))
import qualified Prelude as P
import Data.Complex
import Data.List (transpose)

import Data.Vector hiding (map)
import qualified Data.Vector as V
-- import Uniform.FourierRepa ( grid8_11 ) 
import Uniform.FourierTextBook

-- a two by two  - ste[wise done]
e22 :: [[Double]]
e22 = [[3,2], [1,0]]
e22' :: [[Complex Double]]
e22' = P.map (P.map (:+ 0)) [[3,2], [1,0]]
e22v :: [Vector (Complex Double)]
e22v = fromList2d e22'
e22t :: [Vector (Complex Double)]
e22t = P.map dft e22v  -- here the transformation, 
e22t' :: [[Complex Double]]
e22t' =  toList2d e22t
e22tp' :: [[Complex Double]]
e22tp' = transpose e22t'
e22tp :: [Vector (Complex Double)]
e22tp =  fromList2d e22tp'
e22tpt = P.map dft e22tp  -- here the transformation 
e22tptp' = transpose .   toList2d $ e22tpt

-- a 4 by 4 
h44 = [[1,2,3,4],[2,4,5,6],[1,3,2,2],[1,2,1,1]]
h44' :: [[Complex Double]]
h44' = P.map (P.map (:+ 0)) h44
h44t' = dft2d h44'
h44tt' = idft2d h44t'
h44x1 =  defuzz' . P.map (liftA (/16)) . P.concat $ h44tt' 


-- the 8 by 8 terrain

g88'' = P.map (  P.take 8) grid8_11  -- input original real
g88' = P.map (P.map (:+ 0)) g88''  -- complex

g88t' = dft2d g88' -- transformed, value seem ok compared to octave

g88tt' = idft2d g88t'
g88ttI = fmap imagPart . P.concat $ g88tt'
g88ttR = fmap (fmap realPart) $ g88tt'

pageTerrainLike :: ErrIO ()
pageTerrainLike = do 
    putIOwords ["start pageTerrainLike experiment"]
    putIOwords ["e22'", showT e22']
    putIOwords ["e22", showT e22]
    putIOwords ["e22t'", showT e22t']
    putIOwords ["e22tp'", showT e22tp']
    putIOwords ["e22tp", showT e22tp]
    putIOwords ["e22tpt", showT e22tpt]
    putIOwords ["e22tptp'", showT e22tptp']
    putIOwords ["equal input without transformation ", showT (e22' == e22tptp')]

    return ()


grid8_11 :: [[Double]]
grid8_11 = [[385,382.5,380,378.75,380.714285714286,381.785714285714,385.384615384615,390.666666666667,397,403.333333333333,407.5],
    [387.5,385,382.5,380,380.357142857143,382.692307692308,386.923076923077,392.555555555556,398.888888888889,403.015873015873,407.5],
    [390,387.5,385,382.5,380,384.230769230769,388.461538461538,394.444444444444,398.571428571429,402.698412698413,407.5],
    [392.871287128713,391.485148514851,390.09900990099,388.712871287129,387.647058823529,388.823529411765,390,394.126984126984,398.253968253968,402.5,407.5],
    [400.19801980198,398.811881188119,397.425742574257,396.039603960396,395.294117647059,396.470588235294,395.384615384615,396.666666666667,398.888888888889,402.5,407.5],
    [406.785714285714,406.071428571429,404.752475247525,403.366336633663,402.941176470588,402.307692307692,400.769230769231,401.047619047619,403.142857142857,405.575757575758,409.69696969697],
    [412.5,411.785714285714,411.071428571429,410.357142857143,409,407,406.530612244898,406.367346938776,407.333333333333,409.428571428571,413.212121212121],
    [417.727272727273,416.818181818182,415.909090909091,415,413.30612244898,413.142857142857,412.979591836735,412.816326530612,412.65306122449,413.619047619048,416.727272727273]]