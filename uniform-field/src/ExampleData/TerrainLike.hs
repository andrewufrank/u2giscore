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
import Uniform.FourierRepa ( grid8_11 ) 
import Uniform.Fourier

-- a two by two 
e22 :: [[Complex Double]]
e22 = [[3,2], [1,0]]
e22' :: [[Complex Double]]
e22' = P.map (P.map (:+ 0)) [[3,2], [1,0]]
e22v :: [Vector (Complex Double)]
e22v = P.map fromList e22'
e22t :: [Vector (Complex Double)]
e22t = P.map dft e22v  -- here the transformation, later
e22t' :: [[Complex Double]]
e22t' = P.map toList e22t
e22tp' :: [[Complex Double]]
e22tp' = transpose e22t'
e22tp :: [Vector (Complex Double)]
e22tp = P.map fromList e22tp'
e22tpt = P.map dft e22tp  -- here the transformation  later
e22tptp' = transpose . P.map toList $ e22tpt

-- a 4 by 4 
h44 = [[1,2,3,4],[2,4,5,6],[1,3,2,2],[1,2,1,1]]
h44' :: [[Complex Double]]
h44' = P.map (P.map (:+ 0)) h44
h44t' = dft2d h44'
h44tt' = idft2d h44t'
h44x1 =  defuzz' . P.map (liftA (/16)) . P.concat $ h44tt' 


x22tptp' :: [[Complex Double]]
x22tptp' = dft2d e22'
y22' = dft2d x22tptp'

-- the 8 by 8 terrain

g88'' = P.map (  P.take 8) grid8_11  -- input original real
g88' = P.map (P.map (:+ 0)) g88''  -- complex

g88t' = dft2d g88' -- transformed, value seem ok compared to octave

g88tt' = idft2d g88t'
g88ttI = fmap imagPart . P.concat $ g88tt'
g88ttR = fmap (fmap realPart) $ g88tt'


diff_e_x =  P.zipWith (P.-)  (P.concat x22tptp') (P.concat e22tptp')

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
    putIOwords ["x22tptp'", showT x22tptp']

    putIOwords ["equal with transformation ", showT (x22tptp' == e22tptp')]
    putIOwords ["difference with transformation e or x", showT diff_e_x]

    putIOwords ["back", showT y22']

    -- putIOwords ["grid88", showT grid8_11]
    -- putIOwords ["j88", showT j88]
    -- putIOwords ["j88t mapped dft", showT $ P.map toList j88t]
    return ()


-- g88tt' = P.map (P.map (/(8*8))). P.map ( P.map realPart) . dft2d $ g88t'
-- d88 =  (P.zipWith (-)) (P.concat g88'')  (P.concat g88tt')




-- j88 :: [(Vector (Complex Double))]
-- j88 = fromList2d g88'
-- j88t :: [Vector (Complex Double)]
-- j88t= P.map dft j88
-- j88tu = P.map toList j88t
-- j88tup = transpose j88tu