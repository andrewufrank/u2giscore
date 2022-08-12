 -----------------------------------------------------------------------------
--
-- Module      :  Test Naive Triple Store 
--          with a minimal Schema:
-- the tag of the sum type is the constructor for the node id 
--

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , UndecidableInstances     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module ExampleData.HQfaces_test
    where

import           Test.Framework hiding (scale, (.&.))

import UniformBase  
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.Point2d ()
import Uniform.Point2dData
import Uniform.TesselationHalfQuads
  
import Uniform.TripleRels
import Data.List.Extra
import Uniform.Drawings
import Control.Monad.State  

 
tess44short = makeCatFrom fourPnt2d 
tess55short = makeCatFrom fivePnt2d 


-- coords2faces :: (MonadState m) => CatStoreTessShort -> m [(IDtype, [V2D])]
coords2faces :: StateT CatStoreTessShort Identity [(IDtype, [V2D])]
coords2faces = do 
    f <- inv2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  (f .&. n .&. xy)
    return $ map onef . groupSort $ fp3 

onef (Face i, pts) = (i, map (unName . unPointTag) pts)


(coords2faces_4) = evalState coords2faces tess44short
(coords2faces_5) = evalState coords2faces tess55short


(.&.) = flip compRel  


 
-- points12 :: StateT CatStoreTessShort Identity (Rel2 ObjTessShort, Rel2 ObjTessShort)
points12 :: StateT
  CatStoreTessShort
  Identity
  [(ObjTessShort, (ObjTessShort, ObjTessShort))]
points12 = do 
    hqn <- rel2 HqNode 
    xy <- rel2 XY 
    twin <- rel2 Twin 
    return (compRelZip (hqn .&. xy) (twin .&. hqn .&. xy))


point1s :: [(ObjTessShort, (ObjTessShort, ObjTessShort))]
(point1s) = evalState points12 tess44short

dist12 :: [(ObjTessShort, Double)]
dist12 = map dist12one point1s

dist12one :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> (ObjTessShort, Double) 
dist12one (a,(p1,p2)) = (a, (distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)))
-- zipWith distance (map (unPointTag . snd) point1s) 
--         (map (unPointTag . snd) point2s)



instance NiceStrings Float where shownice = showT 

pageHQfaces_test :: ErrIO ()
pageHQfaces_test = do
    putIOwords ["the tests for relations after storing four and five points"]
    -- putIOwords ["tess44short\n", showlong tess44short, "\n"    ]
    putIOwords ["the end coord of the hqs", showAsLines point1s]
    -- let hqNodeRel = getRel tess44short HqNode
    -- putIOwords ["([hqNodeRel])\n", showlong hqNodeRel]
    -- let nodeXY =   (getRel tess44short XY)
    -- putIOwords ["([nodeXY])\n", showlong nodeXY]
    -- let hqHqNodeXY =  hqNodeRel .&.nodeXY
    -- putIOwords ["([hqHqNodeXY]) the quad with the coord of its node\n", showlong hqHqNodeXY]
    -- here ok the hq headnode 
    
    -- let hqtwinhq = (getRel tess44short Twin)    
    -- putIOwords ["hqtwinhq the quad with its twin \n", showlong hqtwinhq]
    -- -- let hqtwinhqConv = converseRel   hqtwinhq 
    -- -- putIOwords ["hqtwinhqConv twin hq left \n", showlong hqtwinhqConv]
    -- let hqtwinhqNode = hqtwinhq  .&. (getRel tess44short HqNode) 
    -- putIOwords ["hqtwinhqNode\n", showlong hqtwinhqNode]
    -- let hqtwinhqNodeXY = hqtwinhqNode .&. nodeXY 
    -- putIOwords ["hqtwinhqNodeXY the coords of the other end\n", showlong hqtwinhqNodeXY]
    
    -- let start_end = zip (map snd hqHqNodeXY) (map snd hqtwinhqNodeXY)
    -- putIOwords ["start_end the points", showlong start_end]

    -- let ds = map (uncurry distance) . map (cross (unPointTag, unPointTag) ) $     start_end 
    -- putIOwords ["the distances ", showT ds]
    putIOwords ["the distances2 ", showT dist12]


  
    return () 