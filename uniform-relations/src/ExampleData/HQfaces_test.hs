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

import UniformBase hiding (Rel)
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.Point2d
import Uniform.Point2dData
import Uniform.TesselationHalfQuads
    -- ( delaunay2,
    --   fourPnt2d, fivePnt2d,
    --   toHq1,
    --   FaceHQ(circumcenter),
    --   HQ(node, face, twin, halflength),
    --   NodeHQ(..),
    --   TesselationHQ(_Nodes, _Faces, _HQs) )
-- import Uniform.NaiveTripleStore
-- -- import Uniform.Object 
-- -- import Storable.Value
-- import Uniform.TripleStore
import Uniform.TripleRels
import Data.List.Extra
-- -- import  qualified         Algebra.Laws             as Law
-- import Data.List ( nub ) 

-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

--- example code  -- Minimal Schema

tessShort4 = makeCatFrom fourPnt2d

hqface :: Rel ObjTessShort
hqface = getRel tessShort4 HqFace 
hqnode = getRel tessShort4 HqNode
hqfaceInv = converseRel hqface
hqnodeInv = converseRel hqnode
xyPoint = getRel tessShort4 XY 

(.&.) = flip compRel  
-- faceNode = compRel hqnode hqfaceInv
faceNode2 = hqfaceInv `semicolon` hqnode
-- facePoint = compRel xyPoint faceNode 
facePoint2 =  hqfaceInv .&. hqnode `semicolon` xyPoint 
nodePoint2 =  hqnodeInv `semicolon` hqface `semicolon` xyPoint 
pointsFace400 = filter (( Face 400==).fst) facePoint2
pointsFace401 = filter (( Face 401==).fst) facePoint2

--- as functions in tess 
hqfacet ::  CatStoreTessShort -> Rel ObjTessShort
hqfacet tess = getRel tess  HqFace 

hqnodet tess = getRel tess  HqNode

hqfaceInvt = converseRel . hqfacet 

hqnodeInvt = converseRel . hqnodet 

xyPointt tess = getRel tess  XY 

facePoint2t tess =  hqfaceInvt tess .&. hqnodet tess `semicolon` xyPointt tess 

pointsF500_2 tess = filter (( Face 500==).fst) (facePoint2t tess )
pointsF501_2 tess = filter (( Face 501==).fst) (facePoint2t tess )
pointsF502_2 tess = filter (( Face 502==).fst) (facePoint2t tess )
pointsF503_2 tess = filter (( Face 503==).fst) (facePoint2t tess )
    

pointsDualFace400 = filter ((( (Node 400))==).fst) nodePoint2
-- pointsF400 :: [Double]
pointsF400 = map ( (scale 20) . toV2 .  unPointTag . snd) pointsFace400 
-- [[0.0,0.0],[2.0,0.0],[1.5,1.5]]
pointsF401 = map ( (scale 20) . toV2 .  unPointTag . snd) pointsFace401 
pointsD400 = map ( (scale 20) . toV2 .  unPointTag . snd) pointsDualFace400 

tess44short = makeCatFrom fourPnt2d 
getRel4 = getRel tess44short  -- similar to monadic??

point1s = (getRel4 HqNode) .&. (getRel4 XY)
point2s = (getRel4 Twin) .&. (getRel4 HqNode) .&. (getRel4 XY)
dist12 = zipWith distance (map (unPointTag . snd) point1s) 
        (map (unPointTag . snd) point2s)

face3inv = converseRel (getRel4 HqFace) 
node3 = face3inv .&. (getRel4 HqNode)
face_pnt3 = node3 .&. (getRel4 XY)

facesGrouped = groupBy (\a b -> fst a == fst b) face_pnt3
facesXY = map (map oneFacexy) facesGrouped
    where 
oneFacexy (Face i, PointTag p) = (i, unName p)

facesXYlist :: [(ObjTessShort, [ObjTessShort])]
facesXYlist = groupSort  face_pnt3
faces_gloss :: [(ObjTessShort, [ObjTessShort])] -> [(IDtype, [GlossPoint])]
faces_gloss ips = map onef ips 
    where
        onef (Face i, pts) = (i, map (toGloss . unPointTag) pts)

-- faces4gloss :: [[(IDtype, [GlossPoint])]]
faces4gloss =  faces_gloss facesXYlist 

pageHQforglossFaces :: ErrIO ()
pageHQforglossFaces = do 
    putIOwords ["get the faces for tess4"]
    putIOwords ["tess44short\n", showlong tess44short, "\n"    ]
    putIOwords ["face_pnt3\n", showlong face_pnt3, "\n"    ]




-- compLength = map distance a 
-- run in repl with runErr pageHQfaces_test 
pageHQfaces_test :: ErrIO ()
pageHQfaces_test = do
    putIOwords ["the tests for relations after storing four and five points"]
    putIOwords ["tess44short\n", showlong tess44short, "\n"    ]
    let hqNodeRel = getRel tess44short HqNode
    putIOwords ["([hqNodeRel])\n", showlong hqNodeRel]
    let nodeXY =   (getRel tess44short XY)
    putIOwords ["([nodeXY])\n", showlong nodeXY]
    let hqHqNodeXY =  hqNodeRel .&.nodeXY
    putIOwords ["([hqHqNodeXY]) the quad with the coord of its node\n", showlong hqHqNodeXY]
    -- here ok the hq headnode 
    
    let hqtwinhq = (getRel tess44short Twin)    
    putIOwords ["hqtwinhq the quad with its twin \n", showlong hqtwinhq]
    -- let hqtwinhqConv = converseRel   hqtwinhq 
    -- putIOwords ["hqtwinhqConv twin hq left \n", showlong hqtwinhqConv]
    let hqtwinhqNode = hqtwinhq  .&. (getRel tess44short HqNode) 
    putIOwords ["hqtwinhqNode\n", showlong hqtwinhqNode]
    let hqtwinhqNodeXY = hqtwinhqNode .&. nodeXY 
    putIOwords ["hqtwinhqNodeXY the coords of the other end\n", showlong hqtwinhqNodeXY]
    
    let start_end = zip (map snd hqHqNodeXY) (map snd hqtwinhqNodeXY)
    putIOwords ["start_end the points", showlong start_end]

    let ds = map (uncurry distance) . map (cross (unPointTag, unPointTag) ) $     start_end 
    putIOwords ["the distances ", showT ds]
    putIOwords ["the distances2 ", showT dist12]


  
    return () 