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
    --   fourV2, fiveV2,
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

-- -- import  qualified         Algebra.Laws             as Law
-- import Data.List ( nub ) 

-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

--- example code  -- Minimal Schema

tessShort4 = makeCatFrom fourV2

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

pageHQfaces_test :: ErrIO ()
pageHQfaces_test = do
    -- mainMakeTessShort

    -- tessShort4 <- liftIO $ delaunay2 fourV2    
    -- let tessShort4 = delaunay2 fourV2

    -- putIOwords ["\n [pageTriple4cat"]
    -- putIOwords ["pointsF400", shownice pointsF400]
    -- putIOwords ["pointsF401", shownice pointsF401]
    -- putIOwords ["pointsD400", shownice pointsD400]

    -- let tessShort5 = delaunay2 fiveV2  

    -- putIOwords ["pointsF500_2\n", shownice $ pointsF500_2 tessShort5]
    -- putIOwords ["pointsF501_2\n", shownice $ pointsF501_2 tessShort5]
    -- putIOwords ["pointsF502_2\n", shownice $ pointsF502_2 tessShort5]
    -- putIOwords ["pointsF503_2\n", shownice $ pointsF503_2 tessShort5]
    return () 

    
    -- old 
    -- putIOwords ["tessShort5", shownice $   tessShort5]
    -- putIOwords ["hqfacet", shownice $ hqfacet tessShort5]
    -- putIOwords ["hqfacet", shownice hqface]
    -- putIOwords ["hqnodet\n", showlong $ hqnodet tessShort5]
    -- putIOwords ["xyPointt\n", showlong $ xyPointt tessShort5]
    -- putIOwords ["facePoint2t\n", showT $ facePoint2t tessShort5]
--    putIOwords ["pointsF501_2", shownice $ pointsF501_2 tessShort5]
--    putIOwords ["pointsF501_2", shownice $ pointsF501_2 tessShort5]
--    putIOwords ["pointsF501_2", shownice $ pointsF501_2 tessShort5]
--     putIOwords ["ts one", showT x1]

    -- putIOwords ["CatStore empty", shownice v0]
    -- putIOwords ["CatStore v1 with cp1", shownice v1]
    -- putIOwords ["CatStore v2 added cp2, deleted cp1", shownice v2]
    -- putIOwords ["CatStore a1x added batch cp1 cp2", shownice a1x]
    -- putIOwords ["CatStore  a2x", shownice a2x]
    -- putIOwords ["CatStore  v2a", shownice v2a]
    -- page2

test_x1 = do
        res <- runErr $  pageHQfaces_test 
                -- return True
        assertEqual (Right ()) res  -- does not produce output
