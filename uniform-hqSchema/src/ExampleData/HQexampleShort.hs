-----------------------------------------------------------------------------
--
-- Module      :  Uniform. 
--      convert the HQ produced by geometry 
--      to a list of load instructions 
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
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
{-# HLINT ignore "Redundant $" #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module ExampleData.HQexampleShort where

-- import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import UniformBase
import Uniform.GeometryFunctions
import ExampleData.Point2d
import HQschema.HQschemaShort
-- import Uniform.TripleStore
-- import Uniform.Point2d
import Uniform.TesselationHalfQuads
import Uniform.NaiveTripleStore
import HQschema.HQschemaTop

--- Make tirples 



-- makeTripFace :: Int -> FaceHQ -> StoreTessShortElement
-- ^ convert to trip; contains only circumcenter
-- dual to node 
-- makeTripFace  i (FaceHQ j) =    (  Face $ i, XY,  zero)
-- is constructed later from hq 
        -- , XY, PointTag . toPnv2 . circumcenter $ fhq)
        -- compute later and fill 


-- sub: make triples for HQ 

-- makeTripHq :: Int -> Int -> HQdataHQ -> [StoreTessShortElement]
-- -- convert the HQ data to StoreTessShortElements
-- makeTripHq offset i hq = catMaybes [hqnode,  hqface, hqtwin, hqhalflength]
--     where
--         hqid =   HalfQuad $ i 
--         hqnode, hqface, hqtwin, hqhalflength :: Maybe StoreTessShortElement
--         hqnode = Just $ (hqid, HqNode,  Node   . (+offset) . hq_node $ hq)
--         hqface =   fmap  (\fi -> (hqid, HqFace,   Face  . (+offset) $ fi)) (Just . hq_face  $ hq)
--         -- in qhull is outerface = Nothing
--         -- in hgeometry is outerface just one of the faces
--         hqnext =  (hqid, NextHq,   HalfQuad  . (+offset) . hq_orbitNext $   hq)        
--         hqtwin = Just $ (hqid, Twin, HalfQuad  . (+offset) . hq_twin $ hq)
--         hqhalflength = Nothing 
--             -- Just $ (hqid, Dist, LengthTag . Length . halflength $ hq) 


-- hqToTrip :: Int -> TesselationHQ ->  TessShortHQtriples
-- hqToTrip offset teshq  = TessShortHQtriples
--     { _NodesTrip = zipWith (makeTripNode) [offset ..] (_Nodes teshq) 
--     -- , _FacesTrip = zipWith (makeTripFace) [offset ..] (_Faces teshq)
--     -- are constructed later 
--     , _FacesTrip = []
--     , _HQtrips   = concat $ zipWith (makeTripHq offset)   [offset ..] (_HQdatas teshq)
--     } 


-- -- cat400 :: CatStoreTessShort
-- -- cat400 = catStoreEmpty
-- -- cat401 :: CatStore ObjPoint MorphTessShort
-- intoCat :: [(StoreTessShortElement)] -> CatStoreTessShort
-- intoCat ts = catStoreBatch (map wrapIns ts) $ catStoreEmpty -- cat400


-- makeCat :: 

-- makeCatFrom offset pnts = intoCat . getAllTrips . hqToTrip offset . toHq1 . delaunay2 $ pnts
mainMakeTessShort :: ErrIO () 
mainMakeTessShort = do 
    putIOwords ["\nmainDelaunayTriples TessShort\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    -- tessShort4 <- liftIO $ delaunay2 fourPnt2d    
    let tessShort4 = delaunay2 fourPnt2d
    let trips4 = hqToTrip 400 . toHq1 $ tessShort4 
    -- putIOwords ["triples produces\n", showT trips]
    let res4 = intoCat (getAllTrips trips4) 
    putIOwords ["tessShort triple store  produced\n", shownice res4]

    -- tessShort5 <- liftIO $ delaunay2 fivePnt2d  
    let tessShort5 = delaunay2 fivePnt2d  
    let trips5 = hqToTrip 500 . toHq1 $ tessShort5 
    -- putIOwords ["triples produces\n", showT trips]
    let res5 = intoCat (getAllTrips trips5) 
    putIOwords ["tessShort triple store  produced\n", shownice res5]
    return ()

{-
tessShort4  = CatStoreK [
        (Node 400,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 0.0 0.0})),
        (Node 401,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 1.5 1.5})),
        (Node 402,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 0.0 2.0})),
        (Node 403,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 2.0 0.0})),
        (Face 400,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 1.0 0.5})),
        (Face 401,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 0.5 1.0})),
        (HalfQuad 400,HqNode,Node 401),
        (HalfQuad 400,Twin,HalfQuad 405),
        (HalfQuad 400,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 401,HqNode,Node 400),
        (HalfQuad 401,HqFace,Face 400),
        (HalfQuad 401,Twin,HalfQuad 406),
        (HalfQuad 401,Dist,LengthTag (Length 1.0)),
        (HalfQuad 402,HqNode,Node 400),
        (HalfQuad 402,HqFace,Face 401),
        (HalfQuad 402,Twin,HalfQuad 407),
        (HalfQuad 402,Dist,LengthTag (Length 1.0606601717798212)),
        (HalfQuad 403,HqNode,Node 401),
        (HalfQuad 403,HqFace,Face 401),
        (HalfQuad 403,Twin,HalfQuad 408),
        (HalfQuad 403,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 404,HqNode,Node 400),
        (HalfQuad 404,Twin,HalfQuad 409),
        (HalfQuad 404,Dist,LengthTag (Length 1.0)),
        (HalfQuad 405,HqNode,Node 403),
        (HalfQuad 405,HqFace,Face 400),
        (HalfQuad 405,Twin,HalfQuad 400),
        (HalfQuad 405,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 406,HqNode,Node 403),
        (HalfQuad 406,Twin,HalfQuad 401),
        (HalfQuad 406,Dist,LengthTag (Length 1.0)),
        (HalfQuad 407,HqNode,Node 401),
        (HalfQuad 407,HqFace,Face 400),
        (HalfQuad 407,Twin,HalfQuad 402),
        (HalfQuad 407,Dist,LengthTag (Length 1.0606601717798212)),
        (HalfQuad 408,HqNode,Node 402),
        (HalfQuad 408,Twin,HalfQuad 403),
        (HalfQuad 408,Dist,LengthTag (Length 0.7905694150420949)),
        (HalfQuad 409,HqNode,Node 402),
        (HalfQuad 409,HqFace,Face 401),
        (HalfQuad 409,Twin,HalfQuad 404),
        (HalfQuad 409,Dist,LengthTag (Length 1.0))
        ]

tessShort5 = CatStoreK [
    (Node 500,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 0.0 0.0})),
    (Node 501,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 3.0 0.0})),
    (Node 502,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 4.0 2.0})),
    (Node 503,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 3.0 5.0})),
    (Node 504,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 0.0 3.0})),
    (Face 500,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 2.1363636363636362 3.0454545454545454})),
    (Face 501,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 1.5 1.5})),
    (Face 502,XY,PointTag (Pnv2d {_p2id = "", _v2 = V2 1.8333333333333335 1.8333333333333333})),
    (HalfQuad 500,HqNode,Node 503),
    (HalfQuad 500,HqFace,Face 500),
    (HalfQuad 500,Twin,HalfQuad 507),
    (HalfQuad 500,Dist,LengthTag (Length 1.8027756377319946)),
    (HalfQuad 501,HqNode,Node 502),
    (HalfQuad 501,HqFace,Face 502),
    (HalfQuad 501,Twin,HalfQuad 508),
    (HalfQuad 501,Dist,LengthTag (Length 2.0615528128088303)),
    (HalfQuad 502,HqNode,Node 502),
    (HalfQuad 502,HqFace,Face 500),
    (HalfQuad 502,Twin,HalfQuad 509),
    (HalfQuad 502,Dist,LengthTag (Length 1.5811388300841898)),
    (HalfQuad 503,HqNode,Node 501),
    (HalfQuad 503,Twin,HalfQuad 510),
    (HalfQuad 503,Dist,LengthTag (Length 2.1213203435596424)),
    (HalfQuad 504,HqNode,Node 500),
    (HalfQuad 504,Twin,HalfQuad 511),
    (HalfQuad 504,Dist,LengthTag (Length 1.5)),
    (HalfQuad 505,HqNode,Node 500),
    (HalfQuad 505,HqFace,Face 501),
    (HalfQuad 505,Twin,HalfQuad 512),
    (HalfQuad 505,Dist,LengthTag (Length 1.5)),
    (HalfQuad 506,HqNode,Node 501),
    (HalfQuad 506,HqFace,Face 502),
    (HalfQuad 506,Twin,HalfQuad 513),
    (HalfQuad 506,Dist,LengthTag (Length 1.118033988749895)),
    (HalfQuad 507,HqNode,Node 504),
    (HalfQuad 507,Twin,HalfQuad 500),
    (HalfQuad 507,Dist,LengthTag (Length 1.8027756377319946)),
    (HalfQuad 508,HqNode,Node 504),
    (HalfQuad 508,HqFace,Face 500),
    (HalfQuad 508,Twin,HalfQuad 501),
    (HalfQuad 508,Dist,LengthTag (Length 2.0615528128088303)),
    (HalfQuad 509,HqNode,Node 503),
    (HalfQuad 509,Twin,HalfQuad 502),
    (HalfQuad 509,Dist,LengthTag (Length 1.5811388300841898)),
    (HalfQuad 510,HqNode,Node 504),
    (HalfQuad 510,HqFace,Face 502),
    (HalfQuad 510,Twin,HalfQuad 503),
    (HalfQuad 510,Dist,LengthTag (Length 2.1213203435596424)),
    (HalfQuad 511,HqNode,Node 504),
    (HalfQuad 511,HqFace,Face 501),
    (HalfQuad 511,Twin,HalfQuad 504),
    (HalfQuad 511,Dist,LengthTag (Length 1.5)),
    (HalfQuad 512,HqNode,Node 501),
    (HalfQuad 512,Twin,HalfQuad 505),
    (HalfQuad 512,Dist,LengthTag (Length 1.5)),
    (HalfQuad 513,HqNode,Node 502),
    (HalfQuad 513,Twin,HalfQuad 506),
    (HalfQuad 513,Dist,LengthTag (Length 1.118033988749895))
            ]
            -}