 -----------------------------------------------------------------------------
--
-- Module      :  Test loading the tesselation in store  
--          with a minimal Schema:
--  
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
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant $" #-}

module HQschema.LoadTesselation_test
    where

import           Test.Framework hiding (scale, (.&.))

import UniformBase  
 
-- import ExampleHQ.Schema
import ExampleHQ.SchemaInstances
 
import HQgeneric.LoadTesselation
import HQgeneric.LoadDual
import Uniform.SchemaFoundation  
import ExampleData.Point2d

test_41 = assertEqual res41 $ country41
test_51 = assertEqual res51 $ country51

country41, country51:: Store ObjCountry MorphCountry  -- primal from tesselation
country41 = makeCatFrom 400 fourPnt2d 
country51 = makeCatFrom 500 fivePnt2d 

test_42 = assertEqual res42 $ country42
test_52 = assertEqual res52 $ country52

country42, country52:: Store ObjCountry MorphCountry  -- with dual 
country42 = addDual country41
country52= addDual country51

res41 = Store [(XY,(Node 400,PointTag (Pnt2d {_p2id = 11, _v2 = V2 0.0 0.0}))),(XY,(Node 401,PointTag (Pnt2d {_p2id = 12, _v2 = V2 1.5 1.5}))),(XY,(Node 402,PointTag (Pnt2d {_p2id = 13, _v2 = V2 0.0 2.0}))),(XY,(Node 403,PointTag (Pnt2d {_p2id = 14, _v2 = V2 2.0 0.0}))),(HqNode,(HalfQuad 400,Node 402)),(HqFace,(HalfQuad 400,Face 400)),(Twin,(HalfQuad 400,HalfQuad 401)),(HqNode,(HalfQuad 401,Node 400)),(HqFace,(HalfQuad 401,Face 401)),(Twin,(HalfQuad 401,HalfQuad 400)),(HqNode,(HalfQuad 402,Node 403)),(HqFace,(HalfQuad 402,Face 401)),(Twin,(HalfQuad 402,HalfQuad 403)),(HqNode,(HalfQuad 403,Node 400)),(HqFace,(HalfQuad 403,Face 402)),(Twin,(HalfQuad 403,HalfQuad 402)),(HqNode,(HalfQuad 404,Node 401)),(HqFace,(HalfQuad 404,Face 402)),(Twin,(HalfQuad 404,HalfQuad 405)),(HqNode,(HalfQuad 405,Node 400)),(HqFace,(HalfQuad 405,Face 400)),(Twin,(HalfQuad 405,HalfQuad 404)),(HqNode,(HalfQuad 406,Node 403)),(HqFace,(HalfQuad 406,Face 402)),(Twin,(HalfQuad 406,HalfQuad 407)),(HqNode,(HalfQuad 407,Node 401)),(HqFace,(HalfQuad 407,Face 401)),(Twin,(HalfQuad 407,HalfQuad 406)),(HqNode,(HalfQuad 408,Node 402)),(HqFace,(HalfQuad 408,Face 401)),(Twin,(HalfQuad 408,HalfQuad 409)),(HqNode,(HalfQuad 409,Node 401)),(HqFace,(HalfQuad 409,Face 400)),(Twin,(HalfQuad 409,HalfQuad 408))]

res51 = Store [(XY,(Node 500,PointTag (Pnt2d {_p2id = 21, _v2 = V2 0.0 0.0}))),(XY,(Node 501,PointTag (Pnt2d {_p2id = 22, _v2 = V2 2.0 0.0}))),(XY,(Node 502,PointTag (Pnt2d {_p2id = 23, _v2 = V2 4.0 2.0}))),(XY,(Node 503,PointTag (Pnt2d {_p2id = 24, _v2 = V2 3.0 5.0}))),(XY,(Node 504,PointTag (Pnt2d {_p2id = 25, _v2 = V2 0.0 3.0}))),(HqNode,(HalfQuad 500,Node 504)),(HqFace,(HalfQuad 500,Face 500)),(Twin,(HalfQuad 500,HalfQuad 501)),(HqNode,(HalfQuad 501,Node 500)),(HqFace,(HalfQuad 501,Face 501)),(Twin,(HalfQuad 501,HalfQuad 500)),(HqNode,(HalfQuad 502,Node 501)),(HqFace,(HalfQuad 502,Face 501)),(Twin,(HalfQuad 502,HalfQuad 503)),(HqNode,(HalfQuad 503,Node 500)),(HqFace,(HalfQuad 503,Face 500)),(Twin,(HalfQuad 503,HalfQuad 502)),(HqNode,(HalfQuad 504,Node 502)),(HqFace,(HalfQuad 504,Face 501)),(Twin,(HalfQuad 504,HalfQuad 505)),(HqNode,(HalfQuad 505,Node 501)),(HqFace,(HalfQuad 505,Face 502)),(Twin,(HalfQuad 505,HalfQuad 504)),(HqNode,(HalfQuad 506,Node 503)),(HqFace,(HalfQuad 506,Face 501)),(Twin,(HalfQuad 506,HalfQuad 507)),(HqNode,(HalfQuad 507,Node 502)),(HqFace,(HalfQuad 507,Face 503)),(Twin,(HalfQuad 507,HalfQuad 506)),(HqNode,(HalfQuad 508,Node 504)),(HqFace,(HalfQuad 508,Face 503)),(Twin,(HalfQuad 508,HalfQuad 509)),(HqNode,(HalfQuad 509,Node 502)),(HqFace,(HalfQuad 509,Face 502)),(Twin,(HalfQuad 509,HalfQuad 508)),(HqNode,(HalfQuad 510,Node 504)),(HqFace,(HalfQuad 510,Face 501)),(Twin,(HalfQuad 510,HalfQuad 511)),(HqNode,(HalfQuad 511,Node 503)),(HqFace,(HalfQuad 511,Face 503)),(Twin,(HalfQuad 511,HalfQuad 510)),(HqNode,(HalfQuad 512,Node 504)),(HqFace,(HalfQuad 512,Face 502)),(Twin,(HalfQuad 512,HalfQuad 513)),(HqNode,(HalfQuad 513,Node 501)),(HqFace,(HalfQuad 513,Face 500)),(Twin,(HalfQuad 513,HalfQuad 512))]

res42 = Store
  [(Quant 1, (HalfQuad 400, LengthTag (Length 1.0))),
   (Quant 1, (HalfQuad 401, LengthTag (Length 1.0))),
   (Quant 1, (HalfQuad 402, LengthTag (Length 1.0))),
   (Quant 1, (HalfQuad 403, LengthTag (Length 1.0))),
   (Quant 1, (HalfQuad 404, LengthTag (Length 1.0606601717798212))),
   (Quant 1, (HalfQuad 405, LengthTag (Length 1.0606601717798212))),
   (Quant 1, (HalfQuad 406, LengthTag (Length 0.7905694150420949))),
   (Quant 1, (HalfQuad 407, LengthTag (Length 0.7905694150420949))),
   (Quant 1, (HalfQuad 408, LengthTag (Length 0.7905694150420949))),
   (Quant 1, (HalfQuad 409, LengthTag (Length 0.7905694150420949))),
   (XY,
    (HalfQuad 400, PointTag (Pnt2d{_p2id = 400, _v2 = V2 0.0 1.0}))),
   (XY,
    (HalfQuad 401, PointTag (Pnt2d{_p2id = 401, _v2 = V2 0.0 1.0}))),
   (XY,
    (HalfQuad 402, PointTag (Pnt2d{_p2id = 402, _v2 = V2 1.0 0.0}))),
   (XY,
    (HalfQuad 403, PointTag (Pnt2d{_p2id = 403, _v2 = V2 1.0 0.0}))),
   (XY,
    (HalfQuad 404, PointTag (Pnt2d{_p2id = 404, _v2 = V2 0.75 0.75}))),
   (XY,
    (HalfQuad 405, PointTag (Pnt2d{_p2id = 405, _v2 = V2 0.75 0.75}))),
   (XY,
    (HalfQuad 406, PointTag (Pnt2d{_p2id = 406, _v2 = V2 1.75 0.75}))),
   (XY,
    (HalfQuad 407, PointTag (Pnt2d{_p2id = 407, _v2 = V2 1.75 0.75}))),
   (XY,
    (HalfQuad 408, PointTag (Pnt2d{_p2id = 408, _v2 = V2 0.75 1.75}))),
   (XY,
    (HalfQuad 409, PointTag (Pnt2d{_p2id = 409, _v2 = V2 0.75 1.75}))),
   (Quant 2, (Face 400, AreaTag (Area 1.5))),
   (Quant 2, (Face 401, AreaTag (Area 3.0))),
   (Quant 2, (Face 402, AreaTag (Area 1.5))),
   (XY, (Face 400, PointTag (Pnt2d{_p2id = 400, _v2 = V2 0.5 1.0}))),
   (XY, (Face 402, PointTag (Pnt2d{_p2id = 402, _v2 = V2 1.0 0.5}))),
   (Incenter,
    (Face 400,
     PointTag
       (Pnt2d{_p2id = 400,
              _v2 = V2 0.5260888168854738 1.2700907567377264}))),
   (Incenter,
    (Face 402,
     PointTag
       (Pnt2d{_p2id = 402,
              _v2 = V2 1.2700907567377262 0.5260888168854737}))),
   (XY, (Node 400, PointTag (Pnt2d{_p2id = 11, _v2 = V2 0.0 0.0}))),
   (XY, (Node 401, PointTag (Pnt2d{_p2id = 12, _v2 = V2 1.5 1.5}))),
   (XY, (Node 402, PointTag (Pnt2d{_p2id = 13, _v2 = V2 0.0 2.0}))),
   (XY, (Node 403, PointTag (Pnt2d{_p2id = 14, _v2 = V2 2.0 0.0}))),
   (HqNode, (HalfQuad 400, Node 402)),
   (HqFace, (HalfQuad 400, Face 400)),
   (Twin, (HalfQuad 400, HalfQuad 401)),
   (HqNode, (HalfQuad 401, Node 400)),
   (HqFace, (HalfQuad 401, Face 401)),
   (Twin, (HalfQuad 401, HalfQuad 400)),
   (HqNode, (HalfQuad 402, Node 403)),
   (HqFace, (HalfQuad 402, Face 401)),
   (Twin, (HalfQuad 402, HalfQuad 403)),
   (HqNode, (HalfQuad 403, Node 400)),
   (HqFace, (HalfQuad 403, Face 402)),
   (Twin, (HalfQuad 403, HalfQuad 402)),
   (HqNode, (HalfQuad 404, Node 401)),
   (HqFace, (HalfQuad 404, Face 402)),
   (Twin, (HalfQuad 404, HalfQuad 405)),
   (HqNode, (HalfQuad 405, Node 400)),
   (HqFace, (HalfQuad 405, Face 400)),
   (Twin, (HalfQuad 405, HalfQuad 404)),
   (HqNode, (HalfQuad 406, Node 403)),
   (HqFace, (HalfQuad 406, Face 402)),
   (Twin, (HalfQuad 406, HalfQuad 407)),
   (HqNode, (HalfQuad 407, Node 401)),
   (HqFace, (HalfQuad 407, Face 401)),
   (Twin, (HalfQuad 407, HalfQuad 406)),
   (HqNode, (HalfQuad 408, Node 402)),
   (HqFace, (HalfQuad 408, Face 401)),
   (Twin, (HalfQuad 408, HalfQuad 409)),
   (HqNode, (HalfQuad 409, Node 401)),
   (HqFace, (HalfQuad 409, Face 400)),
   (Twin, (HalfQuad 409, HalfQuad 408))]

res52 = Store
  [(Quant 1, (HalfQuad 500, LengthTag (Length 1.5))),
   (Quant 1, (HalfQuad 501, LengthTag (Length 1.5))),
   (Quant 1, (HalfQuad 502, LengthTag (Length 1.0))),
   (Quant 1, (HalfQuad 503, LengthTag (Length 1.0))),
   (Quant 1, (HalfQuad 504, LengthTag (Length 1.4142135623730951))),
   (Quant 1, (HalfQuad 505, LengthTag (Length 1.4142135623730951))),
   (Quant 1, (HalfQuad 506, LengthTag (Length 1.5811388300841898))),
   (Quant 1, (HalfQuad 507, LengthTag (Length 1.5811388300841898))),
   (Quant 1, (HalfQuad 508, LengthTag (Length 2.0615528128088303))),
   (Quant 1, (HalfQuad 509, LengthTag (Length 2.0615528128088303))),
   (Quant 1, (HalfQuad 510, LengthTag (Length 1.8027756377319946))),
   (Quant 1, (HalfQuad 511, LengthTag (Length 1.8027756377319946))),
   (Quant 1, (HalfQuad 512, LengthTag (Length 1.8027756377319946))),
   (Quant 1, (HalfQuad 513, LengthTag (Length 1.8027756377319946))),
   (XY,
    (HalfQuad 500, PointTag (Pnt2d{_p2id = 500, _v2 = V2 0.0 1.5}))),
   (XY,
    (HalfQuad 501, PointTag (Pnt2d{_p2id = 501, _v2 = V2 0.0 1.5}))),
   (XY,
    (HalfQuad 502, PointTag (Pnt2d{_p2id = 502, _v2 = V2 1.0 0.0}))),
   (XY,
    (HalfQuad 503, PointTag (Pnt2d{_p2id = 503, _v2 = V2 1.0 0.0}))),
   (XY,
    (HalfQuad 504, PointTag (Pnt2d{_p2id = 504, _v2 = V2 3.0 1.0}))),
   (XY,
    (HalfQuad 505, PointTag (Pnt2d{_p2id = 505, _v2 = V2 3.0 1.0}))),
   (XY,
    (HalfQuad 506, PointTag (Pnt2d{_p2id = 506, _v2 = V2 3.5 3.5}))),
   (XY,
    (HalfQuad 507, PointTag (Pnt2d{_p2id = 507, _v2 = V2 3.5 3.5}))),
   (XY,
    (HalfQuad 508, PointTag (Pnt2d{_p2id = 508, _v2 = V2 2.0 2.5}))),
   (XY,
    (HalfQuad 509, PointTag (Pnt2d{_p2id = 509, _v2 = V2 2.0 2.5}))),
   (XY,
    (HalfQuad 510, PointTag (Pnt2d{_p2id = 510, _v2 = V2 1.5 4.0}))),
   (XY,
    (HalfQuad 511, PointTag (Pnt2d{_p2id = 511, _v2 = V2 1.5 4.0}))),
   (XY,
    (HalfQuad 512, PointTag (Pnt2d{_p2id = 512, _v2 = V2 1.0 1.5}))),
   (XY,
    (HalfQuad 513, PointTag (Pnt2d{_p2id = 513, _v2 = V2 1.0 1.5}))),
   (Quant 2, (Face 500, AreaTag (Area 3.0))),
   (Quant 2, (Face 501, AreaTag (Area 13.5))),
   (Quant 2, (Face 502, AreaTag (Area 5.0))),
   (Quant 2, (Face 503, AreaTag (Area 5.5))),
   (XY, (Face 500, PointTag (Pnt2d{_p2id = 500, _v2 = V2 1.0 1.5}))),
   (XY, (Face 502, PointTag (Pnt2d{_p2id = 502, _v2 = V2 1.9 2.1}))),
   (XY,
    (Face 503,
     PointTag
       (Pnt2d{_p2id = 503,
              _v2 = V2 2.1363636363636362 3.0454545454545454}))),
   (Incenter,
    (Face 500,
     PointTag
       (Pnt2d{_p2id = 500,
              _v2 = V2 0.6972243622680053 0.6972243622680053}))),
   (Incenter,
    (Face 502,
     PointTag
       (Pnt2d{_p2id = 502,
              _v2 = V2 2.147223257637539 1.4868105517362036}))),
   (Incenter,
    (Face 503,
     PointTag
       (Pnt2d{_p2id = 503,
              _v2 = V2 2.459983744097888 3.426103007934949}))),
   (XY, (Node 500, PointTag (Pnt2d{_p2id = 21, _v2 = V2 0.0 0.0}))),
   (XY, (Node 501, PointTag (Pnt2d{_p2id = 22, _v2 = V2 2.0 0.0}))),
   (XY, (Node 502, PointTag (Pnt2d{_p2id = 23, _v2 = V2 4.0 2.0}))),
   (XY, (Node 503, PointTag (Pnt2d{_p2id = 24, _v2 = V2 3.0 5.0}))),
   (XY, (Node 504, PointTag (Pnt2d{_p2id = 25, _v2 = V2 0.0 3.0}))),
   (HqNode, (HalfQuad 500, Node 504)),
   (HqFace, (HalfQuad 500, Face 500)),
   (Twin, (HalfQuad 500, HalfQuad 501)),
   (HqNode, (HalfQuad 501, Node 500)),
   (HqFace, (HalfQuad 501, Face 501)),
   (Twin, (HalfQuad 501, HalfQuad 500)),
   (HqNode, (HalfQuad 502, Node 501)),
   (HqFace, (HalfQuad 502, Face 501)),
   (Twin, (HalfQuad 502, HalfQuad 503)),
   (HqNode, (HalfQuad 503, Node 500)),
   (HqFace, (HalfQuad 503, Face 500)),
   (Twin, (HalfQuad 503, HalfQuad 502)),
   (HqNode, (HalfQuad 504, Node 502)),
   (HqFace, (HalfQuad 504, Face 501)),
   (Twin, (HalfQuad 504, HalfQuad 505)),
   (HqNode, (HalfQuad 505, Node 501)),
   (HqFace, (HalfQuad 505, Face 502)),
   (Twin, (HalfQuad 505, HalfQuad 504)),
   (HqNode, (HalfQuad 506, Node 503)),
   (HqFace, (HalfQuad 506, Face 501)),
   (Twin, (HalfQuad 506, HalfQuad 507)),
   (HqNode, (HalfQuad 507, Node 502)),
   (HqFace, (HalfQuad 507, Face 503)),
   (Twin, (HalfQuad 507, HalfQuad 506)),
   (HqNode, (HalfQuad 508, Node 504)),
   (HqFace, (HalfQuad 508, Face 503)),
   (Twin, (HalfQuad 508, HalfQuad 509)),
   (HqNode, (HalfQuad 509, Node 502)),
   (HqFace, (HalfQuad 509, Face 502)),
   (Twin, (HalfQuad 509, HalfQuad 508)),
   (HqNode, (HalfQuad 510, Node 504)),
   (HqFace, (HalfQuad 510, Face 501)),
   (Twin, (HalfQuad 510, HalfQuad 511)),
   (HqNode, (HalfQuad 511, Node 503)),
   (HqFace, (HalfQuad 511, Face 503)),
   (Twin, (HalfQuad 511, HalfQuad 510)),
   (HqNode, (HalfQuad 512, Node 504)),
   (HqFace, (HalfQuad 512, Face 502)),
   (Twin, (HalfQuad 512, HalfQuad 513)),
   (HqNode, (HalfQuad 513, Node 501)),
   (HqFace, (HalfQuad 513, Face 500)),
   (Twin, (HalfQuad 513, HalfQuad 512))]


-- (coords2faces_4) = evalState coords2faces tess41short
-- (coords2faces_5) = evalState coords2faces tess51short

-- point1s :: [(ObjTessShort, (ObjTessShort, ObjTessShort))]
-- (point1s) = evalState points12 tess41short

-- lengthHQ = evalState distanceOfHQ tess41short

-- -- lengthHQ2Ins4 :: [StoreTessShortElement]
-- lengthHQ2Ins4 = evalState lengthHQasTriple tess41short
-- lengthHQ2Ins5 = evalState lengthHQasTriple tess51short

-- -- midpointHQ4 = evalState midpointHQ tess41short 
-- midpointHQasTriple4 = evalState midpointHQasTriple tess41short 

-- | evaluate a transformation to a queryresult against a catStore 
-- questionalbe shortcut - may be difficult to debug?? 
-- evalTrans4query2cat trans query cat = evalState ((fmap (map trans )) query) cat 

-- build the completion 
-- with length, midpoint 
-- with area incircle circumcircle 

-- -- trans which go for edges 
-- forEdges2points = [lengthHQtriple, midpointHQtriple]  
-- forFaces = [circumcenter2triple, incenter2triple]
-- forFaces2 = [area2triples]
-- forqueries = [points12]
-- theCats = [tess41short, tess51short]
-- additinsPoints :: CatStoreTessShort -> [(ObjTessShort, MorphTessShort, ObjTessShort)]
-- additinsPoints cat =  concat [evalTrans4query2cat trans points12 cat | trans <-[lengthHQtriple, midpointHQtriple]  ] -- trans query cat
-- additinsAreas cat  =  concat [evalTrans4query2cat trans coords2faces cat | trans <-[area2triples] ] -- trans query cat
-- additinsCenters cat =  catMaybes . concat   $ [evalTrans4query2cat trans coords2faces cat | trans <-[circumcenter2triple, incenter2triple]  ] -- trans query cat
-- allAddins cat = concat [additinsPoints cat, additinsAreas cat, additinsCenters cat]

-- should work again

-- cat42 = storeBatch (map Ins (allAddins tess41short)) tess41short
-- cat52 = storeBatch (map Ins (allAddins tess51short)) tess51short

-- -- addinsFirst = evalTrans4query2cat midpointHQtriple points12 tess41short

-- hq4 :: [(ObjTessShort, [V2D])]
-- hq4 = evalTrans4query2cat points2v2 hqTriangles cat42 
-- hq5 = evalTrans4query2cat points2v2 hqTriangles cat52 
-- hqV4 :: [(ObjTessShort, (V2D, V2D))]
-- hqV4 = evalTrans4query2cat pointsPairsv2 hqVoro cat42 
-- hqD4 = evalTrans4query2cat pointsPairsv2 hqDela cat42 
-- hqV5 = evalTrans4query2cat pointsPairsv2 hqVoro cat52 
-- hqD5 = evalTrans4query2cat pointsPairsv2 hqDela cat52 

-- fig4 :: [Figure V2D]
-- fig4 = map closeLine . map (\p -> Figure (dark green) p) . map snd $ hq4
-- fig5 = map closeLine . map (\p -> Figure (dark red) p) . map snd $ hq5

-- fig4voro :: [Figure V2D]
-- fig4voro = map (\p -> figLine (dark green) p) . map snd $ hqV4
-- fig4dela = map (\p -> figLine (dark red) p) . map snd $ hqD4

-- fig5voro = map (\p -> figLine (dark green) p) . map snd $ hqV5
-- fig5dela = map (\p -> figLine (dark red) p) . map snd $ hqD5

-- pageHQfaces_testGraphicsx ::  ExceptT Text IO ()
-- pageHQfaces_testGraphicsx    = do
--     putIOwords ["pageHQfaces_testGraphics - the hq4  ", shownice hq4]
--     putIOwords ["pageHQfaces_testGraphics - the hq5  ", shownice hq5]
--     putIOwords ["the voronoi lines  \n ", showT $ hqV4]
--     putIOwords ["the delaunay lines  \n ", showT $ hqD4]

--     -- putIOwords ["pageHQfaces_testGraphics - the figure  ", shownice fig]

--     -- showFacePage2 (fig4voro ++ fig4dela)
--     showFacePage2 (fig5voro ++ fig5dela)

-- pageHQfaces_test3 :: ErrIO ()
-- pageHQfaces_test3 = do
--     putIOwords ["the triples in cat45  \n ", shownice cat42]

--     putIOwords ["the hq triangles  \n ", showAsLines hq4]
--     putIOwords ["the voronoi lines  \n ", showT $ hqV4]

    
--     -- putIOwords ["the hq triangles 1  \n ", showAsLines $ evalTrans4query2cat id hqTriangles1 cat45 ]
--     -- putIOwords ["the hq triangles 2  \n ", showAsLines $ evalTrans4query2cat id hqTriangles2 cat45 ]
--     -- putIOwords ["the hq triangles 3 \n ", showAsLines $ evalTrans4query2cat id hqTriangles3 cat45 ]
--     -- putIOwords ["the midpoint of the hq \n ", showAsLines  $ evalState midpointHQ tess41short]
--     -- putIOwords  ["first additions \n", showAsLines $ addinsFirst]


-- pageHQfaces_test2 :: ErrIO ()
-- pageHQfaces_test2 = do

--     putIOwords ["the midpointHQtriple \n ", showAsLines $  evalTrans4query2cat midpointHQtriple points12 cat42]
--     -- putIOwords ["the midpoint of the hq \n ", showAsLines  $ evalState midpointHQ tess41short]
--     -- putIOwords  ["first additions \n", showAsLines $ addinsFirst]
--     -- putIOwords  ["all the additinsPoints \n", showAsLines  $ additinsPoints]
--     -- putIOwords  ["all the additinsarea \n", showAsLines  $ additinsAreas]
--     -- putIOwords  ["all the additinscenters \n", showAsLines  $ additinsCenters]
--     -- putIOwords  ["all  \n", showAsLines  $ allAddins]
--     putIOwords ["the new triplestore cat42", shownice cat42]

--     return ()

-- pageHQfaces_test :: ErrIO ()
-- pageHQfaces_test = do
--     putIOwords ["the tests for relations after storing four and five points"]
--     -- putIOwords ["tess41short\n", showlong tess41short, "\n"    ]
--     putIOwords ["the end coord of the hqs", showAsLines point1s]
 
--     putIOwords ["the distances2 distanceOfHQ ", showAsLines lengthHQ]
--     putIOwords ["the distances triples to insert \n ", showAsLines   lengthHQ2Ins4]
--     putIOwords ["the distances triples to insert for five \n ", showAsLines   lengthHQ2Ins5]
--     putIOwords ["the midpoint of the hq \n ", showAsLines  $ evalState midpointHQ tess41short]
--     putIOwords ["the midpoint as triple \n ", showAsLines  $ evalState midpointHQasTriple tess41short]
--     putIOwords ["the area \n ", showAsLines  $ evalState area2facesM tess41short]
--     putIOwords ["the circum2facesM \n ", showAsLines  $ evalState circum2facesM tess41short]
--     putIOwords ["the incenter2facesM \n ", showAsLines  $ evalState incenter2facesM tess41short]

--     putIOwords ["the incenter2facesTriples \n ", showAsLines . catMaybes $ evalState incenter2facesTriples tess41short]
--     putIOwords ["the incircumCenter2facesTriples \n ", showAsLines . catMaybes $ evalState incircumCenter2facesTriples tess41short]

--     putIOwords ["the incircumCenter2facesTriples \n ", showAsLines . catMaybes $  evalTrans4query2cat circumcenter2triple coords2faces tess41short
--         ]
  
--     return () 