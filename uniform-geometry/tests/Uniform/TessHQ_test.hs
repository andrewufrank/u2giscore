-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pretty_test
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
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

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.TessHQ_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Control.Lens
import Control.Lens.Tuple
import Test.Framework
import ExampleData.Point2d
import UniformBase
import Uniform.TesselationHalfQuads
-- import Control.Exception
import Uniform.GeometryFunctions
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.DelaunayTriangulation.Naive
import Data.Ext
import Data.PlaneGraph
import qualified Data.PlaneGraph as Plane
import qualified Data.Vector as Vec
-- import qualified Data.Geometry.Point as HP 

controlList :: Triangulation v r -> [(Int, Dart Int, VertexId' Int, VertexId' Int, FaceId' Int, FaceId' Int)]
controlList tess = map oneDart darts 
    where 
        pg = toPlaneGraph tess 
        darts = Vec.toList . darts' $  pg
        oneDart d = ( fromEnum d
            , d
            , tailOf d pg
            , headOf d pg
            , leftFace d pg 
            , rightFace d pg
             )

mainHQ :: ErrIO ()
mainHQ = do
    putIOwords ["the conversion to a tesselation As Half-Quads"]
    putIOwords ["the given points", showlong fourPnt2d]
    let tess41 = delaunay2 fourPnt2d
    putIOwords ["the returned tesselation", showT tess41]
    putIOwords ["point2d two\n", showlong (toHq1 tess41), "\n"]
    putIOwords ["controlList \nid, dart,          tail,      head,      left,    right \n",   showAsLines . controlList $ tess41, "\n"]

mainHQ2 :: ErrIO ()
mainHQ2 = do
    putIOwords ["the conversion to a tesselation As Half-Quads"]
    putIOwords ["the given points", showlong fivePnt2d]
    let tess51 = delaunay2 fivePnt2d
    putIOwords ["the returned tesselation", showT tess51]
    putIOwords ["point2d two\n", showlong (toHq1 tess51), "\n"]
    putIOwords ["controlList \nid, dart,          tail,      head,      left,    right \n",   showAsLines . controlList $ tess51, "\n"]

-- mainMakeTess v2s= do 
--     putIOwords ["\nmainDelaunayTriples\n"]
--     -- putIOwords ["\nthe hq for faces\n", showT ]
--     tess <-   delaunay2 v2s 
--     putIOwords ["\nthe tess\n", showT tess]

--     let trips =  zero --  toHq1 $ tess   --hqToTrip 400 .
--     putIOwords ["\nthe hq for faces\n", showT trips]
--     return  trips 

-- test_toHqFour = (do 
--                 res <- mainMakeTess fourV2
--                 assertEqual tripsResfour res )
-- -- test_toHqFive = (do 
-- --                 res <- mainMakeTess fiveV2
-- --                 assertEqual tripsResfive res )

-- -- test_toHqFive31 = (do 
-- --                 res <- mainMakeTess fiveV2_31
-- --                 assertEqual tripsResfive_31 res )

-- to test that delaunay works as before 
test_delaunay4 = assertEqual triangulationFour (showT . delaunay2 $ fourPnt2d)
test_delaunay5 = assertEqual triangulationFive (showT . delaunay2 $ fivePnt2d)

-- test thta planarSubdiv and planeGraph works as before 
test_planar1 = assertEqual planarSubdi2Four (showT . planarSubdiv2 . delaunay2 $ fourPnt2d)
test_planeGraph1 = assertEqual toPlaneGraphFour (showT . toPlaneGraph   . delaunay2 $ fourPnt2d)

planeGraphFour = toPlaneGraph . delaunay2 $ fourPnt2d 
planarSubdivFour = planarSubdiv2   . delaunay2 $ fourPnt2d 
planarSubdivFive = planarSubdiv2   . delaunay2 $ fourPnt2d 

verticesFour = vertices   planeGraphFour --planarSubdivFour
vFourList = Vec.toList verticesFour

{-
circumCenter :: Point -> Point -> Point -> Point
> circumCenter (ax, ay) (bx, by) (cx, cy)
>     =  (((ay**2+ax**2)*(by-cy)+(by**2+bx**2)*(cy-ay)+(cy**2+cx**2)*(ay-by))/d,
>        ((ay**2+ax**2)*(cx-bx)+(by**2+bx**2)*(ax-cx)+(cy**2+cx**2)*(bx-ax))/d)
>        where d = 2*(ax*(by-cy)+bx*(cy-ay)+cx*(ay-by)
-}
v4at1 = snd $ vFourList !! 0

loc41 =  v4at1 ^. location
loc41b = ( vFourList !! 0) ^. _2 . location
loc41c = map (\e -> (snd e) ^. location) vFourList 
-- [Point2 0.0 0.0,Point2 1.5 1.5,Point2 0.0 2.0,Point2 2.0 0.0]
vFacesFourList = Vec.toList . faces $ planeGraphFour
test_vFacesFourList = assertEqual 
    "[(FaceId 0,()),(FaceId 1,()),(FaceId 2,())]"
    (showT vFacesFourList)

vDartsFourList = Vec.toList . darts $ planeGraphFour 
vDartsFourList' = Vec.toList . darts' $ planeGraphFour 
test_DartsfFourList = assertEqual res (showT vDartsFourList)
    where 
        res = "[(Dart (Arc 0) +1,()),(Dart (Arc 1) +1,()),(Dart (Arc 2) +1,()),(Dart (Arc 3) +1,()),(Dart (Arc 4) +1,()),(Dart (Arc 2) -1,()),(Dart (Arc 4) -1,()),(Dart (Arc 0) -1,()),(Dart (Arc 1) -1,()),(Dart (Arc 3) -1,())]"

heads4 = map (\d -> headOf d planeGraphFour) vDartsFourList'
-- [VertexId 2,VertexId 3,VertexId 1,VertexId 3,VertexId 2,VertexId 0,VertexId 1,VertexId 0,VertexId 0,VertexId 1]
tails4 = map (\d -> tailOf d planeGraphFour) vDartsFourList'
-- [VertexId 0,VertexId 0,VertexId 0,VertexId 1,VertexId 1,VertexId 1,VertexId 2,VertexId 2,VertexId 3,VertexId 3]
left4 = map (\d -> leftFace d planeGraphFour) vDartsFourList'
--[FaceId 1,FaceId 2,FaceId 0,FaceId 1,FaceId 0,FaceId 2,FaceId 1,FaceId 0,FaceId 1,FaceId 2]
right4 = map (\d -> rightFace d planeGraphFour) vDartsFourList'
-- [FaceId 0,FaceId 1,FaceId 2,FaceId 2,FaceId 1,FaceId 0,FaceId 0,FaceId 1,FaceId 2,FaceId 1]
-- twin is witzlos - von + auf - 
-- twin4 = map (\d -> Plane.twin d planeGraphFour) vDartsFourList'



test_verticesDataFourList = 
    assertEqual vertexFourList (showT . Vec.toList $ verticesFour)

test_verticesDataFour = assertEqual vertexDataFour (showT verticesFour)
vertexFourList ="[(VertexId 0,VertexData {_location = Point2 0.0 0.0, _vData = 11}),(VertexId 1,VertexData {_location = Point2 1.5 1.5, _vData = 12}),(VertexId 2,VertexData {_location = Point2 0.0 2.0, _vData = 13}),(VertexId 3,VertexData {_location = Point2 2.0 0.0, _vData = 14})]"
vertexDataFour = "[(VertexId 0,VertexData {_location = Point2 0.0 0.0, _vData = 11}),(VertexId 1,VertexData {_location = Point2 1.5 1.5, _vData = 12}),(VertexId 2,VertexData {_location = Point2 0.0 2.0, _vData = 13}),(VertexId 3,VertexData {_location = Point2 2.0 0.0, _vData = 14})]"

triangulationFour = "Triangulation {_vertexIds = fromList [(Point2 0.0 0.0,0),(Point2 0.0 2.0,2),(Point2 1.5 1.5,1),(Point2 2.0 0.0,3)], _positions = [Point2 0.0 0.0 :+ 11,Point2 1.5 1.5 :+ 12,Point2 0.0 2.0 :+ 13,Point2 2.0 0.0 :+ 14], _neighbours = [fromList [2,1,3],fromList [3,0,2],fromList [1,0],fromList [0,1]]}"

triangulationFive = "Triangulation {_vertexIds = fromList [(Point2 0.0 0.0,0),(Point2 0.0 3.0,4),(Point2 2.0 0.0,1),(Point2 3.0 5.0,3),(Point2 4.0 2.0,2)], _positions = [Point2 0.0 0.0 :+ 21,Point2 2.0 0.0 :+ 22,Point2 4.0 2.0 :+ 23,Point2 3.0 5.0 :+ 24,Point2 0.0 3.0 :+ 25], _neighbours = [fromList [4,1],fromList [0,4,2],fromList [1,4,3],fromList [2,4],fromList [2,1,0,3]]}"

planarSubdi2Four = "PlanarSubdivision {_components = [PlaneGraph {_graph = PlanarGraph embedding = Permutation {_orbits = [[Dart (Arc 0) +1,Dart (Arc 1) +1,Dart (Arc 2) +1],[Dart (Arc 3) +1,Dart (Arc 4) +1,Dart (Arc 2) -1],[Dart (Arc 4) -1,Dart (Arc 0) -1],[Dart (Arc 1) -1,Dart (Arc 3) -1]], _indexes = [(0,0),(2,1),(0,1),(3,0),(0,2),(1,2),(1,0),(3,1),(1,1),(2,0)]} , vertexData = [VertexData {_location = Point2 0.0 0.0, _vData = VertexId 0},VertexData {_location = Point2 1.5 1.5, _vData = VertexId 1},VertexData {_location = Point2 0.0 2.0, _vData = VertexId 2},VertexData {_location = Point2 2.0 0.0, _vData = VertexId 3}] , rawDartData = [Dart (Arc 0) +1,Dart (Arc 0) -1,Dart (Arc 1) +1,Dart (Arc 1) -1,Dart (Arc 2) +1,Dart (Arc 2) -1,Dart (Arc 3) +1,Dart (Arc 3) -1,Dart (Arc 4) +1,Dart (Arc 4) -1] , faceData = [FaceId 1,FaceId 0,FaceId 2]}], _rawVertexData = [Raw {_compId = ComponentId {unCI = 0}, _idxVal = VertexId 0, _dataVal = 11},Raw {_compId = ComponentId {unCI = 0}, _idxVal = VertexId 1, _dataVal = 12},Raw {_compId = ComponentId {unCI = 0}, _idxVal = VertexId 2, _dataVal = 13},Raw {_compId = ComponentId {unCI = 0}, _idxVal = VertexId 3, _dataVal = 14}], _rawDartData = [Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 0) +1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 0) -1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 1) +1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 1) -1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 2) +1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 2) -1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 3) +1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 3) -1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 4) +1, _dataVal = ()},Raw {_compId = ComponentId {unCI = 0}, _idxVal = Dart (Arc 4) -1, _dataVal = ()}], _rawFaceData = [RawFace {_faceIdx = Nothing, _faceDataVal = FaceData {_holes = fromList [Dart (Arc 0) +1], _fData = ()}},RawFace {_faceIdx = Just (ComponentId {unCI = 0},FaceId 0), _faceDataVal = FaceData {_holes = fromList [], _fData = ()}},RawFace {_faceIdx = Just (ComponentId {unCI = 0},FaceId 2), _faceDataVal = FaceData {_holes = fromList [], _fData = ()}}]}"


toPlaneGraphFour = "PlaneGraph {_graph = PlanarGraph embedding = Permutation {_orbits = [[Dart (Arc 0) +1,Dart (Arc 1) +1,Dart (Arc 2) +1],[Dart (Arc 3) +1,Dart (Arc 4) +1,Dart (Arc 2) -1],[Dart (Arc 4) -1,Dart (Arc 0) -1],[Dart (Arc 1) -1,Dart (Arc 3) -1]], _indexes = [(0,0),(2,1),(0,1),(3,0),(0,2),(1,2),(1,0),(3,1),(1,1),(2,0)]} , vertexData = [VertexData {_location = Point2 0.0 0.0, _vData = 11},VertexData {_location = Point2 1.5 1.5, _vData = 12},VertexData {_location = Point2 0.0 2.0, _vData = 13},VertexData {_location = Point2 2.0 0.0, _vData = 14}] , rawDartData = [(),(),(),(),(),(),(),(),(),()] , faceData = [(),(),()]}"


