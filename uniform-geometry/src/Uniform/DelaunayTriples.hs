-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Delaunay

-- collect the data to store in triple storage
-- the data is not yet typed, in order no to introduce the triple store types here
-- but every relation is a single list of pairs.
-- the ids are types (N for NodeID, )
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
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.DelaunayTriples
    -- ( module Uniform.Delaunay
    -- , module Uniform.DelaunayTiles
    -- , module Uniform.Point2dData
    -- , module Uniform.Point2d
    -- , module Linear.V2
    -- , module Control.Lens
    --     ) 
         where

import UniformBase
import Uniform.Point2d
import Uniform.Point2dData
import Uniform.GeometryFunctions
import qualified Data.Map as Map 
import Uniform.Delaunay
import Uniform.DelaunayTiles
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics
import Delaunay
  

import Delaunay.Types
import Qhull.Types
-- import qualified Data.Map as Ix
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict  as IM

import           Data.HashMap.Strict.InsOrd as H hiding (map)
import Language.Haskell.TH.Lens (_Overlapping)


newtype HqID = Hq Integer 
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Enum)

-- offset 
-- must be the same for N, F, HQ 

--- nodes 

trip_node_name :: Integer -> Integer -> [Text]-> [(NodeID, Text)]
-- build the name relation for nodes 
-- should check that the length of ts is as expected
-- call with offset decided for this input and the number of points
-- respective the number of faces 


trip_node_name offs ct ts = zip (map N [offs .. offs+ct]) ts 

trip_x :: (Integer -> a) -> Integer ->  [[Double]]-> [(a, Coord)]
-- |input is vertices (for nodes) or center (for faces)
trip_x idType offs xs = zip (map idType [offs .. ]) (map (!!0) xs) 

trip_y :: (Integer -> a) -> Integer  -> [[Double]]-> [(a, Coord)]
-- inpur is vertices (for nodes) or center (for faces)

trip_y idType offs  xs = zip (map idType [offs .. ]) (map (!!1) xs) 

--- faces 
trip_surface :: Integer  -> [Double] -> [(FaceID, Coord)]
-- inpur is surface
trip_surface offs xs = zip (map F [offs .. ]) ( xs) 
trip_surface2 offs = trip_surface offs . surface 

-- could add a name

-- half quads 

trip_hq_node :: Integer -> [Integer] -> [(HqID, NodeID)]
-- input is start3
trip_hq_node offs ss = zip (map Hq [offs, offs+2 ..]) (map N ss)

trip_hq_nodeX offs = trip_hq_node offs . start3

trip_hq_length:: Integer -> [Double] -> [(HqID, Double)]
-- input is length3 (the length of the HALF)
trip_hq_length offs ss = zip (map Hq [offs, offs+2 ..]) (map (/2) ss)
trip_hq_lengthX offs  = trip_hq_length offs . length1


trip_hq_node2 :: Integer -> [Integer] -> [(HqID, NodeID)]
-- input is end3
trip_hq_node2 offs ss = zip (map Hq [offs+1, offs+3 ..]) (map N ss)
trip_hq_node2X offs = trip_hq_node2 offs . end3

trip_hq_length2:: Integer -> [Double] -> [(HqID, Double)]
-- input is length3 (the length of the HALF)
trip_hq_length2 offs ss = zip (map Hq [offs+1, offs+3 ..]) (map (/2) ss)
trip_hq_length2X offs = trip_hq_length2 offs . length1 

-- hq to face - function to convert Tesselation to list of triples for the HalfQuads
-- test wether the center is left or right of edge
-- i.e. test area start - end - center >0
-- trip_hqs_faces :: Integer -> Tesselation -> [[_]]
trip_hqs_faces offs tess = zipWith (trip_hq_faces tiles1 offs) [offs, offs+2 ..] tilefacets1
    where 
            tiles1 :: [Tile]
            tiles1 = IM.elems . _tiles $ tess 
            tilefacets1  :: [TileFacet]
            tilefacets1 = IM.elems . _tilefacets $ tess


-- | process one tileface 
-- trip_hq_faces :: [Tile] -> Integer -> TileFacet -> _
-- process one tilefacet and decide for each face where to put
trip_hq_faces tiles tilesoffs thisoffs  tft = ([starthq, endhq], [twinhqS,twinhqT], facehqs)
    where 
        vertices1 ::  [(IM.Key, [Double])]
        vertices1 =   IM.assocs .  _vertices' .  _subsimplex $ tft
        (startid, startxy) =  (!!0)  vertices1 
        (endid, endxy) =  vertices1 !! 1
        starthq = (Hq thisoffs, N . toInteger $ startid)
        endhq = (Hq (thisoffs +1), N . toInteger $ endid)
        twinhqS =  (Hq thisoffs, Hq (thisoffs +1))
        twinhqT =  (Hq (thisoffs+1), Hq thisoffs)

        facetofs1 :: [Int] -- the list of faces
        facetofs1 =    IS.elems . _facetOf  $ tft

        centerxy i = _circumcenter . _simplex . (!! i)  $ tiles 
        centers = map centerxy facetofs1
        ccws = map  (ccw_test startxy endxy) centers
            -- test the center to determine which side 
            -- add to the correct hq 
        facehqs = zipWith facehq ccws (map toInteger facetofs1)

        facehq :: Bool -> Integer -> (HqID, FaceID)
        facehq bool1 fid = 
            if bool1 then (Hq thisoffs, F (tilesoffs + fid))
                        else (Hq (thisoffs +1), F (tilesoffs + fid))

-- trip_hq_face :: Integer -> [[Integer]] -> [(HqID, FaceID)]
-- -- input is facetof3 --  start -- face right of start-end 
-- trip_hq_face offs ss = zip (map Hq [offs, offs+2 ..]) (map F . map (!!0) $ ss)

-- trip_hq_face2 :: Integer -> [[Integer]] -> [(HqID, FaceID)]
-- -- input is facetof3 -- end -- face left of start-end
-- trip_hq_face2 offs ss = zip (map Hq [offs+1, offs+3 ..]) (map F . map (!!0) $ ss)

-- a start to find it with searching in the edge list of a face.
-- abandoned because too complicated. 


delaunay2 v2s = delaunay (map v2toList2 v2s) False False Nothing 
-- ^ calling delaunay with a list of V2


fourV2 = map p2toV2 fourP2 

mainDelaunayTriples :: ErrIO ()
mainDelaunayTriples = do 
    putIOwords ["\nmainDelaunayTriples\n"]
    -- putIOwords ["\nthe hq for faces\n", showT ]
    res4 <- liftIO $ delaunay2 fourV2 
    -- res4 <- liftIO $ delaunay (map (v2toList2 . p2toV2) $ fourP2) False False Nothing
    let hqf = trip_surface2 400 res4
    putIOwords ["\nfaces res4\n", showT hqf]
    -- let hqnn = trip_node_name offs ct ts = zip (map N [offs .. offs+ct]) ts
    -- not used, no names, would require removing ct (count)
    -- let hqn = trip_hq_nodeX 400 res4
    -- let hqn2 = trip_hq_node2X 400 res4
    let hqnx = trip_x N 400 (vertices res4)
    let hqny = trip_y N 400 (vertices res4)
    putIOwords ["\nnode hqs for res4\n", showT hqnx, showT hqny]
    let hqfx = trip_x F 400 (center res4)
    let hqfy = trip_y F 400 (center res4)
    putIOwords ["\ncenter hqs for res4\n", showT hqfx, showT hqfy]

    let hqlength = trip_hq_lengthX 400 res4
    let hqlength2 = trip_hq_length2X 400 res4
    putIOwords ["\nhqs length for res4\n", showT hqlength, showT hqlength2]

    let hqs = trip_hqs_faces 400 res4
    putIOwords ["\nall the face hqs for res4\n", showT hqs]


-- triplePerLine2 :: (Show a, Show b) => [[(a,b)]] -> Text
-- triplePerLine2 = unlines' . map (unlines' .map   ( unlines'.   map showT)) 
-- not working on hqs, not a list before changed to sum type
