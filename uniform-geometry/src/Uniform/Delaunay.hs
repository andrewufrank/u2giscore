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


module Uniform.Delaunay 
    ( module Uniform.Delaunay
    , module Uniform.PointData
    , module Uniform.Point
    , module Linear.V2
    , module Control.Lens
        )  where

import UniformBase
import Uniform.Point
import Uniform.PointData
import qualified Data.Map as Map 

-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics
import Delaunay.Types
import Qhull.Types
import Data.Map hiding (map)

newtype NodeID = N Integer 
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Enum)
type NodeName = Text 
type Coord = Double

p2_node_name :: NodeID -> P2 -> (NodeID, NodeName)
p2_node_name i p = (i, showT $ p ^. p2id)
type Node_Name = (NodeID, NodeName)

p2s_node_name :: Integer -> [P2] -> [Node_Name]
p2s_node_name off1 p2s = zipWith p2_node_name (map N [off1 ..]) p2s

node_name_five :: [Node_Name]
node_name_five = p2s_node_name 50 fiveP2
node_name_four = p2s_node_name 40 fourP2

p2_node_x :: NodeID -> P2 -> (NodeID, Coord)
p2_node_x i p = (i,    p ^. v2._x)
p2s_node_x :: Integer -> [P2] -> [(NodeID, Coord)]
p2s_node_x off1 p2s = zipWith p2_node_x (map N [off1 ..]) p2s
node_x_four :: [(NodeID, Coord)]
node_x_four = p2s_node_x 40 fourP2

{-
tess = Tesselation {_sites = fromList [(0,Site {_point = [0.0,0.0], _neighsitesIds = fromList [1,2,3], _neighfacetsIds = fromList [1,2,4], _neightilesIds = fromList [0,1]}),(1,Site {_point = [1.5,1.5], _neighsitesIds = fromList [0,2,3], _neighfacetsIds = fromList [0,2,3], _neightilesIds = fromList [0,1]}),(2,Site {_point = [0.0,2.0], _neighsitesIds = fromList [0,1], _neighfacetsIds = fromList [3,4], _neightilesIds = fromList [1]}),(3,Site {_point = [2.0,0.0], _neighsitesIds = fromList [0,1], _neighfacetsIds = fromList [0,1], _neightilesIds = fromList [0]})], _tiles = fromList [(0,Tile {_simplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5]),(3,[2.0,0.0])], _circumcenter = [1.0,0.5], _circumradius = 1.118033988749895, _volume' = 1.5}, _neighborsIds = fromList [1], _facetsIds = fromList [0,1,2], _family' = None, _toporiented = False}),(1,Tile {_simplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5]),(2,[0.0,2.0])], _circumcenter = [0.5,1.0], _circumradius = 1.118033988749895, _volume' = 1.5}, _neighborsIds = fromList [0], _facetsIds = fromList [2,3,4], _family' = None, _toporiented = True})], _tilefacets = fromList [(0,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(1,[1.5,1.5]),(3,[2.0,0.0])], _circumcenter = [1.75,0.75], _circumradius = 0.7905694150420949, _volume' = 1.5811388300841898}, _facetOf = fromList [0], _normal' = [0.9486832980505138,0.31622776601683794], _offset' = 1.8973665961010275}),(1,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(3,[2.0,0.0])], _circumcenter = [1.0,0.0], _circumradius = 1.0, _volume' = 2.0}, _facetOf = fromList [0], _normal' = [0.0,-1.0], _offset' = 0.0}),(2,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(1,[1.5,1.5])], _circumcenter = [0.75,0.75], _circumradius = 1.0606601717798212, _volume' = 2.1213203435596424}, _facetOf = fromList [0,1], _normal' = [0.7071067811865476,-0.7071067811865476], _offset' = 0.0}),(3,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(1,[1.5,1.5]),(2,[0.0,2.0])], _circumcenter = [0.75,1.75], _circumradius = 0.7905694150420949, _volume' = 1.5811388300841898}, _facetOf = fromList [1], _normal' = [0.31622776601683794,0.9486832980505138], _offset' = -1.8973665961010275}),(4,TileFacet {_subsimplex = Simplex {_vertices' = fromList [(0,[0.0,0.0]),(2,[0.0,2.0])], _circumcenter = [0.0,1.0], _circumradius = 1.0, _volume' = 2.0}, _facetOf = fromList [1], _normal' = [-1.0,0.0], _offset' = 0.0})]
-- , _edges' = fromList [(Pair 0 1,([0.0,0.0],[1.5,1.5])),(Pair 0 2,([0.0,0.0],[0.0,2.0])),(Pair 0 3,([0.0,0.0],[2.0,0.0])),(Pair 1 2,([1.5,1.5],[0.0,2.0])),(Pair 1 3,([1.5,1.5],[2.0,0.0])) ]
    }
-}
-- type NodeX = Double  
-- type P2_X = (NodeID, NodeX)
p2_node_y i p = (i, p ^. v2._y)
p2s_node_y off1 p2s = zipWith p2_node_y [off1 ..] p2s
node_y_four = p2s_node_y 40 fourP2

-- -- import           Uniform.Strings hiding ((</>), (<.>), S)

-- -- fourP :: [PtTuple Int]
-- fourPoints :: [(Double, Double, Int)]
-- -- points to form two triangles
-- fourPoints = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]

-- fivePoints :: [(Double, Double, Int)]
-- -- to form 3 triangles
-- fivePoints = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]

-- fourP2 = map tup2P2 fourPoints

-- tup2P2 :: (Double, Double, Int) -> P2 
-- tup2P2 (x,y,i)= Point2d i (V2 x y)
-- p2_tup_id :: P2 -> (Double, Double, Int)
-- p2_tup_id p7 = (p7 ^. v2._x, p7 ^. v2._y, p7 ^. p2id)
--     -- with pattern matching simpler?
-- -- p2_tup_id (Point2d i (V2 x y)) = (x,y,i)

-- p2_tup :: P2 -> (Int, [Double])
-- p2_tup (Point2d i (V2 x y)) =(i, [x, y])


-- fiveP2 :: [P2]
-- fiveP2 = map tup2P2 fivePoints 

-- fiveD = map (snd . p2_tup) fiveP2

-- fivemap :: Map.Map Int [Double]
-- fivemap = Map.fromList . fmap p2_tup $ fiveP2



-- data Point2d i v = Point2d {_p2id:: i, _v2:: V2 v}
--     deriving (Show, Read, Ord, Eq, Generic)
-- instance (Zeros i, Zeros v, Num v) => Zeros (Point2d i v) where zero = Point2d zero zero 
-- instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
-- instance Zeros Double where zero = 0.0
-- makeLenses ''Point2d

-- type P2 = Point2d Int Double
