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
    , module Uniform.Point2dData
    , module Uniform.Point2d
    , module Linear.V2
    , module Control.Lens
        )  where

import UniformBase
import Uniform.Point2d
import Uniform.Point2dData
import qualified Data.Map as Map 

-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics
import Delaunay
import Delaunay.Types
import GHC.Base (Alternative(many))


-- ----------------------------for nodes 
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
p2_node_y i p = (i,    p ^. v2._y)
p2s_node_x :: Integer -> [P2] -> [(NodeID, Coord)]
p2s_node_x off1 p2s = zipWith p2_node_x (map N [off1 ..]) p2s
p2s_node_y off1 p2s = zipWith p2_node_y (map N [off1 ..]) p2s
node_x_four :: [(NodeID, Coord)]
node_x_four = p2s_node_x 40 fourP2
node_y_four = p2s_node_y 40 fourP2

mainDelaunay :: ErrIO ()
mainDelaunay = do 
    putIOwords ["point2d two", showT (fourP2)]
    res4 <- liftIO $ delaunay (map (v2toList2 . p2toV2) $ fourP2) False False Nothing
    putIOwords ["point2d two", showT res4, "\n"]
    -- -- res4x <- liftIO $ delaunay fourDouble True False Nothing
    -- -- -- with point at infinity -- no difference observable
    -- -- putIOwords ["point2d two", showT res4x]
    -- -- let resVor1 = voronoi2 res4
    -- -- putIOwords ["voronoi from res", showT resVor1]
    -- -- -- putIOwords ["voronoi from res", showT voronoi1]
    -- -- liftIO $ prettyShowVoronoi2 resVor1 Nothing 
    -- -- -- building the triples
    putIOwords ["five name", showT node_name_five]
    putIOwords ["four name", showT node_name_four]
    -- putIOwords ["four x", showT node_x_four]
    -- putIOwords ["four y", showT node_y_four]
    putIOwords ["four name x y", showT $ zip3 node_name_four node_x_four node_y_four]    



