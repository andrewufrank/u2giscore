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
-- {-# HLINT ignore "Redundant $" #-}
-- {-# HLINT ignore "Use map once" #-}
-- {-# HLINT ignore "Use newtype instead of data" #-}
-- {-# HLINT ignore "Use fewer imports" #-}


module Uniform.TesselationHalfQuads
    ( module Uniform.TesselationHalfQuads
    -- , module Uniform.DelaunayTriples
    -- , module Uniform.DelaunayTriples
    -- , module Uniform.Delaunay
    -- , module Uniform.DelaunayTiles
    , module Uniform.Point2dData
    , module Uniform.Point2d
    , module Linear.V2
    , module Control.Lens
    , Tesselation  
        ) 
         where

import UniformBase
import Uniform.Point2d
import Uniform.Point2dData
import Uniform.GeometryFunctions
import qualified Data.Map as Map 
-- import Uniform.Delaunay
-- import Uniform.DelaunayTiles
-- import Uniform.DelaunayTriples
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
-- import GHC.Generics
  
import Delaunay.Types
import Delaunay
-- import Qhull.Types
-- import qualified Data.Map as Ix
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict  as IM
-- import           Data.HashMap.Strict.InsOrd as H hiding (map)
import Language.Haskell.TH.Lens (_Overlapping)
-- import Uniform.TesselationHalfQuads (TesselationHQ(_HQs))


-- | a data structure to represent a tesselation (and its dual)
-- with Nodes and Faces (dual to each other)
-- and half of quad edges (see Guibas & stolfi)
data NodeHQ = NodeHQ V2d
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
data FaceHQ = FaceHQ {circumcenter ::V2d} deriving (Show, Read, Ord, Eq, Generic, Zeros)



data HQ = HQ 
    { node:: Int    -- ^ end of hq (start or end of edge)
    , face::Maybe Int -- ^ face right of the hq
    , twin::Int     -- the other hq for the edge
    , halflength :: Double  -- the half of the length of the edge
    } 
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

data TesselationHQ = TesselationHQ {
          _Nodes      :: [NodeHQ]
        , _Faces      :: [FaceHQ]
        , _HQs       :: [HQ]      -- ^ the tileface starting and ending, alternating
        } deriving Show

-- | conversion of the Tesselation data structure from qhull to the HQ form
-- all indices are local
toHq1 :: Tesselation -> TesselationHQ
toHq1 t = TesselationHQ 
    { _Nodes = map (NodeHQ . toV2 . _point)  
        . IM.elems . _sites $ t
    , _Faces = map (FaceHQ .   toV2 .  _circumcenter .   _simplex ) ts
           
    , _HQs = zipWith (\tf i -> 
            HQ {node =  (!!0) . IM.keys . _vertices'  . _subsimplex $ tf
                , face = testSide tf ts  (start tf) (end tf)
                , twin =  tfCount + i   
                , halflength = (/2) . _volume' . _subsimplex $ tf
            }  ) tfs [0..]
            ++ zipWith (\tf i -> 
            HQ {node =  (!!1) . IM.keys . _vertices'  . _subsimplex $ tf
                , face = testSide tf ts  (end tf) (start tf) 
                , twin =    i   
                , halflength = (/2) . _volume' . _subsimplex $ tf
            } ) tfs [0..]
    }
        where 
            tfs :: [TileFacet]
            tfs =  IM.elems . _tilefacets $ t
            tfCount = length tfs 
            ts :: [Tile]
            ts = IM.elems . _tiles $ t
            start tf =  (!!0) . vertices1x $ tf
            end tf =  (!!1) . vertices1x $ tf
            vertices1x :: TileFacet -> [([Double])]
            vertices1x tf = IM.elems . _vertices' . _subsimplex $ tf 

-- test for each face mentioned in a tilefacet on which side it is 
-- true - startHQ,
-- for second hq swtich start and  end
-- returns the tile to the right of the hq (in the direction of the hq)
-- Nothing if none 

testSide :: TileFacet -> [Tile] -> [Double] -> [Double]->  Maybe Int 
testSide tft tiles startxy endxy = listToMaybe . catMaybes $ res
            -- assumes that center is on one (but noth both) sides
            -- possible numerical issue
    where 
        facetofs1 :: [Int] -- the list of faces
        facetofs1 =    IS.elems . _facetOf  $ tft
        centerxy i = _circumcenter . _simplex . (!! i)  $ tiles 
        centers = map centerxy facetofs1
        -- vertices1 ::  [(IM.Key, [Double])]
        -- vertices1 =   IM.assocs .  _vertices' .  _subsimplex $ tft
        -- (startid, startxy) =  (!!0)  vertices1 
        -- (endid, endxy) =  vertices1 !! 1
        ccws = map  (ccw_test startxy endxy) centers
            -- test the center to determine which side of face 
        res = zipWith (\b i -> if b then Just i else Nothing) ccws facetofs1



mainHQ :: ErrIO ()
mainHQ = do 
    putIOwords ["the conversion to a tesselation As Half-Quads"]
    tess4 <- liftIO $ delaunay (map (v2toList2 . p2toV2) fourP2) False False Nothing
    putIOwords ["the given tesselation", showT tess4]
    putIOwords ["point2d two\n", showT (toHq1 tess4), "\n"]
    -- putIOwords ["point2d two", showT tess4, "\n"]

