-----------------------------------------------------------------------------
--
-- Module      :   main for building geometry module
-----------------------------------------------------------------------------
    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where



 
import UniformBase
import Uniform.Point2d
import Uniform.Point2dData 

import Delaunay.Delaunay
import Voronoi2D
import Uniform.Delaunay 
import Uniform.DelaunayTiles
import Uniform.DelaunayTriples

import qualified Data.IntMap.Strict  as IM
import           Text.Show.Pretty
import Control.Lens 
import           Delaunay

import Delaunay.Types
import Qhull.Types

main =
  startProg
    (unwords' ["Uniform.Geometry", "the test for geometry"])
    ( do
        -- let sitefn = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/settings3"
        workingdir1 :: Path Abs Dir <- currentDir 
        let sitefn = workingdir1 </> (makeRelFile "settings3" )  
        start NoticeLevel0 sitefn
    )

p1 = Point2d 1 (V2 1 1):: P2

fourDouble :: [[Double]]
fourDouble = map (snd . p2_tup) fourP2 

delaunayResult = delaunay fourDouble False False Nothing

start :: MonadIO m => p1 -> p2 -> m ()
start debug fn = do 
    -- let p1v2 =  p1 ^. v2 
    -- putIOwords ["piv2", showT p1v2]
    -- putIOwords ["piv2", showT p1v2]
    -- putIOwords ["point2d zero", showT (zero::P2)]
    -- putIOwords ["point2d two", showT (fourP2)]
    putIOwords ["point2d two", showT (fourDouble)]
    res4 <- liftIO $ delaunay fourDouble False False Nothing
    putIOwords ["point2d two", showT res4, "\n"]
    -- -- res4x <- liftIO $ delaunay fourDouble True False Nothing
    -- -- -- with point at infinity -- no difference observable
    -- -- putIOwords ["point2d two", showT res4x]
    -- -- let resVor1 = voronoi2 res4
    -- -- putIOwords ["voronoi from res", showT resVor1]
    -- -- -- putIOwords ["voronoi from res", showT voronoi1]
    -- -- liftIO $ prettyShowVoronoi2 resVor1 Nothing 
    -- -- -- building the triples
    -- putIOwords ["five name", showT node_name_five]
    -- putIOwords ["four name", showT node_name_four]
    -- putIOwords ["four x", showT node_x_four]
    -- putIOwords ["four y", showT node_y_four]
    -- putIOwords ["four name x y", showT $ zip3 node_name_four node_x_four node_y_four]

    let vs4 = vertices res4
    -- let vs4 = IM.elems $ _vertices res4 :: [[Double]]
    liftIO $ pPrint vs4
    liftIO $ putIOwords ["vs4", showT vs4]
    putIOwords ["tiles res4\n", showT $ tiles2 res4]
    putIOwords ["no_tiles res4\n", showT $ no_tiles res4]
    putIOwords ["circum2 res4\n", showT $ center res4]
    putIOwords ["surface res4\n", showT $ surface res4]
    putIOwords ["toporiented res4\n", showT $ toporiented res4]
    putIOwords ["\n edges \n", showT $ edges res4]
    putIOwords ["tilefaces1 \n", showT $ tilefacets1 res4]
    putIOwords ["length1 \n", showT $ length1 res4]
    putIOwords ["simplex3 \n", showT $ simplex3 res4]
    putIOwords ["vertices3 \n", showT $ vertices3 res4]
    putIOwords ["start3 \n", showT $ start3 res4]
    putIOwords ["end3 \n", showT $ end3 res4]
    putIOwords ["facetof3 \n", showT $ facetof3 res4]

    let hqs = trip_hqs_faces 400 res4
    putIOwords ["\nall the face hqs for res4\n", showT hqs]
    return ()