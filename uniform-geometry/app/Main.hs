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
import Uniform.Point
import Uniform.PointData 

import Delaunay.Delaunay
import Voronoi2D
import Uniform.Delaunay  

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
    let p1v2 =  p1 ^. v2 
    putIOwords ["piv2", showT p1v2]
    putIOwords ["piv2", showT p1v2]
    putIOwords ["point2d zero", showT (zero::P2)]
    putIOwords ["point2d two", showT (fourP2)]
    putIOwords ["point2d two", showT (fourDouble)]
    res <- liftIO $ delaunay fourDouble False False Nothing
    putIOwords ["point2d two", showT res]
    res2 <- liftIO $ delaunay fourDouble True False Nothing
    -- with point at infinity -- no difference observable
    putIOwords ["point2d two", showT res2]
    let resVor1 = voronoi2 res
    putIOwords ["voronoi from res", showT resVor1]
    -- putIOwords ["voronoi from res", showT voronoi1]
    liftIO $ prettyShowVoronoi2 resVor1 Nothing 
    -- building the triples
    putIOwords ["five name", showT node_name_five]
    putIOwords ["four name", showT node_name_four]
    putIOwords ["four x", showT node_x_four]
    putIOwords ["four y", showT node_y_four]
    putIOwords ["four name x y", showT $ zip3 node_name_four node_x_four node_y_four]
    return ()