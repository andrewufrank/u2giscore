-----------------------------------------------------------------------------
--
-- Module      :   main for building relations and triples 
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
import Uniform.NaiveTripleStore 
import Uniform.TripleStore
import Uniform.TripleRels
import ExampleData.HQexampleLong
import ExampleData.HQexampleShort
import ExampleData.HQfaces_test

-- import Uniform.Point2d
-- import Uniform.Point2dData 

-- -- import Delaunay.Delaunay
-- import Voronoi2D
-- -- import Uniform.Delaunay 
-- -- import Uniform.DelaunayTiles
-- -- import Uniform.DelaunayTriples
-- import Uniform.TesselationHalfQuads 

-- import qualified Data.IntMap.Strict  as IM
-- import           Text.Show.Pretty
-- import Control.Lens 
-- import           Delaunay

-- -- import Delaunay.Types
-- import Qhull.Types

main =
  startProg
    (unwords' ["Uniform.Relations", "the test for relatiosn and triple store"])
    ( do
        -- let sitefn = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/settings3"
        -- workingdir1 :: Path Abs Dir <- currentDir 
        -- let sitefn = workingdir1 </> (makeRelFile "settings3" )  
        -- start NoticeLevel0 sitefn

        main2
    )

main2 :: ErrIO () 
main2 = do 
    -- mainPoint2dData
    -- mainDelaunay
    -- mainDelaunayTiles
    -- mainDelaunayTriples
    -- mainHQ
    -- mainMakeTessLong
    -- mainMakeTessShort
    pageHQfaces_test
    return () 
    
-- p1 = Point2d 1 (V2 1 1):: P2

-- fourDouble :: [[Double]]
-- fourDouble = map (snd . p2_tup) fourP2 

-- -- delaunayResult = delaunay fourDouble False False Nothing

-- -- start :: MonadIO m => p1 -> p2 -> m ()
-- -- start debug fn = do 

 


--     -- let hqs = trip_hqs_faces 400 res4
--     -- putIOwords ["\nall the face hqs for res4\n", showT hqs]
--     return ()