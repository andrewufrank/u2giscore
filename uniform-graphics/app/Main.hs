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
-- import Uniform.Point2d
-- import Uniform.Point2dData 
-- import Uniform.GeometryFunctions
-- -- import Delaunay.Delaunay
-- -- import Voronoi2D
-- -- import Uniform.Delaunay 
-- -- import Uniform.DelaunayTiles
-- -- import Uniform.DelaunayTriples
-- import Uniform.TesselationHalfQuads
import Uniform.Drawings 

-- import qualified Data.IntMap.Strict  as IM
-- import           Text.Show.Pretty
-- import Control.Lens 
-- import           Delaunay

-- import Delaunay.Types
-- import Qhull.Types

main =
  startProg
    (unwords' ["Uniform.Graphics", "the test for graphical output"])
    ( do
        -- let sitefn = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/settings3"
        -- workingdir1 :: Path Abs Dir <- currentDir 
        -- let sitefn = workingdir1 </> (makeRelFile "settings3" )  
        -- start NoticeLevel0 sitefn

        main2
    )

main2 :: ErrIO () 
main2 = do 
 
    showFacePage
    
