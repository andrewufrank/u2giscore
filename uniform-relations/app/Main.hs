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
import Uniform.Rels2
-- import ExampleData.HQexampleLong
-- import ExampleData.HQexampleShort
-- import ExampleData.HQfaces_test

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
    

        main2
    )

main2 :: ErrIO () 
main2 = do 


    return () 
    
