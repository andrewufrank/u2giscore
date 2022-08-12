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
-- import ExampleData.HQexampleLong
import ExampleData.HQexampleShort
import ExampleData.HQfaces_test



main =
  startProg
    (unwords' ["Uniform.Relations", "the test for relatiosn and triple store"])
    ( do

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
    -- mainMakeTessShort  -- from ExampleData.HQexampleShort
    -- pageHQfaces_test
    -- pageHQforglossFaces
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