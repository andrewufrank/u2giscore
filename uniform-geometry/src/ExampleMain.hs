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

module ExampleMain     where



 
import UniformBase
import Uniform.Point

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

start debug fn = do 
    let p1v2 =  p1 ^. v2 
    putIOwords ["piv2", showT p1v2]
    putIOwords ["piv2", showT p1v2]
    putIOwords ["point2d zero", showT (zero::P2)]
    return ()