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


main =
  startProg
    (unwords' ["Uniform.Geometry", "the test for geometry"])
    ( do
        -- let sitefn = makeAbsFile "/home/frank/Workspace11/ssg/docs/site/settings3"
        workingdir1 :: Path Abs Dir <- currentDir 
        let sitefn = workingdir1 </> (makeRelFile "settings3" )  
        start NoticeLevel0 sitefn
    )

start debug fn = do 
    return ()