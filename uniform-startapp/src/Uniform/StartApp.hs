--{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE StandaloneDeriving  #-}
 -- {-# OPTIONS -Wall #-}


module Uniform.StartApp(
    module Uniform.StartApp
    -- , module UniformBase
--    , module Uniform.Strings
        )   where


import UniformBase
-- import UniformBase (Path(..), Read(..))
-- import           Uniform.Error
--import           Uniform.Strings
-- import GHC.Read

-- data Aby44 = Aby44 Int  (Path Abs Dir)  deriving (Eq, Ord, Show, Read )
-- data Aby = Aby44 Int  (Path Abs Dir)  deriving (Eq, Ord, Show )
-- deriving instance Read (Path Abs Dir) 
    -- where readsPrec = undefined 

startProgWithTitle :: Show a => Text -> Text -> ErrIO a -> IO ()
startProgWithTitle programName   progTitle mainProg = do  -- (mainProg prefsfilename gladefilename ) = do
--        putIOwords ["the files to start with \n"
--            ,"\n", "prefsfile", prefsfilename
--            , "\ngladefile", gladefilename]
        putIOwords [    "------------------ ", 
                        programName ,   progTitle, 
                        " ----------------------------"]
        r <- runErr $ mainProg
        putIOwords 
            [ "\n------------------", "main", progTitle
            , "\nreturning", either id showT r
            , "\n -------------------------"]
        return ()
    `catchError` (\e  -> do
            putIOwords 
                [ "startProg error caught\n", programName, progTitle
                , "\n", showT e ] -- " showT msg])
            return ()
            )
