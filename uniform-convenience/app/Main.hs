-----------------------------------------------------------------------------
--Main.hs
-- Module      :  Main
-- Copyright   :  andrew u frank 2016
--
-- | test  the error modue
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE
--     MultiParamTypeClasses
--     , TypeSynonymInstances
-- --    , FunctionalDependencies
--     , FlexibleInstances
-- --    , FlexibleContexts
-- --    , DeriveFunctor
--     , ScopedTypeVariables
-- --    , UndecidableInstances
     OverloadedStrings
    #-}
-- -- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main (main) where


--import qualified Data.Text as T (Text)
import UniformBase
-- -- import Uniform.Zero
-- import System.Exit
-- --import Data.Easy
-- import TestingConvenience

programName = "error-0.0.7"

debug_main  =  True


main :: IO ()
main = do
    putIOwords  ["start  \n" ]
    putIOwords  [ "------------------ ", programName
--                , toText versionNum
            , " -------------------------"]
    putIOwords ["read Path test", showT c1, showT c2, showT c3, showT $ c1 == c3]
    -- r1 <- convenienceTest
    -- putIOwords["main", programName, "returning\n"
    --         , show' r1
    --         , "-------------------------\n\n"]
    -- let bs = [ r1 ]
    -- putIOwords ["convenienceTest  Main", showT bs]
    -- if (and bs) then exitSuccess else exitFailure


-- test for read of Path 

data C  = C Float  (Path Abs File)  deriving (Eq, Ord, Show, Read) 
p = makeAbsFile "/home/testquarto" :: Path Abs File 
c1 = C 1.1 p

c2 = show c1 :: String 
c3 = read c2 :: C 
-- test_readPath = assertEqual  c1 c3
