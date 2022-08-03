-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pretty_test
--
-- | import examples to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.TessHQ_test where

import           Test.Framework hiding (scale)
-- import           Uniform.Strings hiding ((</>), (<.>), (<|>))
-- import   Uniform.Point2d
import Uniform.Point2dData
import UniformBase

-- import Control.Exception
import Uniform.GeometryFunctions
-- import qualified Data.Geometry.Point as HP 

tess = unCatStore catstoreTess 
catstoreTess = CatStoreK  [(NodeTag (Node 400),XYtag XY,PointTag (Point2d 0.0 0.0)),
    (NodeTag (Node 401),XYtag XY,PointTag (Point2d 1.5 1.5)),
    (NodeTag (Node 402),XYtag XY,PointTag (Point2d 0.0 2.0)),
    (NodeTag (Node 403),XYtag XY,PointTag (Point2d 2.0 0.0)),
    (FaceTag (Face 400),XYtag XY,PointTag (Point2d 1.0 0.5)),
    (FaceTag (Face 401),XYtag XY,PointTag (Point2d 0.5 1.0)),
    (HQTag (Hq 400),HqNodeTag HqNode,NodeTag (Node 401)),
    (HQTag (Hq 400),TwinTag Twin,HQTag (Hq 405)),
    (HQTag (Hq 400),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 401),HqNodeTag HqNode,NodeTag (Node 400)),
    (HQTag (Hq 401),HqFaceTag HqFace,FaceTag (Face 400)),
    (HQTag (Hq 401),TwinTag Twin,HQTag (Hq 406)),
    (HQTag (Hq 401),DistTag Distant,LengthTag (Length 1.0)),
    (HQTag (Hq 402),HqNodeTag HqNode,NodeTag (Node 400)),
    (HQTag (Hq 402),HqFaceTag HqFace,FaceTag (Face 401)),
    (HQTag (Hq 402),TwinTag Twin,HQTag (Hq 407)),
    (HQTag (Hq 402),DistTag Distant,LengthTag (Length 1.0606601717798212)),
    (HQTag (Hq 403),HqNodeTag HqNode,NodeTag (Node 401)),
    (HQTag (Hq 403),HqFaceTag HqFace,FaceTag (Face 401)),
    (HQTag (Hq 403),TwinTag Twin,HQTag (Hq 408)),
    (HQTag (Hq 403),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 404),HqNodeTag HqNode,NodeTag (Node 400)),
    (HQTag (Hq 404),TwinTag Twin,HQTag (Hq 409)),
    (HQTag (Hq 404),DistTag Distant,LengthTag (Length 1.0)),
    (HQTag (Hq 405),HqNodeTag HqNode,NodeTag (Node 403)),
    (HQTag (Hq 405),HqFaceTag HqFace,FaceTag (Face 400)),
    (HQTag (Hq 405),TwinTag Twin,HQTag (Hq 400)),
    (HQTag (Hq 405),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 406),HqNodeTag HqNode,NodeTag (Node 403)),
    (HQTag (Hq 406),TwinTag Twin,HQTag (Hq 401)),
    (HQTag (Hq 406),DistTag Distant,LengthTag (Length 1.0)),
    (HQTag (Hq 407),HqNodeTag HqNode,NodeTag (Node 401)),
    (HQTag (Hq 407),HqFaceTag HqFace,FaceTag (Face 400)),
    (HQTag (Hq 407),TwinTag Twin,HQTag (Hq 402)),
    (HQTag (Hq 407),DistTag Distant,LengthTag (Length 1.0606601717798212)),
    (HQTag (Hq 408),HqNodeTag HqNode,NodeTag (Node 402)),
    (HQTag (Hq 408),TwinTag Twin,HQTag (Hq 403)),
    (HQTag (Hq 408),DistTag Distant,LengthTag (Length 0.7905694150420949)),
    (HQTag (Hq 409),HqNodeTag HqNode,NodeTag (Node 402)),
    (HQTag (Hq 409),HqFaceTag HqFace,FaceTag (Face 401)),
    (HQTag (Hq 409),TwinTag Twin,HQTag (Hq 404)),
    (HQTag (Hq 409),DistTag Distant,LengthTag (Length 1.0))] :: CatStorePoint