module Main where

import Yage.Prelude hiding (head)

import Data.List
import Data.Proxy


import Yage.Geometry.Vertex
import Yage.Geometry
import qualified Yage.Geometry.Formats.Ygm as YGM
import Yage.Geometry.Formats.Ygm ()
import qualified Yage.Geometry.Formats.Obj as OBJ

type OBJVertex = P3T2 "pos" "tex" Float
type YGMVertex = P3T2NT3 "pos" "tex" "norm" "tan" Float

main :: IO ()
main = do
    importFile <- fpFromText . head <$> getArgs
    inGeo <- OBJ.geometryFromOBJFile (Proxy::Proxy OBJVertex) importFile
    let name        = fpToText . basename $ importFile
        exportFile  = basename importFile <.> "ygm"
        exportGeo   = genSmoothings' inGeo
        ygm         = YGM.YGM name exportGeo
    YGM.ygmToFile exportFile ygm
    print ygm

    fileCheck <- YGM.ygmFromFile exportFile (Proxy::Proxy YGMVertex)
    print fileCheck
    --print $ format "file correct: {0}" [show $ fileCheck == ygm]

genSmoothings' :: TriGeo (Vertex OBJVertex) -> TriGeo (Vertex YGMVertex)
genSmoothings' = genSmoothings 
                    (position3 :: Position3 "pos" Float)
                    (texture2  :: Texture2  "tex" Float)
                    (normal3   :: Normal3   "norm" Float)
                    (tangent3  :: Tangent3  "tan" Float)