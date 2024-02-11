module Main (main) where

import Lib (compareBranches)
import Data.Aeson (encode)
import Data.ByteString.Lazy as L

main :: IO ()
main = do
    res <- compareBranches "p10" "p9"
    either print (L.putStr . encode) res
    return ()
