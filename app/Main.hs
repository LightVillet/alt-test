module Main (main) where

import Lib (compareBranches)
import Data.Aeson (encode)

main :: IO ()
main = do
    res <- compareBranches "p10" "p9"
    either print (print . encode) res
    return ()
