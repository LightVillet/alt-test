module Main (main) where

import Lib (compareBranches)

main :: IO ()
main = do
    res <- compareBranches "p10" "p9"
    print res
    return ()
