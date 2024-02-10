{-# LANGUAGE OverloadedStrings #-}
-- Because of JSON parsing
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- TODO: error handling

module Net (getBranchInfo, BranchInfo(..), PackageInfo(..)) where
-- Since AltLinux doesn't have http-conduit package, I should parse JSON by hand

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Control.Monad (mzero)


data BranchInfo = BranchInfo {
    len         :: Integer,
    packages    :: [PackageInfo]
}

instance FromJSON BranchInfo where
    parseJSON (Object jsonObj)  = BranchInfo    <$> jsonObj .: "length"
                                                <*> jsonObj .: "packages"
    parseJSON _                 = mzero

data PackageInfo = PackageInfo {
    name        :: String,
    epoch       :: Integer,
    version     :: String,
    release     :: String,
    arch        :: String,
    disttag     :: String,
    buildtime   :: Integer,
    source      :: String
}

instance FromJSON PackageInfo where
    parseJSON (Object jsonObj) = PackageInfo    <$> jsonObj .: "name"
                                                <*> jsonObj .: "epoch"
                                                <*> jsonObj .: "version"
                                                <*> jsonObj .: "release"
                                                <*> jsonObj .: "arch"
                                                <*> jsonObj .: "disttag"
                                                <*> jsonObj .: "buildtime"
                                                <*> jsonObj .: "source"
    parseJSON _                 = mzero

-- Get data from API
getBranchInfo :: String -> IO (Maybe BranchInfo)
getBranchInfo branch = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest $ "https://rdb.altlinux.org/api/export/branch_binary_packages/" ++ branch
    response <- httpLbs request manager
    let body = responseBody response
    return $ decode body
