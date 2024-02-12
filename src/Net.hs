{-# LANGUAGE OverloadedStrings #-}
-- Because of JSON parsing
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Net (getBranchInfo, BranchInfo(..), PackageInfo(..)) where
-- Since AltLinux doesn't have http-conduit package, I should parse JSON by hand

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Control.Monad (mzero)
import Control.Exception (try)
import Network.Connection
import Network.TLS

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
getBranchInfo :: String -> IO (Either String BranchInfo)
getBranchInfo branch = do
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    request <- parseRequest $ "https://rdb.altlinux.org/api/export/branch_binary_packages/" ++ branch
    responseTry <- try $ httpLbs request manager
    let response = case responseTry of
            Left e      -> Left $ show (e :: HttpException)
            Right res   -> Right res
    let body = responseBody <$> response
    return $ eitherDecode =<< body
