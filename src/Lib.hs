{-# LANGUAGE OverloadedStrings #-}
-- Because of JSON parsing
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib (compareBranches) where

import qualified    Data.Map.Strict as Map
import              Data.Aeson
import              Network.HTTP.Simple
import              Control.Monad (mzero)
import              Control.Applicative (liftA3)


type Branch = String

type PackageName = String
type PackageEpoch = Integer
type PackageVersion = String
type PackageRelease = String
-- EVR is for epoch-version-release triplet
type PackageEVR = (PackageEpoch, PackageVersion, PackageRelease)
type PackageTuple = (PackageName, PackageEVR)
type PackagesMap = Map.Map PackageName PackageEVR

data BranchInfo = BranchInfo {
    len         :: Integer,
    packages    :: [PackageInfo]
}

instance FromJSON BranchInfo where
    parseJSON (Object jsonObj)  = BranchInfo    <$> jsonObj .: "length"
                                                <*> jsonObj .: "packages"
    parseJSON _                 = mzero

data PackageInfo = PackageInfo {
    name        :: PackageName,
    epoch       :: Integer,
    version     :: PackageVersion,
    release     :: PackageRelease,
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
getBranchInfo :: Branch -> IO (Maybe BranchInfo)
getBranchInfo branch = do
    initReq <- parseRequest $ "https://rdb.altlinux.org/api/export/branch_binary_packages/" ++ branch
    resp <- httpBS initReq
    let body = getResponseBody resp
    return $ decodeStrict body

data BranchDiff = BranchDiff {
    extraPackages   :: [PackageTuple],
    missingPackages :: [PackageTuple],
    -- Newer EVR and older
    newerPackages   :: [(PackageName, (PackageEVR, PackageEVR))]
} deriving (Show)

getPackagePair :: PackageInfo -> PackageTuple
getPackagePair p = (name p, (epoch p, version p, release p))

branchInfoToMap :: BranchInfo -> PackagesMap
branchInfoToMap = Map.fromList . fmap getPackagePair . packages

mapADiffToList :: (Applicative f, Ord k) => f (Map.Map k a) -> f (Map.Map k b) -> f [(k, a)]
mapADiffToList m1 m2 = Map.toList <$> liftA2 Map.difference m1 m2

compareBranches :: Branch -> Branch -> IO (Maybe BranchDiff)
compareBranches fstBranch sndBranch = do
    bInfoFst <- getBranchInfo fstBranch
    bInfoSnd <- getBranchInfo sndBranch
    let packagesFst = fmap branchInfoToMap bInfoFst
    let packagesSnd = fmap branchInfoToMap bInfoSnd
    let extra = mapADiffToList packagesFst packagesSnd
    let missing = mapADiffToList packagesSnd packagesFst
    let inter = liftA2 (Map.intersectionWith (,)) packagesFst packagesSnd
    -- Not actually a newer from now, waiting for rework comparison to rpmvercmp
    let newer = fmap Map.toList inter
    return $ liftA3 BranchDiff extra missing newer