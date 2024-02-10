module Lib (compareBranches) where

-- TODO: error handling

import qualified    Data.Map.Strict as Map
import              Control.Applicative (liftA3)
import              EVRComparison (compareEVR)
import              Net (getBranchInfo, PackageInfo(..), BranchInfo(..))


type Branch = String

type PackageName = String
type PackageEpoch = Integer
type PackageVersion = String
type PackageRelease = String
-- EVR is for epoch-version-release triplet
type PackageEVR = (PackageEpoch, PackageVersion, PackageRelease)
type PackageTuple = (PackageName, PackageEVR)
type PackagesMap = Map.Map PackageName PackageEVR


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
    let inter = Map.toList <$> liftA2 (Map.intersectionWith (,)) packagesFst packagesSnd
    let newer = fmap (filter $ (==GT) . uncurry compareEVR . snd) inter
    return $ liftA3 BranchDiff extra missing newer