module Lib (compareBranches) where

-- TODO: error handling

import qualified    Data.Map.Strict as Map
import qualified    Data.Map.Merge.Strict as Map.Merge
import qualified    EVRComparison as EVR
import qualified    Net (getBranchInfo, PackageInfo(..), BranchInfo(..))


type Branch = String

type PackageName = String
type PackageArch = String
type PackageEpoch = Integer
type PackageVersion = String
type PackageRelease = String
-- EVR is for epoch-version-release triplet
type PackageEVR = (PackageEpoch, PackageVersion, PackageRelease)
type PackageMap = Map.Map PackageName PackageEVR

data ArchDiff = ArchDiff {
    extraPackages   :: [(PackageName, PackageEVR)],
    missingPackages :: [(PackageName, PackageEVR)],
    -- Newer EVR and older
    newerPackages   :: [(PackageName, (PackageEVR, PackageEVR))]
} deriving (Show)


packageInfoToPair :: Net.PackageInfo -> (PackageName, PackageEVR)
packageInfoToPair p = (Net.name p, (Net.epoch p, Net.version p, Net.release p))

branchInfoToMap :: Net.BranchInfo -> Map.Map PackageArch PackageMap
branchInfoToMap bi = Map.map Map.fromList archMap
    where archMap = Map.fromListWith (++) $ map (\p -> (Net.arch p, [packageInfoToPair p])) $ Net.packages bi


compareArches :: PackageMap -> PackageMap -> ArchDiff
compareArches m1 m2 = ArchDiff { extraPackages=extra, missingPackages=missing, newerPackages=newer }
    where   extra   = Map.toList $ Map.difference m1 m2
            missing = Map.toList $ Map.difference m2 m1
            inter   = Map.toList $ Map.intersectionWith (,) m1 m2
            newer   = (filter $ (==GT) . uncurry EVR.compareEVR . snd) inter

newArchToArchDiff :: PackageMap -> ArchDiff
newArchToArchDiff m = ArchDiff { extraPackages=Map.toList m, missingPackages=[], newerPackages=[]}

compareBranches :: Branch -> Branch -> IO (Maybe (Map.Map PackageArch ArchDiff))
compareBranches fstBranch sndBranch = do
    bInfoFst <- Net.getBranchInfo fstBranch
    bInfoSnd <- Net.getBranchInfo sndBranch
    let m1 = fmap branchInfoToMap bInfoFst
    let m2 = fmap branchInfoToMap bInfoSnd
    let extraArch = Map.Merge.mapMissing $ const newArchToArchDiff
    let missingArch = Map.Merge.mapMissing $ const newArchToArchDiff
    let sameArch = Map.Merge.zipWithMatched $ const compareArches
    let diff = liftA2 (Map.Merge.merge extraArch missingArch sameArch) m1 m2
    return diff