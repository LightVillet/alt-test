{-# LANGUAGE DeriveGeneric #-}

module AltBranches (compareBranches) where

import qualified GHC.Generics as Generics
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified EVRComparison as EVR
import qualified Net (getBranchInfo, PackageInfo(..), BranchInfo(..))
import qualified Control.Applicative as A
import Foreign.C.String


type PackageName    = String
type PackageArch    = String
type PackageEpoch   = Integer
type PackageVersion = String
type PackageRelease = String
type PackageEVR     = (PackageEpoch, PackageVersion, PackageRelease)

data Package = Package {
    name    :: PackageName,
    epoch   :: PackageEpoch,
    version :: PackageVersion,
    release :: PackageRelease
} deriving (Generics.Generic, Show)

data ArchDiff = ArchDiff {
    extraPackages   :: [Package],
    missingPackages :: [Package],
    newerPackages   :: [Package]
} deriving (Generics.Generic, Show)

instance Aeson.ToJSON Package where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON ArchDiff where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions


packageInfoToPackage :: Net.PackageInfo -> Package
packageInfoToPackage p = Package { name=Net.name p, epoch=Net.epoch p, version=Net.version p,release= Net.release p }

branchInfoToMap :: Net.BranchInfo -> Map.Map PackageArch [Package]
branchInfoToMap bi = Map.fromListWith (++) $ map (\p -> (Net.arch p, [packageInfoToPackage p])) $ Net.packages bi

tupleToPackage :: (PackageName, PackageEVR) -> Package
tupleToPackage (n, (e, v, r)) = Package { name=n, epoch=e, version=v, release=r }

packageToTuple :: Package -> (PackageName, PackageEVR)
packageToTuple p = (name p, (epoch p, version p, release p))

compareArches :: [Package] -> [Package] -> ArchDiff
compareArches l1 l2 = ArchDiff { extraPackages=extra, missingPackages=missing, newerPackages=newer }
    where   m1 = Map.fromList $ packageToTuple <$> l1
            m2 = Map.fromList $ packageToTuple <$> l2
            extra   = map tupleToPackage $ Map.toList $ Map.difference m1 m2
            missing = map tupleToPackage $ Map.toList $ Map.difference m2 m1
            inter   = Map.intersectionWith (,) m1 m2 
            newer   = map tupleToPackage $ Map.toList $ Map.map snd $ Map.filter ((==GT) . uncurry EVR.compareEVR) inter


newArchToArchDiff :: [Package] -> ArchDiff
newArchToArchDiff m = ArchDiff { extraPackages=m, missingPackages=[], newerPackages=[]}

-- For shared library
foreign export ccall compareBranches :: CString -> CString -> IO CString

compareBranches :: CString -> CString -> IO CString
compareBranches fstBranchC sndBranchC = do
    fstBranch <- peekCString fstBranchC
    sndBranch <- peekCString sndBranchC
    bInfoFst <- Net.getBranchInfo fstBranch
    bInfoSnd <- Net.getBranchInfo sndBranch
    let m1 = branchInfoToMap <$> bInfoFst
    let m2 = branchInfoToMap <$> bInfoSnd
    let extraArch = Map.Merge.mapMissing $ const newArchToArchDiff
    let missingArch = Map.Merge.mapMissing $ const newArchToArchDiff
    let sameArch = Map.Merge.zipWithMatched $ const compareArches
    let diff = A.liftA2 (Map.Merge.merge extraArch missingArch sameArch) m1 m2
    case diff of
        Left err    -> newCString err
        Right ans   -> newCString $ L.unpack $ Aeson.encode ans