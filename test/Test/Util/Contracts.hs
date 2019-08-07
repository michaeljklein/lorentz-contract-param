-- | Utility functions to read sample contracts (for testing).

module Test.Util.Contracts
       ( getIllTypedContracts
       , getWellTypedContracts
       , getWellTypedMichelsonContracts
       , getWellTypedMorleyContracts
       , getContractsWithReferences
       ) where

import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath (addExtension, (</>))

getIllTypedContracts :: IO [FilePath]
getIllTypedContracts =
  concatMapM (flip getContractsWithExtension "contracts/ill-typed")
  [".tz", ".mtz"]

getWellTypedContracts :: IO [FilePath]
getWellTypedContracts = getWellTypedMichelsonContracts <> getWellTypedMorleyContracts

getWellTypedMichelsonContracts :: IO [FilePath]
getWellTypedMichelsonContracts = concatMapM (getContractsWithExtension ".tz") wellTypedContractDirs

getWellTypedMorleyContracts :: IO [FilePath]
getWellTypedMorleyContracts = concatMapM (getContractsWithExtension ".mtz") wellTypedContractDirs

getContractsWithExtension :: String -> FilePath -> IO [FilePath]
getContractsWithExtension ext dir = mapMaybe convertPath <$> listDirectory dir
  where
    convertPath :: FilePath -> Maybe FilePath
    convertPath fileName
      | (ext `isSuffixOf` fileName) =
        Just (dir </> fileName)
      | otherwise = Nothing

wellTypedContractDirs :: [FilePath]
wellTypedContractDirs = ["contracts", "contracts/tezos_examples"]

getContractsWithReferences :: String -> FilePath -> String -> IO [(FilePath, FilePath)]
getContractsWithReferences ext fp refExt =
  fmap attachPrettyPath <$> getContractsWithExtension ext fp
  where
    attachPrettyPath :: FilePath -> (FilePath, FilePath)
    attachPrettyPath src = (src, addExtension src  refExt)
