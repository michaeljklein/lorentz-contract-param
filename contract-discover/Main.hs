{-# LANGUAGE DeriveAnyClass #-}

-- | An executable for haskell preprocessor which finds
-- all contract declarations in Haskell modules within the directory where
-- preprocessor is called and exports found contracts in a map.
module Main
  ( main
  ) where

import qualified Unsafe

import Data.Default (Default(..))
import Fmt (Builder, build, fmt, indentF, listF', unlinesF, (+|), (+||), (|+), (||+))
import qualified System.Directory as Dir
import System.Environment (getArgs)
import System.FilePath.Posix (takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty, runParser)

import Lorentz.Discover
import Util.IO (hSetTranslit, readFileUtf8, writeFileUtf8)

main :: IO ()
main = do
  hSetTranslit stderr
  args <- getArgs
  case args of
    src : _ : dst : opts -> do
      Config{..} <- parseOpts def (map toText opts)
      contracts <- findContracts cDebug (takeDirectory src)

      verifyContractDecls contracts
      let output = fmt @Text $ unlinesF
            [ "module " +| cModuleName |+ " (contracts) where"
            , ""
            , importsSection contracts
            , ""
            , contractsMapSection contracts
            ]
      printDebug cDebug . fmt $
        "Generated module:\n" +| indentF 2 (build output) |+ ""

      writeFileUtf8 dst output
    _ -> do
      die "Usage: lorentz-discover src _ dst moduleName"

newtype IsDebug = IsDebug Bool

data Config = Config
  { cModuleName :: Text
  , cDebug :: IsDebug
  }

instance Default Config where
  def = Config
    { cModuleName = "Main"
    , cDebug = IsDebug False
    }

parseOpts :: Config -> [Text] -> IO Config
parseOpts cfg = \case
  [] -> pure cfg
  "--debug" : opts ->
    parseOpts cfg{ cDebug = IsDebug True } opts
  "--generated-module" : opts ->
    case opts of
      [] -> die "Usage: `--generated-module <module name>`"
      modName : os -> parseOpts cfg{ cModuleName = modName } os
  opt : _ ->
    die $ "Unknown option: " <> show opt

printDebug :: IsDebug -> Text -> IO ()
printDebug (IsDebug isDebug)
  | isDebug = hPutStrLn stderr . ("contract-discover: " <>)
  | otherwise = \_ -> pass

findContracts :: IsDebug -> FilePath -> IO [ExportedContractInfo]
findContracts isDebug path = do
  isDir <- Dir.doesDirectoryExist path
  if isDir
    then do
      dirs <- Dir.listDirectory path
      concatMapM (findContracts isDebug) (map (path </>) dirs)
    else do
      if isHaskellModule path
        then parseHaskellModule isDebug path
        else pure []

parseHaskellModule :: IsDebug -> FilePath -> IO [ExportedContractInfo]
parseHaskellModule isDebug path = do
  file <- readFileUtf8 path
  case runParser haskellExportsParser path file of
    -- Some autogenerated modules may have no export list, we ignore them
    Left err -> do
      printDebug isDebug . toText $ errorBundlePretty err
      return []
    Right res -> do
      printDebug isDebug $ "In module " +|| path ||+ " found contracts "
                         +| listF' (build . ecdName . eciContractDecl) res |+ ""
      return res

-- | Ensures that:
--
-- 1. No two contracts have the same name.
verifyContractDecls :: [ExportedContractInfo] -> IO ()
verifyContractDecls contracts = do
  let vars = map (ecdVar . eciContractDecl) contracts
  let dups = map head . mapMaybe (nonEmpty . Unsafe.init) . group $ sort vars
  case dups of
    [] -> pass
    dup : _ -> die $ "Found multiple contracts with the same name: " <> show dup

importsSection :: [ExportedContractInfo] -> Builder
importsSection =
  mconcat . map (<> "\n") .
  (extraImports ++) . map mkImport . ordNub . map eciModuleName
  where
  mkImport m = "import qualified " +| m |+ ""
  extraImports =
    [ "import Data.Text (Text)"
    , "import Data.String (fromString)"
    , "import qualified Data.Map as Map"

    , "import qualified Michelson.Untyped as U"
    , "import Lorentz.Discover"
    ]

contractsMapSection :: [ExportedContractInfo] -> Builder
contractsMapSection contracts =
  "contracts :: Map.Map Text U.Contract\n\
  \contracts = Map.fromList\n  " <> listF' contractPairF contracts
  where
    -- Forms @(contract name, contract variable)@ pair.
    contractPairF ExportedContractInfo{..} =
      let ExportedContractDecl{..} = eciContractDecl
          var = "toUntypedContract " +| eciModuleName |+ "." +| ecdVar |+ ""
      in "\n  (\"" +| ecdName |+ "\", " +| var +| ")"
