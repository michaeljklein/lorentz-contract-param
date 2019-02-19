module Main
  ( main
  ) where

import Data.Text.IO (getContents)
import Fmt (pretty)
import Options.Applicative
  (auto, command, eitherReader, execParser, help, info, long, metavar, option, progDesc, strOption,
  subparser, switch, value)
import qualified Options.Applicative as Opt
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

import Michelson.Typecheck (typecheckContract)
import Michelson.Types
import Morley.Macro (expandFlattenContract, expandProgramMacros, expandValue)
import qualified Morley.Parser as P
import Morley.Runtime (Account(..), TxData(..), originateContract, runContract)
import Morley.Types

data CmdLnArgs
  = Parse (Maybe FilePath) Bool
  | TypeCheck (Maybe FilePath) Bool
  | Run !RunOptions
  | Originate !OriginateOptions

data RunOptions = RunOptions
  { roContractFile :: !(Maybe FilePath)
  , roDBPath :: !FilePath
  , roStorageValue :: !(Value Op)
  , roTxData :: !TxData
  , roVerbose :: !Bool
  }

data OriginateOptions = OriginateOptions
  { ooContractFile :: !(Maybe FilePath)
  , ooDBPath :: !FilePath
  , ooStorageValue :: !(Value Op)
  , ooBalance :: !Mutez
  , ooVerbose :: !Bool
  }

argParser :: Opt.Parser CmdLnArgs
argParser = subparser $
  parseSubCmd <>
  typecheckSubCmd <>
  runSubCmd <>
  originateSubCmd
  where
    parseSubCmd = command "parse" $
      info (uncurry Parse <$> parseOptions) $
        progDesc "Parse passed contract"

    typecheckSubCmd = command "typecheck" $
      info (uncurry TypeCheck <$> typecheckOptions) $
        progDesc "Typecheck passed contract"

    runSubCmd = command "run" $
      info (Run <$> runOptions) $
        progDesc "Run passed contract on "

    originateSubCmd = command "run" $
      info (Originate <$> originateOptions) $
        progDesc "Originate passed contract. Add it to passed DB"

    verboseFlag :: Opt.Parser Bool
    verboseFlag = switch $
      long "verbose" <>
      help "Whether output should be verbose"

    typecheckOptions :: Opt.Parser (Maybe FilePath, Bool)
    typecheckOptions = (,)
      <$> contractFileOption
      <*> verboseFlag

    parseOptions :: Opt.Parser (Maybe FilePath, Bool)
    parseOptions = (,)
      <$> contractFileOption
      <*> switch (
        long "expand-macros" <>
        help "Whether expand macros after parsing or not")

    runOptions :: Opt.Parser RunOptions
    runOptions =
      RunOptions
        <$> contractFileOption
        <*> dbPathOption
        <*> valueOption "storage" "Initial storage of a running contract"
        <*> txData
        <*> verboseFlag

    originateOptions :: Opt.Parser OriginateOptions
    originateOptions =
      OriginateOptions
        <$> contractFileOption
        <*> dbPathOption
        <*> valueOption "storage" "Initial storage of an originating contract"
        <*> mutezOption "balance" "Initial balance of an originating contract"
        <*> verboseFlag

contractFileOption :: Opt.Parser (Maybe FilePath)
contractFileOption = optional $ strOption $
  long "contract" <>
  metavar "FILEPATH" <>
  help "Path to contract file"

dbPathOption :: Opt.Parser FilePath
dbPathOption = strOption $
  long "db" <>
  metavar "FILEPATH" <>
  value "db.json" <>
  help "Path to DB with data which is used instead of real blockchain data"

valueOption :: String -> String -> Opt.Parser (Value Op)
valueOption name hInfo = option (eitherReader parseValue) $
  long name <>
  help hInfo
  where
    parseValue :: String -> Either String (Value Op)
    parseValue s =
      either (Left . mappend "Failed to parse value: " . show)
             (Right . expandValue)
      $ parse (P.noEnv P.value) "" (toText s)

mutezOption :: String -> String -> Opt.Parser Mutez
mutezOption name hInfo = fmap Mutez $ option auto $
  long name <>
  metavar "INT" <>
  help hInfo

txData :: Opt.Parser TxData
txData =
  mkTxData
    <$> sender
    <*> valueOption "parameter" "Parameter of passed contract"
    <*> mutezOption "amount" "Amout sent by a transaction"
  where
    sender = strOption $
      long "sender" <>
      metavar "ADDRESS" <>
      help "Sender address"
    mkTxData :: String -> Value Op -> Mutez -> TxData
    mkTxData addr param amount =
      TxData
        { tdSenderAddress = Address (toText addr)
        , tdParameter = param
        , tdAmount = amount
        }


main :: IO ()
main = do
  cmdLnArgs <- execParser (info argParser progInfo)
  run cmdLnArgs
  where
    progInfo = progDesc "Haskell implementation of Michelson typechecker and interpreter"
    run :: CmdLnArgs -> IO ()
    run args = case args of
      Parse mFilename hasExpandMacros -> do
        program <- readAndParseProgram mFilename
        if hasExpandMacros
          then pPrint $ expandFlattenContract (getContract program)
          else pPrint program
      TypeCheck mFilename _hasVerboseFlag -> do
        void $ prepareContract mFilename
        putTextLn "Contract is well-typed"
      Run RunOptions {..} -> do
        michelsonContract <- prepareContract roContractFile
        -- TODO: [TM-18] Pass timestamp from CLI if it's provided.
        runContract Nothing roVerbose roDBPath roStorageValue michelsonContract roTxData
      Originate OriginateOptions {..} -> do
        michelsonContract <- prepareContract ooContractFile
        let acc = Account
              { accBalance = ooBalance
              , accStorage = ooStorageValue
              , accContract = michelsonContract
              }
        addr <- originateContract ooVerbose ooDBPath acc
        putTextLn $ "Originated contract " <> pretty addr

    readCode :: Maybe FilePath -> IO Text
    readCode = maybe getContents readFile

    readAndParseProgram :: Maybe FilePath -> IO Program
    readAndParseProgram mFilename = do
      code <- readCode mFilename
      let filename = fromMaybe "<stdin>" mFilename
      either (throwM . P.ParserException) pure $
        parse (P.noEnv P.program) filename code

    -- Read and parse the contract, expand and type check.
    prepareContract :: Maybe FilePath -> IO (Contract Op)
    prepareContract mFile = do
      contract <- getContract <$> readAndParseProgram mFile
      -- TEMPORARY SHIM: this throws away Program Env! BAD!
      let
        michelsonContract :: Contract Op
        michelsonContract = expandFlattenContract contract

      either throwM pure $ typecheckContract michelsonContract
      pure michelsonContract
