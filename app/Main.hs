{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import qualified System.Environment as Environment
import qualified Paths_kesha 
import qualified Data.Version as Version

import Data.Foldable (for_)
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import qualified Kesha

main :: IO ()
main = getCmd >>= runCmd

usage :: String
usage = 
  "kesha: compute the cryptographic hash of filesystem paths \n\
  \ \n\
  \usage: kesha (--version | --help | paths...) \n\
  \ \n\
  \example: kesha ~/.bashrc ~/.cabal/logs/ \n\
  \"

data Cmd
  = VersionCmd
  | HelpCmd 
  | BadUsageCmd BadUsageReason
  | HashCmd [FilePath]

data BadUsageReason
  = UnknownFlags [String]
  | MissingPaths

runCmd :: Cmd -> IO ()
runCmd VersionCmd = putStrLn (Version.showVersion Paths_kesha.version)
runCmd HelpCmd = putStrLn usage
runCmd (BadUsageCmd (UnknownFlags unknowns)) = do
  putStrLn $ "unknown flags: " <> intercalate "," (fmap show unknowns)
  putStrLn usage
runCmd (BadUsageCmd MissingPaths) = do
  putStrLn "no paths specified\n"
  putStrLn usage
runCmd (HashCmd paths) =
  for_ paths $ \path -> do
    result <- Kesha.hash path
    case result of
      Left err -> hPutStrLn stderr err
      Right hash -> do
        BS.putStr hash 
        putStr ("\t" <> path <> "\n")

getCmd :: IO Cmd
getCmd = cmdFromArgs . fmap parseArg <$> Environment.getArgs
  where
  cmdFromArgs :: [Arg] -> Cmd
  cmdFromArgs args
    | unknowns@(_ : _) <- filterUnknownFlags args = BadUsageCmd (UnknownFlags unknowns)
    | HelpFlag `elem` args = HelpCmd 
    | VersionFlag `elem` args = VersionCmd
    | otherwise =
        case filterPathArgs args of
          [] -> BadUsageCmd MissingPaths
          paths -> HashCmd paths

  filterUnknownFlags :: [Arg] -> [String]
  filterUnknownFlags = 
    mapMaybe $ \case
      UnknownFlag unknown -> Just unknown
      _ -> Nothing

  filterPathArgs :: [Arg] -> [String]
  filterPathArgs = 
    mapMaybe $ \case
      PathArg path -> Just path
      _ -> Nothing

data Arg 
  = VersionFlag
  | HelpFlag
  | UnknownFlag String
  | PathArg String
  deriving (Eq)

parseArg :: String -> Arg
parseArg arg = 
  case arg of
    "--version" -> VersionFlag
    "-v"        -> VersionFlag
    "--help"    -> HelpFlag
    "-h"        -> HelpFlag
    ('-' : _)   -> UnknownFlag arg
    _           -> PathArg arg
