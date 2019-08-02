module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import qualified System.Environment as Environment
import qualified System.Exit as Exit

import qualified Kesha

main :: IO ()
main = do
  [path] <- Environment.getArgs
  result <- Kesha.hash path
  case result of
    Left err -> Exit.die err
    Right hash -> BS.putStr hash >> putStrLn ""
