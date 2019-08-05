{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Implementation of the Nix ARchive format (NAR)
--
-- https://nixos.org/~eelco/pubs/phd-thesis.pdf
--
module Kesha.NAR
  ( NAR
  , localPack
  , dump
  ) where

import Prelude

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Directory as Directory

import Control.Monad (when)
import Data.Bifunctor (second)
import Data.Foldable (traverse_, for_)
import Data.Int (Int64)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import System.FilePath ((</>))

-- |
-- A packed NAR archive. 
--
newtype NAR = NAR FSO

data FSO
  = Regular IsExecutable Size BSL.ByteString
  | SymLink Utf8FilePath
  | Directory (Map.Map PathPiece FSO)

type IsExecutable = Bool

type Size = Int64

type Utf8FilePath = Text
type PathPiece = Text    -- shouldn't include '/' or 'NUL' !

data PathType
  = RegularType
  | SymLinkType
  | DirectoryType
  | AmbiguousType

-- | 
-- Create a NAR archive for the given path in a local context.
--
-- See figure 5.2 of https://nixos.org/~eelco/pubs/phd-thesis.pdf
--
localPack :: FilePath -> IO (Either String NAR)
localPack path = second NAR <$> localPackFSO path

localPackFSO :: FilePath -> IO (Either String FSO)
localPackFSO path =
  guessPathType path >>= \case
    Nothing ->
      pure $ Left (path <> ": path doesn't exist")

    Just AmbiguousType ->
      pure $ Left (path <> ": couldn't work out file type")

    Just RegularType -> do
      isExecutable <- Directory.executable <$> Directory.getPermissions path
      size <- fromIntegral <$> Directory.getFileSize path
      contents <- BSL.readFile path
      let fso = Regular isExecutable size contents
      pure $ Right fso

    Just SymLinkType -> do
      target <- Directory.getSymbolicLinkTarget path
      let fso = SymLink (Text.pack target)
      pure $ Right fso

    Just DirectoryType -> do
      fs <- Directory.listDirectory path
      entries <- for fs $ \path' ->
        (Text.pack path',) <$> localPackFSO (path </> path')

      pure $
        second (Directory  . Map.fromList)
        (traverse sequence entries)

-- |
-- Serialize a NAR archive.
--
dump :: NAR -> BSL.ByteString
dump = Binary.runPut . putNAR

putNAR :: NAR -> Binary.Put
putNAR (NAR fso) = str "nix-archive-1" <> parens (putFSO fso)
  where
  putFSO :: FSO -> Binary.Put
  putFSO = \case
    Regular isExecutable size contents ->  do
      strs ["type", "regular"]
      when isExecutable $ strs ["executable", ""]
      str "contents"
      int size
      pad size contents

    SymLink target -> do
      strs ["type", "symlink"]
      strs ["target", BSL.fromStrict (encodeUtf8 target)]

    Directory entries -> do
      strs ["type", "directory"]
      let sortedEntries = List.sortOn fst (Map.toList entries)

      for_ sortedEntries $ \(name, node) -> do
        str "entry"
        parens $ do
          str "name"
          str (BSL.fromStrict (encodeUtf8 name))
          str "node"
          parens (putFSO node)

  int :: Integral a => a -> Binary.Put
  int = Binary.putInt64le . fromIntegral

  parens :: Binary.Put -> Binary.Put
  parens m = str "(" >> m >> str ")"

  str :: BSL.ByteString -> Binary.Put
  str bs = let len = BSL.length bs in int len <> pad len bs

  strs :: [BSL.ByteString] -> Binary.Put
  strs = traverse_ str

  pad :: Int64 -> BSL.ByteString -> Binary.Put
  pad n bs = do
    Binary.putLazyByteString bs
    Binary.putLazyByteString (BSL.replicate (padLen n) 0)

  -- Distance to the next multiple of 8
  padLen :: Integral a => a -> a
  padLen n = (8 - n) `mod` 8

guessPathType :: FilePath -> IO (Maybe PathType)
guessPathType path = do
  pathExists <- Directory.doesPathExist path
  if not pathExists
     then pure Nothing
     else do
       clues <- (,,)
         -- returns True if the argument file exists and is not a directory,
         <$> Directory.doesFileExist path

         -- returns True if the argument file exists and is either a directory or
         -- a symbolic link to a directory
         <*> Directory.doesDirectoryExist path

         -- Check whether the path refers to a symbolic link
         <*> Directory.pathIsSymbolicLink path

       case clues of
         (True, False, True)  -> pure (Just SymLinkType)
         (True, False, False) -> pure (Just RegularType)
         (False, True, True)  -> pure (Just SymLinkType)
         (False, True, False) -> pure (Just DirectoryType)

         _ -> pure (Just AmbiguousType)
