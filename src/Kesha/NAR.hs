{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Kesha.NAR
-- Copyright: (c) 2020 Jordan Mackie
-- License: MIT
-- Maintainer: Jordan Mackie <contact@jmackie.dev>
-- Stability: experimental
-- Portability: portable
--
-- An implementation of the <https://nixos.org/~eelco/pubs/phd-thesis.pdf Nix ARchive format> (NAR).
module Kesha.NAR
  ( NAR,
    PackError (..),
    localPack,
    dump,
  )
where

{- HLINT ignore "Use lambda-case" -}

import Control.Monad (when)
import Data.Bifunctor (second)
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_, traverse_)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import qualified System.Directory as Directory
import System.FilePath ((</>))
import Prelude

-- |
-- A packed NAR archive.
newtype NAR = NAR {getFSO :: FSO}

-- |
-- Errors that can be raised when attempting to pack a path into a NAR archive.
data PackError
  = -- |
    -- Attempted to pack a path that doesn't exist.
    FileDoesNotExist FilePath
  | -- |
    -- Heuristic for detecting the /type/ of path failed. Where /type/ is one:
    -- a regular file, a directory, or a symbolic link.
    AmbiguousFileType FilePath
  deriving (Show, Eq)

data FSO
  = Regular !IsExecutable !Size !BS.ByteString
  | SymLink !UTF8FilePath
  | Directory !(Map.Map PathSegment FSO)

type IsExecutable = Bool

type Size = Int

type UTF8FilePath = Text

type PathSegment = Text -- shouldn't include '/' or 'NUL' !

data PathType
  = RegularType
  | SymLinkType
  | DirectoryType
  | AmbiguousType

-- |
-- Create a NAR archive for the given path in a local context.
--
-- See figure 5.2 of https://nixos.org/~eelco/pubs/phd-thesis.pdf
localPack :: FilePath -> IO (Either PackError NAR)
localPack path = second NAR <$> localPackFSO path

localPackFSO :: FilePath -> IO (Either PackError FSO)
localPackFSO path =
  guessPathType path >>= \guess -> case guess of
    Nothing ->
      pure $ Left (FileDoesNotExist path)
    Just AmbiguousType ->
      pure $ Left (AmbiguousFileType path)
    Just RegularType -> do
      isExecutable <- Directory.executable <$> Directory.getPermissions path
      size <- fromIntegral <$> Directory.getFileSize path
      contents <- BS.readFile path
      let fso = Regular isExecutable size contents
      pure $ Right fso
    Just SymLinkType -> do
      target <- Directory.getSymbolicLinkTarget path
      let fso = SymLink (Text.pack target)
      pure $ Right fso
    Just DirectoryType -> do
      fs <- Directory.listDirectory path
      entries <- for fs $ \path' -> do
        results <- localPackFSO (path </> path')
        pure (Text.pack path', results)
      pure $
        second
          (Directory . Map.fromList)
          (traverse sequence entries)

-- |
-- Serialize a NAR archive.
dump :: NAR -> BS.ByteString
dump = BSL.toStrict . Binary.runPut . putNAR

putNAR :: NAR -> Binary.Put
putNAR nar = str "nix-archive-1" <> parens (putFSO (getFSO nar))
  where
    putFSO :: FSO -> Binary.Put
    putFSO fso = case fso of
      Regular isExecutable size contents -> do
        strs ["type", "regular"]
        when isExecutable $ strs ["executable", ""]
        str "contents"
        int size
        pad size contents
      SymLink target -> do
        strs ["type", "symlink"]
        strs ["target", encodeUtf8 target]
      Directory entries -> do
        strs ["type", "directory"]
        let sortedEntries = List.sortOn fst (Map.toList entries)
        for_ sortedEntries $ \(name, node) -> do
          str "entry"
          parens $ do
            str "name"
            str (encodeUtf8 name)
            str "node"
            parens (putFSO node)

    int :: Integral a => a -> Binary.Put
    int = Binary.putInt64le . fromIntegral

    parens :: Binary.Put -> Binary.Put
    parens m = str "(" >> m >> str ")"

    str :: BS.ByteString -> Binary.Put
    str bs = let len = BS.length bs in int len <> pad len bs

    strs :: [BS.ByteString] -> Binary.Put
    strs = traverse_ str

    pad :: Int -> BS.ByteString -> Binary.Put
    pad n bs = do
      Binary.putByteString bs
      Binary.putByteString (BS.replicate (padLen n) 0)

    -- Distance to the next multiple of 8
    padLen :: Integral a => a -> a
    padLen n = (8 - n) `mod` 8

guessPathType :: FilePath -> IO (Maybe PathType)
guessPathType path = do
  pathExists <- Directory.doesPathExist path
  if not pathExists
    then pure Nothing
    else do
      clues <-
        (,,)
          -- returns True if the argument file exists and is not a directory,
          <$> Directory.doesFileExist path
          -- returns True if the argument file exists and is either a directory or
          -- a symbolic link to a directory
          <*> Directory.doesDirectoryExist path
          -- Check whether the path refers to a symbolic link
          <*> Directory.pathIsSymbolicLink path
      case clues of
        (True, False, True) -> pure (Just SymLinkType)
        (True, False, False) -> pure (Just RegularType)
        (False, True, True) -> pure (Just SymLinkType)
        (False, True, False) -> pure (Just DirectoryType)
        _ -> pure (Just AmbiguousType)
