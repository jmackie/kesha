{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Prelude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import System.FilePath ((</>))

import qualified Kesha.NAR

import Test.Hspec
  ( Spec
  , Expectation
  , describe
  , expectationFailure
  , hspec
  , it
  , runIO
  , shouldBe
  )
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , choose
  , elements
  , oneof
  , property
  , resize
  , scale
  , sized
  , vectorOf
  )
import Test.Hspec.QuickCheck
  ( modifyMaxSuccess
  )

main :: IO ()
main = Temp.withSystemTempDirectory "kesha-test" (hspec . spec)

spec :: FilePath -> Spec
spec tempDir =
  describe "NAR packing" $ do
    describe "matches the output of `nix-store --dump`" $ do

      modifyMaxSuccess (const 20) $
        it "matches for Regular files" $
          property $ \regular ->
            inTempDirectory tempDir "Regular" $
              createFSO_Regular regular >>= checkNAR

      modifyMaxSuccess (const 20) $
        it "matches for SymLinks" $
          property $ \symLink -> do
            inTempDirectory tempDir "SymLink" $
              createFSO_SymLink symLink >>= checkNAR

      modifyMaxSuccess (const 20) $
        it "matches for Directories" $
          property $ \directory ->
            Temp.withTempDirectory tempDir "Directory" $ \path ->
              createFSO_Directory path directory >> checkNAR path

checkNAR :: FilePath -> Expectation
checkNAR path = do
  result <- Kesha.NAR.localPack path
  case result of
    Left err -> expectationFailure err
    Right nar -> do
      want <- nixStoreDump path
      let got = Kesha.NAR.dump nar
      want `shouldBe` got

data FSO
  = Regular FSO_Regular
  | SymLink FSO_SymLink
  | Directory FSO_Directory
  deriving (Show)

instance Arbitrary FSO where
  arbitrary = oneof
    [ Regular <$> arbitrary
    , SymLink <$> arbitrary
    , Directory <$> arbitrary
    ]

data FSO_Regular
  = FSO_Regular
    { regularIsExecutable :: Bool
    , regularName :: PathPiece
    , regularContents :: Contents
    }
  deriving (Show)

instance Arbitrary FSO_Regular where
  arbitrary = FSO_Regular <$> arbitrary <*> arbitrary <*> arbitrary

data FSO_SymLink
  = FSO_SymLink
      { symLinkIsFile :: Bool
      , symLinkTarget :: PathPiece
      , symLinkName :: PathPiece
      }
  deriving (Show)

instance Arbitrary FSO_SymLink where
  arbitrary = FSO_SymLink <$> arbitrary <*> arbitrary <*> arbitrary

newtype FSO_Directory
  = FSO_Directory { directoryMap :: Map.Map PathPiece FSO }
  deriving newtype (Show)

instance Arbitrary FSO_Directory where
  arbitrary =
    scale (min 5) $ sized $ \size -> do
      len <- choose (0, size)
      FSO_Directory . Map.fromList <$> vectorOf len (resize (pred size) arbitrary)

newtype PathPiece
  = PathPiece { unPathPiece :: String } -- FIXME: Text
  deriving newtype (Eq, Ord, Show)

instance Arbitrary PathPiece where
  arbitrary = do
    len <- choose (10, 20)
    PathPiece <$> vectorOf len (elements validChars)
    where
    validChars :: String
    validChars = ['A'..'Z'] <> ['a'..'z']

newtype Contents
  = Contents { unContents :: BSL.ByteString }
  deriving newtype (Show)

instance Arbitrary Contents where
  arbitrary = fmap (Contents . BSL.pack) arbitrary

createFSO_Regular :: FSO_Regular -> IO FilePath
createFSO_Regular (FSO_Regular isExecutable (PathPiece path) contents) = do
  BSL.writeFile path (unContents contents)
  when isExecutable $ do
    perm <- Directory.getPermissions path
    Directory.setPermissions path perm { Directory.executable = True }
  pure path

createFSO_SymLink :: FSO_SymLink -> IO FilePath
createFSO_SymLink (FSO_SymLink isFile (PathPiece target) (PathPiece name))
  | isFile = do
      BSL.writeFile target mempty
      Directory.createFileLink target name
      pure target
  | otherwise = do
      Directory.createDirectory target
      Directory.createDirectoryLink target name
      pure target

createFSO_Directory :: FilePath -> FSO_Directory -> IO ()
createFSO_Directory root =
  traverse_ (uncurry writeNode) . flattenNodes root
  where
  flattenNodes
    :: FilePath -> FSO_Directory -> [(FilePath, Either FSO_SymLink FSO_Regular)]
  flattenNodes dir =
    Map.foldMapWithKey
      (\piece fso ->
          case fso of
            Regular regular -> [(dir, Right regular { regularName = piece } )]
            SymLink symLink -> [(dir, Left symLink { symLinkName = piece } )]
            Directory directory -> flattenNodes (dir </> unPathPiece piece) directory)
    . directoryMap

  writeNode :: FilePath -> Either FSO_SymLink FSO_Regular -> IO FilePath
  writeNode path (Left symLink) = do
    Directory.createDirectoryIfMissing True path
    Directory.withCurrentDirectory path (createFSO_SymLink symLink)

  writeNode path (Right regular) = do
    Directory.createDirectoryIfMissing True path
    Directory.withCurrentDirectory path (createFSO_Regular regular)

nixStoreDump :: FilePath -> IO BSL.ByteString
nixStoreDump path = do
  (_, Just hout, _, processHandle) <-
    Process.createProcess (Process.proc "nix-store" ["--dump", path])
      { Process.std_out = Process.CreatePipe }

  exit <- Process.waitForProcess processHandle
  case exit of
    Exit.ExitFailure _code -> undefined
    Exit.ExitSuccess -> BSL.hGetContents hout

inTempDirectory :: FilePath -> String -> IO a -> IO a
inTempDirectory parent template m =
  Temp.withTempDirectory parent template $ \tempDir ->
    Directory.withCurrentDirectory tempDir m
