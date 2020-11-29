{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use lambda-case" -}

import Control.Applicative (liftA2)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import qualified Kesha
import qualified Kesha.NAR
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck
  ( modifyMaxSuccess,
  )
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    choose,
    elements,
    oneof,
    property,
    resize,
    scale,
    sized,
    vectorOf,
  )
import Prelude

main :: IO ()
main = do
  exes <-
    liftA2
      (,)
      (Directory.findExecutable "nix-store")
      (Directory.findExecutable "nix-hash")
  case exes of
    (Nothing, Nothing) -> do
      putStrLn "Nix tooling not found on path - skipping tests"
    (Just _, Nothing) -> do
      putStrLn "`nix-store` not found on path - aborting"
      Exit.exitFailure
    (Nothing, Just _) -> do
      putStrLn "`nix-hash` not found on path - aborting"
      Exit.exitFailure
    (Just _, Just _) ->
      Temp.withSystemTempDirectory "kesha-test" (hspec . spec)

spec :: FilePath -> Spec
spec tempDir = do
  describe "NAR packing" $ do
    describe "matches the output of `nix-store --dump`" $ do
      modifyMaxSuccess (const 20) $
        it "matches for Regular files" $
          property $
            \regular ->
              inTempDirectory tempDir "Regular" $
                checkNAR =<< createFSO_Regular regular
      modifyMaxSuccess (const 20) $
        it "matches for SymLinks" $
          property $
            \symLink -> do
              inTempDirectory tempDir "SymLink" $
                checkNAR =<< createFSO_SymLink symLink
      modifyMaxSuccess (const 20) $
        it "matches for Directories" $
          property $
            \directory ->
              Temp.withTempDirectory tempDir "Directory" $ \path -> do
                createFSO_Directory path directory
                checkNAR path

  describe "Hashing" $ do
    describe "matches the output of `nix-hash --type md5" $ do
      hashTests 20 (Kesha.HashOptions Kesha.MD5 Kesha.Base16)

    describe "matches the output of `nix-hash --type md5 --base32" $ do
      hashTests 20 (Kesha.HashOptions Kesha.MD5 Kesha.Base32)

    describe "matches the output of `nix-hash --type sha1" $ do
      hashTests 20 (Kesha.HashOptions Kesha.SHA1 Kesha.Base16)

    describe "matches the output of `nix-hash --type sha1 --base32" $ do
      hashTests 20 (Kesha.HashOptions Kesha.SHA1 Kesha.Base32)

    describe "matches the output of `nix-hash --type sha256" $ do
      hashTests 20 (Kesha.HashOptions Kesha.SHA256 Kesha.Base16)

    describe "matches the output of `nix-hash --type sha256 --base32`" $ do
      hashTests 20 (Kesha.HashOptions Kesha.SHA256 Kesha.Base32)
  where
    hashTests n opts = do
      modifyMaxSuccess (const n) $
        it "matches for any FSO" $
          property $ \fso -> case fso of
            Regular regular ->
              inTempDirectory tempDir "Regular" $
                checkHash opts =<< createFSO_Regular regular
            SymLink symLink ->
              inTempDirectory tempDir "SymLink" $
                checkHash opts =<< createFSO_SymLink symLink
            Directory directory ->
              Temp.withTempDirectory tempDir "Directory" $ \path -> do
                createFSO_Directory path directory
                checkHash opts path

    checkNAR :: FilePath -> Expectation
    checkNAR path = do
      result <- liftA2 (,) (nixStoreDump path) (Kesha.NAR.localPack path)
      case result of
        (Right want, Right got) ->
          want `shouldBe` Kesha.NAR.dump got
        (Left exitCode, _) ->
          expectationFailure ("nix-store --dump failed: " <> show exitCode)
        (_, Left err) ->
          expectationFailure ("Kesha.NAR.localPack failed: " <> show err)

    checkHash :: Kesha.HashOptions -> FilePath -> Expectation
    checkHash opts path = do
      result <- liftA2 (,) (nixHash (optsToArgs opts) path) (Kesha.hashWith opts path)
      case result of
        (Right want, Right got) ->
          want `shouldBe` got
        (Left exitCode, _) ->
          expectationFailure ("nix-hash failed: " <> show exitCode)
        (_, Left err) ->
          expectationFailure ("Kesha.hash failed: " <> show err)

    optsToArgs :: Kesha.HashOptions -> [String]
    optsToArgs (Kesha.HashOptions algo repr) =
      ( case algo of
          Kesha.MD5 -> ["--type", "md5"]
          Kesha.SHA1 -> ["--type", "sha1"]
          Kesha.SHA256 -> ["--type", "sha256"]
      )
        <> ( case repr of
               Kesha.Base16 -> []
               Kesha.Base32 -> ["--base32"]
           )

data FSO
  = Regular FSO_Regular
  | SymLink FSO_SymLink
  | Directory FSO_Directory
  deriving (Show)

instance Arbitrary FSO where
  arbitrary =
    oneof
      [ Regular <$> arbitrary,
        SymLink <$> arbitrary,
        Directory <$> arbitrary
      ]

data FSO_Regular = FSO_Regular
  { _regularIsExecutable :: Bool,
    regularName :: PathSegment,
    _regularContents :: Contents
  }
  deriving (Show)

instance Arbitrary FSO_Regular where
  arbitrary = FSO_Regular <$> arbitrary <*> arbitrary <*> arbitrary

data FSO_SymLink = FSO_SymLink
  { _symLinkIsFile :: Bool,
    _symLinkTarget :: PathSegment,
    symLinkName :: PathSegment
  }
  deriving (Show)

instance Arbitrary FSO_SymLink where
  arbitrary = FSO_SymLink <$> arbitrary <*> arbitrary <*> arbitrary

newtype FSO_Directory = FSO_Directory {directoryMap :: Map.Map PathSegment FSO}
  deriving (Show)

instance Arbitrary FSO_Directory where
  arbitrary =
    scale (min 5) $
      sized $ \size -> do
        len <- choose (0, size)
        FSO_Directory . Map.fromList <$> vectorOf len (resize (pred size) arbitrary)

newtype PathSegment = PathSegment {unPathSegment :: String}
  deriving (Eq, Ord, Show)

instance Arbitrary PathSegment where
  arbitrary = do
    len <- choose (10, 20)
    PathSegment <$> vectorOf len (elements validChars)
    where
      validChars :: String
      validChars = ['A' .. 'Z'] <> ['a' .. 'z']

newtype Contents = Contents {unContents :: BS.ByteString}
  deriving (Show)

instance Arbitrary Contents where
  arbitrary = fmap (Contents . BS.pack) arbitrary

createFSO_Regular :: FSO_Regular -> IO FilePath
createFSO_Regular (FSO_Regular isExecutable (PathSegment path) contents) = do
  BS.writeFile path (unContents contents)
  when isExecutable $ do
    perm <- Directory.getPermissions path
    Directory.setPermissions path perm {Directory.executable = True}
  pure path

createFSO_SymLink :: FSO_SymLink -> IO FilePath
createFSO_SymLink (FSO_SymLink isFile (PathSegment target) (PathSegment name))
  | isFile = do
    BS.writeFile target mempty
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
    flattenNodes ::
      FilePath -> FSO_Directory -> [(FilePath, Either FSO_SymLink FSO_Regular)]
    flattenNodes dir =
      Map.foldMapWithKey
        ( \piece fso ->
            case fso of
              Regular regular -> [(dir, Right regular {regularName = piece})]
              SymLink symLink -> [(dir, Left symLink {symLinkName = piece})]
              Directory directory -> flattenNodes (dir </> unPathSegment piece) directory
        )
        . directoryMap

    writeNode :: FilePath -> Either FSO_SymLink FSO_Regular -> IO FilePath
    writeNode path (Left symLink) = do
      Directory.createDirectoryIfMissing True path
      Directory.withCurrentDirectory path (createFSO_SymLink symLink)
    writeNode path (Right regular) = do
      Directory.createDirectoryIfMissing True path
      Directory.withCurrentDirectory path (createFSO_Regular regular)

nixStoreDump :: FilePath -> IO (Either Int BS.ByteString)
nixStoreDump path = do
  (_, Just hout, _, processHandle) <-
    Process.createProcess
      (Process.proc "nix-store" ["--dump", path])
        { Process.std_out = Process.CreatePipe
        }
  exit <- Process.waitForProcess processHandle
  case exit of
    Exit.ExitFailure code -> pure (Left code)
    Exit.ExitSuccess -> Right <$> BS.hGetContents hout

nixHash :: [String] -> FilePath -> IO (Either Int BS.ByteString)
nixHash args path = do
  (_, Just hout, _, processHandle) <-
    Process.createProcess
      (Process.proc "nix-hash" (args ++ [path]))
        { Process.std_out = Process.CreatePipe
        }
  exit <- Process.waitForProcess processHandle
  case exit of
    Exit.ExitFailure code -> pure (Left code)
    Exit.ExitSuccess ->
      -- `BS.init` is to drop the trailing newline
      Right . BS.init <$> BS.hGetContents hout

inTempDirectory :: FilePath -> String -> IO a -> IO a
inTempDirectory parent template m =
  Temp.withTempDirectory parent template $ \tempDir ->
    Directory.withCurrentDirectory tempDir m
