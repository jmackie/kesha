module Kesha
  ( hash
  ) where

import Prelude

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Lazy as BSL

import qualified Kesha.NAR as NAR

hash :: FilePath -> IO (Either String BS.ByteString)
hash path = fmap (sha256sum . NAR.dump) <$> NAR.localPack path

sha256sum :: BSL.ByteString -> BS.ByteString
sha256sum = Base32.encode . SHA256.hashlazy
