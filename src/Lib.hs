module Lib where

import System.Environment (lookupEnv)
import GitHub
import Data.ByteString.Char8 as BS

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
  token <- lookupEnv "GITHUB_TOKEN"
  pure (GitHub.OAuth . BS.pack <$> token)
