{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Common where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

gitlabParseTestOne :: (FromJSON a, Eq a, Show a) => a -> String -> Assertion
gitlabParseTestOne expectedHaskellValue filename = do
  raw <- BSL.readFile filename
  result <- parseOne raw
  expectedHaskellValue @=? result

gitlabParseTestMany :: (FromJSON a, Eq a, Show a) => [a] -> String -> Assertion
gitlabParseTestMany expectedHaskellValue filename = do
  raw <- BSL.readFile filename
  result <- parseMany raw
  expectedHaskellValue @=? result

parseOne :: FromJSON a => BSL.ByteString -> IO a
parseOne bs =
  case eitherDecode bs of
    Left err -> assertFailure err
    Right xs -> return xs

parseMany :: FromJSON a => BSL.ByteString -> IO [a]
parseMany bs =
  case eitherDecode bs of
    Left err -> assertFailure err
    Right xs -> return xs
