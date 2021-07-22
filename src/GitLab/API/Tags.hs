{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tags
-- Description : Queries about tags in repositories
-- Copyright   : (c) Jihyun Yu, 2021
-- License     : BSD3
-- Maintainer  : yjh0502@gmail.com
-- Stability   : stable
module GitLab.API.Tags where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns all commits with tags.
tags ::
  -- | project
  Project ->
  GitLab [Tag]
tags project = do
  result <- tags' (project_id project)
  return (fromRight [] result)

-- | returns all commits with tags from a project given its project ID.
tags' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Tag])
tags' projectId = do
  gitlabGetMany (commitsAddr projectId) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/" <> T.pack (show projId) <> "/repository" <> "/tags"
