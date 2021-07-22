{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Branches
-- Description : Queries about repository branches
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Branches where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Get a list of repository branches from a project, sorted by name
-- alphabetically.
branches :: Project -> GitLab [Branch]
branches project = do
  result <- branches' (project_id project)
  return (fromRight (error "branches error") result)

-- | Get a list of repository branches from a project given its
-- project ID, sorted by name alphabetically.
branches' :: Int -> GitLab (Either (Response BSL.ByteString) [Branch])
branches' projectId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/branches"
