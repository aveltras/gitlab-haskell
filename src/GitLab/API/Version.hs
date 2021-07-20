{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Version
-- Description : Queries about GitLab instance version
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Version where

import qualified Data.ByteString.Lazy as BSL
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Get the version of the GitLab server.
gitlabVersion :: GitLab (Either (Response BSL.ByteString) (Maybe Version))
gitlabVersion = do
  let urlPath = "/version"
  gitlabOne urlPath
