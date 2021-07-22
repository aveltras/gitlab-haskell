{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Notes
-- Description : Notes on issues, snippets, merge requests and epics.
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Notes where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

createMergeRequestNote ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  -- | the note
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
createMergeRequestNote project =
  createMergeRequestNote' (project_id project)

createMergeRequestNote' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  -- | the note
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
createMergeRequestNote' projectId mergeRequestIID comment =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("id", Just (T.encodeUtf8 (T.pack (show projectId)))),
        ("merge_request_iid", Just (T.encodeUtf8 (T.pack (show mergeRequestIID)))),
        ("body", Just (T.encodeUtf8 comment))
      ]
    addr =
      T.pack $
        "/projects/" <> show projectId <> "/merge_requests/"
          <> show mergeRequestIID
          <> "/notes"
