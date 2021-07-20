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
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      T.pack $
        "id="
          <> show projectId
          <> "&merge_request_iid="
          <> show mergeRequestIID
          <> "&body="
          <> T.unpack comment
    addr =
      T.pack $
        "/projects/" <> show projectId <> "/merge_requests/"
          <> show mergeRequestIID
          <> "/notes"
