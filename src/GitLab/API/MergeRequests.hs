{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MergeRequests
-- Description : Queries about merge requests against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.MergeRequests where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns the merge request for a project given its merge request
-- IID.
mergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
mergeRequest project =
  mergeRequest' (project_id project)

-- | returns the merge request for a project given its project ID and
-- merge request IID.
mergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
mergeRequest' projectId mergeRequestIID =
  gitlabGetOne addr []
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIID)

-- | returns the merge requests for a project.
mergeRequests ::
  -- | the project
  Project ->
  GitLab [MergeRequest]
mergeRequests p = do
  result <- mergeRequests' (project_id p)
  return (fromRight (error "mergeRequests error") result)

-- | returns the merge requests for a project given its project ID.
mergeRequests' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [MergeRequest])
mergeRequests' projectId =
  gitlabGetMany addr [("scope", Just "all")]
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests"

-- | Creates a merge request.
createMergeRequest ::
  -- | project
  Project ->
  -- | source branch
  Text ->
  -- | target branch
  Text ->
  -- | target project ID
  Int ->
  -- | merge request title
  Text ->
  -- | merge request description
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
createMergeRequest project =
  createMergeRequest' (project_id project)

-- | Creates a merge request.
createMergeRequest' ::
  -- | project ID
  Int ->
  -- | source branch
  Text ->
  -- | target branch
  Text ->
  -- | target project ID
  Int ->
  -- | merge request title
  Text ->
  -- | merge request description
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
createMergeRequest' projectId sourceBranch targetBranch targetProjectId mrTitle mrDescription =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("source_branch", Just (T.encodeUtf8 sourceBranch)),
        ("target_branch", Just (T.encodeUtf8 targetBranch)),
        ("target_project_id", Just (T.encodeUtf8 (T.pack (show targetProjectId)))),
        ("title", Just (T.encodeUtf8 mrTitle)),
        ("description", Just (T.encodeUtf8 mrDescription))
      ]
    addr = T.pack $ "/projects/" <> show projectId <> "/merge_requests"

-- | Accepts a merge request.
acceptMergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
acceptMergeRequest project =
  acceptMergeRequest' (project_id project)

-- | Accepts a merge request.
acceptMergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe MergeRequest))
acceptMergeRequest' projectId mergeRequestIid = gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("id", Just (T.encodeUtf8 (T.pack (show projectId)))),
        ("merge_request_iid", Just (T.encodeUtf8 (T.pack (show mergeRequestIid))))
      ]
    addr =
      T.pack $
        "/projects/" <> show projectId <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/merge"

-- | Deletes a merge request. Only for admins and project owners.
deleteMergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequest project =
  deleteMergeRequest' (project_id project)

-- | Deletes a merge request. Only for admins and project owners.
deleteMergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequest' projectId mergeRequestIid = gitlabDelete addr
  where
    addr =
      T.pack $
        "/projects/" <> show projectId <> "/merge_requests/"
          <> show mergeRequestIid
