{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Commits
-- Description : Queries about commits in repositories
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Commits where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | returns all commits for a project.
projectCommits ::
  -- | the project
  Project ->
  GitLab [Commit]
projectCommits project = do
  result <- projectCommits' (project_id project)
  -- return an empty list if the repository could not be found.
  return (fromRight [] result)

-- | returns all commits for a project given its project ID.
projectCommits' ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Commit])
projectCommits' projectId =
  gitlabGetMany (commitsAddr projectId) [("with_stats", Just "true")]
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/" <> T.pack (show projId) <> "/repository" <> "/commits"

-- | returns all commits of a branch from a project given the branch
-- name.
branchCommits ::
  -- | project
  Project ->
  -- | branch name
  Text ->
  GitLab [Commit]
branchCommits project branchName = do
  result <- branchCommits' (project_id project) branchName
  return (fromRight [] result)

-- | returns all commits of a branch from a project
-- given its project ID and the branch name.
branchCommits' ::
  -- | project ID
  Int ->
  -- | branch name
  Text ->
  GitLab (Either (Response BSL.ByteString) [Commit])
branchCommits' projectId branchName = do
  gitlabGetMany (commitsAddr projectId) [("ref_name", Just (T.encodeUtf8 branchName))]
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/" <> T.pack (show projId) <> "/repository" <> "/commits"

-- | returns a commit for the given project and commit hash, if such
-- a commit exists.
commitDetails ::
  -- | the project
  Project ->
  -- | the commit hash
  Text ->
  GitLab (Maybe Commit)
commitDetails project theHash = do
  result <- commitDetails' (project_id project) theHash
  return (fromRight Nothing result)

-- | returns a commit for the given project ID and commit hash, if
-- such a commit exists.
commitDetails' ::
  -- | project ID
  Int ->
  -- | the commit hash
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Commit))
commitDetails' projectId hash =
  gitlabGetOne (commitsAddr projectId) []
  where
    commitsAddr :: Int -> Text
    commitsAddr projId =
      "/projects/"
        <> T.pack (show projId)
        <> "/repository"
        <> "/commits"
        <> "/"
        <> hash
