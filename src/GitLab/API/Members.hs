{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Members
-- Description : Queries about and updates to members of projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Members where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | the access levels for project members. See <https://docs.gitlab.com/ee/user/permissions.html#project-members-permissions>
data AccessLevel
  = Guest
  | Reporter
  | Developer
  | Maintainer
  | Owner

instance Show AccessLevel where
  show Guest = "10"
  show Reporter = "20"
  show Developer = "30"
  show Maintainer = "40"
  show Owner = "50"

-- | the members of a project.
membersOfProject :: Project -> GitLab [Member]
membersOfProject p = do
  result <- membersOfProject' (project_id p)
  return (fromRight (error "membersOfProject error") result)

-- | the members of a project given its ID.
membersOfProject' :: Int -> GitLab (Either (Response BSL.ByteString) [Member])
membersOfProject' projectId =
  gitlabGetMany addr []
  where
    addr =
      "/projects/" <> T.pack (show projectId) <> "/members"

-- | adds a user to a project with the given access level. Returns
-- 'Right Member' for each successful action, otherwise it returns
-- 'Left Status'.
addMemberToProject ::
  -- | the project
  Project ->
  -- | level of access
  AccessLevel ->
  -- | the user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addMemberToProject project access usr =
  addMemberToProject' (project_id project) access (user_id usr)

-- | adds a user to a project with the given access level, given the
-- project's ID and the user's ID. Returns @Right Member@ for each
-- successful action, otherwise it returns @Left Status@.
addMemberToProject' ::
  -- | project ID
  Int ->
  -- | level of access
  AccessLevel ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addMemberToProject' projectId access userId =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("user_id", Just (T.encodeUtf8 (T.pack (show userId)))),
        ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/" <> T.pack (show projectId) <> "/members"

-- | adds a list of users to a project with the given access
-- level. Returns 'Right Member' for each successful action, otherwise
-- it returns 'Left Status'.
addMembersToProject ::
  -- | the project
  Project ->
  -- | level of access
  AccessLevel ->
  -- | users to add to the project
  [User] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addMembersToProject project access =
  mapM (addMemberToProject project access)

-- | adds a list of users to a project with the given access level,
-- given the project's ID and the user IDs. Returns @Right Member@ for
-- each successful action, otherwise it returns @Left Status@.
addMembersToProject' ::
  -- | project ID
  Int ->
  -- | level of acces
  AccessLevel ->
  -- | IDs of users to add to the project
  [Int] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addMembersToProject' projectId access =
  mapM (addMemberToProject' projectId access)
