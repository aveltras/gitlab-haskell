{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Groups
-- Description : Queries about and updates to groups
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Groups where

import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.API.Members
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.URI

-- | gets groups with the given group name or path.
--
-- > projectsWithNameOrPath "group1"
groupsWithNameOrPath ::
  -- | group name being searched for.
  Text ->
  GitLab (Either (Response BSL.ByteString) [Group])
groupsWithNameOrPath groupName = do
  result <- gitlabWithAttrs "/groups" ("&search=" <> groupName)
  case result of
    Left {} -> return result
    Right groups ->
      return
        ( Right
            $filter
            ( \group ->
                groupName == group_name group
                  || groupName == group_path group
            )
            groups
        )

-- | adds all registered users to a group.
addAllUsersToGroup ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addAllUsersToGroup groupName access = do
  allRegisteredUsers <- allUsers
  let allUserIds = map user_username allRegisteredUsers
  addUsersToGroup' groupName access allUserIds

-- | adds a user to a group.
addUserToGroup ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | the user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addUserToGroup groupName access usr =
  addUserToGroup' groupName access (user_id usr)

-- | adds a user with a given user ID to a group.
addUserToGroup' ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addUserToGroup' groupName access usrId = do
  attempt <- groupsWithNameOrPath groupName
  case attempt of
    Left resp -> return (Left resp)
    Right [] ->
      -- TODO crreate response
      error "foo"
    -- return (Left (mk (Response BSL.ByteString) 404 (T.encodeUtf8 (T.pack "cannot find group"))))
    -- return (Left (mk (Response (T.encodeUtf8 (T.pack "cannot find group"))))
    Right [grp] ->
      gitlabPost addr dataBody
      where
        dataBody :: Text
        dataBody =
          "user_id=" <> T.pack (show usrId) <> "&access_level="
            <> T.pack (show access)
        addr =
          "/groups/"
            <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
            <> "/members"
    Right (_ : _) ->
      -- return (Left (Response (T.encodeUtf8 (T.pack "too many groups found"))))
      -- TODO crreate response
      error "foo"

-- return (Left (mk (Response BSL.ByteString) 404 (T.encodeUtf8 (T.pack "too many groups found"))))

-- | adds a list of users to a group.
addUsersToGroup ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [User] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addUsersToGroup groupName access =
  mapM (addUserToGroup groupName access)

-- | adds a list of users to a group.
addUsersToGroup' ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [Text] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addUsersToGroup' groupName access usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  mapM (addUserToGroup' groupName access . user_id) users

groupProjects ::
  -- | group
  Group ->
  GitLab (Either (Response BSL.ByteString) [Project])
groupProjects group = do
  groupProjects' (group_id group)

groupProjects' ::
  -- | group ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Project])
groupProjects' groupID = do
  let urlPath =
        T.pack $
          "/groups/"
            <> show groupID
            <> "/projects"
  gitlab urlPath
