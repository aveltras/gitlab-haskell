{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Users
-- Description : Queries about registered users
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Users where

import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | all registered users.
allUsers :: GitLab [User]
allUsers = do
  let path = "/users"
  fromRight (error "allUsers error") <$> gitlabGetMany path []

-- | searches for a user given a user ID. Returns @Just User@ if the
-- user is found, otherwise @Nothing@.
userId ::
  -- | username to search for
  Int ->
  GitLab (Maybe User)
userId usrId = do
  let path =
        "/users/"
          <> T.pack (show usrId)
  res <- gitlabGetOne path []
  case res of
    Left _err -> return Nothing
    Right Nothing -> return Nothing
    Right (Just user) -> return (Just user)

-- | searches for a user given a username. Returns @Just User@ if the
-- user is found, otherwise @Nothing@.
searchUser ::
  -- | username to search for
  Text ->
  GitLab (Maybe User)
searchUser username = do
  let path = "/users"
      params = [("username", Just (T.encodeUtf8 username))]
  result <- gitlabGetMany path params
  case result of
    Left _err -> return Nothing
    Right [] -> return Nothing
    Right (x : _) -> return (Just x)

-- | searches for users given a list of usernames, returns them in
-- alphabetical order of their usernames.
orderedUsers ::
  -- | usernames to search for
  [Text] ->
  GitLab [User]
orderedUsers usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  return (orderUsersByName users)
  where
    orderUsersByName :: [User] -> [User]
    orderUsersByName =
      sortBy (\u1 u2 -> compare (user_name u1) (user_name u2))
