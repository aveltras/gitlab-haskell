{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Projects
-- Description : Queries about projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Projects where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.API.Commits
import GitLab.API.Issues
import GitLab.API.Members
import GitLab.API.Pipelines
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.URI
import UnliftIO.Async

-- | gets all projects.
allProjects :: GitLab [Project]
allProjects =
  fromRight (error "allProjects error")
    <$> gitlabGetMany "/projects" [("statistics", Just "true")]

-- | gets all forks of a project. Supports use of namespaces.
--
-- > projectForks "project1"
-- > projectForks "group1/project1"
projectForks ::
  -- | name or namespace of the project
  Text ->
  GitLab (Either (Response BSL.ByteString) [Project])
projectForks projectName = do
  let urlPath =
        "/projects/"
          <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 projectName))
          <> "/forks"
  gitlabGetMany urlPath []

-- | searches for a 'Project' with the given project ID, returns
-- 'Nothing' if a project with the given ID is not found.
searchProjectId ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
searchProjectId projectId = do
  let urlPath = T.pack ("/projects/" <> show projectId)
  gitlabGetOne urlPath [("statistics", Just "true")]

-- | gets all projects with the given project name.
--
-- > projectsWithName "project1"
projectsWithName ::
  -- | project name being searched for.
  Text ->
  GitLab [Project]
projectsWithName projectName = do
  results <- gitlabGetMany "/projects" [("search", Just (T.encodeUtf8 projectName))]
  case results of
    Left _ -> error "projectsWithName error"
    Right projects ->
      return $
        filter (\project -> projectName == project_path project) projects

-- | gets a project with the given name for the given username. E.g.
--
-- > projectsWithNameAndUser "user1" "project1"
--
-- looks for "user1/project1"
projectsWithNameAndUser :: Text -> Text -> GitLab (Either (Response BSL.ByteString) (Maybe Project))
projectsWithNameAndUser username projectName =
  gitlabGetOne
    ( "/projects/"
        <> T.decodeUtf8
          (urlEncode False (T.encodeUtf8 (username <> "/" <> projectName)))
    )
    [("statistics", Just "true")]

-- | returns 'True' if a project has multiple committers, according to
-- the email addresses of the commits.
multipleCommitters :: Project -> GitLab Bool
multipleCommitters project = do
  emailAddresses <- commitsEmailAddresses project
  return (length (nub emailAddresses) > 1)

-- | gets the email addresses in the author information in all commit
-- for a project.
commitsEmailAddresses :: Project -> GitLab [Text]
commitsEmailAddresses project = do
  result <- commitsEmailAddresses' (project_id project)
  return (fromRight (error "commitsEmailAddresses error") result)

-- | gets the email addresses in the author information in all commit
-- for a project defined by the project's ID.
commitsEmailAddresses' :: Int -> GitLab (Either (Response BSL.ByteString) [Text])
commitsEmailAddresses' projectId = do
  -- (commits :: [Commit]) <- projectCommits' projectId
  attempt <- projectCommits' projectId
  case attempt of
    Left resp -> return (Left resp)
    Right (commits :: [Commit]) ->
      return (Right (map author_email commits))

-- | gets all projects for a user given their username.
--
-- > userProjects "harry"
userProjects' :: Text -> GitLab (Maybe [Project])
userProjects' username = do
  userMaybe <- searchUser username
  case userMaybe of
    Nothing -> return Nothing
    Just usr -> do
      result <- gitlabGetMany (urlPath (user_id usr)) []
      case result of
        Left _ -> error "userProjects' error"
        Right projs -> return (Just projs)
  where
    urlPath usrId = "/users/" <> T.pack (show usrId) <> "/projects"

-- | gets all projects for a user.
--
-- > userProjects myUser
userProjects :: User -> GitLab (Maybe [Project])
userProjects theUser =
  userProjects' (user_username theUser)

-- | gets the 'GitLab.Types.Project' against which the given 'Issue'
-- was created.
projectOfIssue :: Issue -> GitLab Project
projectOfIssue issue = do
  result <- searchProjectId (issue_project_id issue)
  case fromRight (error "projectOfIssue error") result of
    Nothing -> error "projectOfIssue error"
    Just proj -> return proj

-- | finds all issues created by a user.
--
-- > issuesCreatedByUser "user1"
--
-- returns a (user,projects) tuple, where user is the 'User' found
-- for the given searched username, and a list of 'Project's that the
-- user has created issues for.
issuesCreatedByUser :: Text -> GitLab (Maybe (User, [Project]))
issuesCreatedByUser username = do
  user_maybe <- searchUser username
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> do
      usersIssues <- userIssues usr
      projects <- mapConcurrently projectOfIssue usersIssues
      return (Just (usr, projects))

-- | searches for all projects with the given name, and returns a list
-- of triples of: 1) the found project, 2) the list of issues for the
-- found projects, and 3) a list of users who've created issues.
issuesOnForks ::
  -- | name or namespace of the project
  Text ->
  GitLab [(Project, [Issue], [User])]
issuesOnForks projectName = do
  projects <- projectsWithName projectName
  mapM processProject projects
  where
    processProject ::
      Project ->
      GitLab (Project, [Issue], [User])
    processProject proj = do
      (openIssues :: [Issue]) <- projectIssues proj defaultIssueFilters
      let authors = map issue_author openIssues
      return (proj, openIssues, authors)

-- | returns a (namespace,members) tuple for the given 'Project',
-- where namespace is the namespace of the project
-- e.g. "user1/project1", and members is a list of (username,name)
-- tuples about all members of the project.
projectMemebersCount :: Project -> GitLab (Text, [(Text, Text)])
projectMemebersCount project = do
  friends <- count
  return (namespace_name (namespace project), friends)
  where
    count = do
      let addr =
            "/projects/" <> T.pack (show (project_id project)) <> "/members/all"
      (res :: [Member]) <- fromRight (error "projectMembersCount error") <$> gitlabGetMany addr []
      return (map (\x -> (member_username x, member_name x)) res)

-- | returns 'True' is the last commit for a project passes all
-- continuous integration tests.
projectCISuccess ::
  -- | the name or namespace of the project
  Project ->
  GitLab Bool
projectCISuccess project = do
  pipes <- pipelines project
  case pipes of
    [] -> return False
    (x : _) -> return (pipeline_status x == "success")

-- | searches for a username, and returns a user ID for that user, or
-- 'Nothing' if a user cannot be found.
namespacePathToUserId ::
  -- | name or namespace of project
  Text ->
  GitLab (Maybe Int)
namespacePathToUserId namespacePath = do
  user_maybe <- searchUser namespacePath
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> return (Just (user_id usr))

-- | gets all diffs in a project for a given commit SHA.
projectDiffs :: Project -> Text -> GitLab (Either (Response BSL.ByteString) [Diff])
projectDiffs proj =
  projectDiffs' (project_id proj)

-- | gets all diffs in a project for a given project ID, for a given
-- commit SHA.
projectDiffs' :: Int -> Text -> GitLab (Either (Response BSL.ByteString) [Diff])
projectDiffs' projId commitSha =
  gitlabGetMany
    ( "/projects/"
        <> T.pack (show projId)
        <> "/repository/commits/"
        <> commitSha
        <> "/diff/"
    )
    []

-- | add a group to a project.
addGroupToProject ::
  -- | group ID
  Int ->
  -- | project ID
  Int ->
  -- | level of access granted
  AccessLevel ->
  GitLab (Either (Response BSL.ByteString) (Maybe GroupShare))
addGroupToProject groupId projectId access =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("group_id", Just (T.encodeUtf8 (T.pack (show groupId)))),
        ("group_access", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/share"
