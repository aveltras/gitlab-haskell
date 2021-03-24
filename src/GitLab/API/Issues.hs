{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Issues
-- Description : Queries about issues created against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Issues
  ( defaultIssueFilters,
    IssueFilters (..),
    DueDate (..),
    IssueSearchIn (..),
    IssueOrderBy (..),
    IssueScope (..),
    IssueSortBy (..),
    IssueState (..),
    projectIssues,
    projectIssues',
    issueStatisticsUser,
    issueStatisticsGroup,
    issueStatisticsGroup',
    issueStatisticsProject,
    issueStatisticsProject',
    userIssues,
    newIssue,
    newIssue',
    editIssue,
  )
where

import qualified Data.Aeson as J
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Data.Time.Clock
import Data.Time.Format.ISO8601
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | No issue filters, thereby returning all issues.
defaultIssueFilters :: IssueFilters
defaultIssueFilters =
  IssueFilters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data DueDate
  = NoDueDate
  | Overdue
  | Week
  | Month
  | NextMonthPreviousTwoWeeks

instance Show DueDate where
  show NoDueDate = "0"
  show Overdue = "overdue"
  show Week = "week"
  show Month = "month"
  show NextMonthPreviousTwoWeeks = "next_month_and_previous_two_weeks"

data IssueSearchIn
  = JustTitle
  | JustDescription
  | TitleAndDescription

instance Show IssueSearchIn where
  show JustTitle = "title"
  show JustDescription = "description"
  show TitleAndDescription = "title,description"

data IssueOrderBy
  = CreatedAt
  | UpdatedAt
  | Priority
  | DueDate
  | RelativePosition
  | LabelPriority
  | MilestoneDue
  | Popularity
  | Weight

instance Show IssueOrderBy where
  show CreatedAt = "created_at"
  show UpdatedAt = "updated_at"
  show Priority = "priority"
  show DueDate = "due_date"
  show RelativePosition = "relative_position"
  show LabelPriority = "label_priority"
  show MilestoneDue = "milestone_due"
  show Popularity = "popularity"
  show Weight = "weight"

data IssueScope
  = CreatedByMe
  | AssignedToMe
  | All

instance Show IssueScope where
  show CreatedByMe = "created_by_me"
  show AssignedToMe = "assigned_to_me"
  show All = "all"

data IssueSortBy
  = Ascending
  | Descending

instance Show IssueSortBy where
  show Ascending = "asc"
  show Descending = "desc"

data IssueState
  = IssueOpen
  | IssueClosed

instance Show IssueState where
  show IssueOpen = "opened"
  show IssueClosed = "closed"

projectIssues ::
  -- | the project
  Project ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues.html#list-issues
  IssueFilters ->
  -- the GitLab issues
  GitLab [Issue]
projectIssues p filters = do
  result <- projectIssues' (project_id p) filters
  return (fromRight (error "projectIssues error") result)

projectIssues' ::
  -- | the project ID
  Int ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues.html#list-issues
  IssueFilters ->
  -- | the GitLab issues
  GitLab (Either Status [Issue])
projectIssues' projectId filters =
  gitlabWithAttrs path attrs
  where
    path = "/projects/" <> T.pack (show projectId) <> "/issues"
    attrs =
      T.pack $
        "&scope=all"
          <> concat (showIssueFilters filters)

-- | Gets issues count statistics on all issues the authenticated user has access to.
issueStatisticsUser ::
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilters ->
  -- | the issue statistics
  GitLab IssueStatistics
issueStatisticsUser filters =
  gitlabWithAttrsOneUnsafe path attrs
  where
    path = "/issues_statistics"
    attrs =
      T.pack $
        "&scope=all"
          <> concat (showIssueFilters filters)

-- | Gets issues count statistics for a given group.
issueStatisticsGroup ::
  -- | the group
  Group ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilters ->
  -- | the issue statistics
  GitLab IssueStatistics
issueStatisticsGroup group filters = do
  result <- issueStatisticsGroup' (group_id group) filters
  case result of
    Left _s -> error "issueStatisticsGroup error"
    Right Nothing -> error "issueStatisticsGroup error"
    Right (Just stats) -> return stats

-- | Gets issues count statistics for a given group.
issueStatisticsGroup' ::
  -- | the group ID
  Int ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilters ->
  -- | the issue statistics
  GitLab (Either Status (Maybe IssueStatistics))
issueStatisticsGroup' groupId filters =
  gitlabWithAttrsOne path attrs
  where
    path = T.pack $ "/groups/" <> show groupId <> "/issues_statistics"
    attrs =
      T.pack $
        "&scope=all"
          <> concat (showIssueFilters filters)

-- | Gets issues count statistics for a given group.
issueStatisticsProject ::
  -- | the project
  Project ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilters ->
  -- | the issue statistics
  GitLab IssueStatistics
issueStatisticsProject proj filters = do
  result <- issueStatisticsGroup' (project_id proj) filters
  case result of
    Left _s -> error "issueStatisticsProject error"
    Right Nothing -> error "issueStatisticsProject error"
    Right (Just stats) -> return stats

-- | Gets issues count statistics for a given project.
issueStatisticsProject' ::
  -- | the project ID
  Int ->
  -- | filter the issues, see https://docs.gitlab.com/ee/api/issues_statistics.html#get-issues-statistics
  IssueFilters ->
  -- | the issue statistics
  GitLab (Either Status (Maybe IssueStatistics))
issueStatisticsProject' projId filters =
  gitlabWithAttrsOne path attrs
  where
    path = T.pack $ "/projects/" <> show projId <> "/issues_statistics"
    attrs =
      T.pack $
        "&scope=all"
          <> concat (showIssueFilters filters)

-- | gets all issues create by a user.
userIssues ::
  -- | the user
  User ->
  GitLab [Issue]
userIssues usr =
  gitlabWithAttrsUnsafe addr attrs
  where
    addr = "/issues"
    attrs =
      T.pack $
        "&author_id="
          <> show (user_id usr)
          <> "&scope=all"

-- | create a new issue.
newIssue ::
  -- | project
  Project ->
  -- | issue title
  Text ->
  -- | issue description
  Text ->
  GitLab (Either Status (Maybe Issue))
newIssue project =
  newIssue' (project_id project)

-- | create a new issue.
newIssue' ::
  -- | project ID
  Int ->
  -- | issue title
  Text ->
  -- | issue description
  Text ->
  GitLab (Either Status (Maybe Issue))
newIssue' projectId issueTitle issueDescription =
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      "title="
        <> issueTitle
        <> "&description="
        <> issueDescription
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues"

-- | edits an issue. see <https://docs.gitlab.com/ee/api/issues.html#edit-issue>
editIssue ::
  ProjectId ->
  IssueId ->
  EditIssueReq ->
  GitLab (Either Status Issue)
editIssue projId issueId editIssueReq = do
  let path =
        "/projects/" <> T.pack (show projId)
          <> "/issues/"
          <> T.pack (show issueId)
  gitlabPut
    path
    ( Data.Text.Lazy.toStrict
        ( Data.Text.Lazy.Encoding.decodeUtf8
            (J.encode editIssueReq)
        )
    )

-------------------------------
-- Internal functions and types

data IssueFilters = IssueFilters
  { issueFilter_assignee_id :: Maybe Int,
    issueFilter_assignee_username :: Maybe String,
    issueFilter_author_id :: Maybe Int,
    issueFilter_author_username :: Maybe String,
    issueFilter_confidential :: Maybe Bool,
    issueFilter_created_after :: Maybe UTCTime,
    issueFilter_created_before :: Maybe UTCTime,
    issueFilter_due_date :: Maybe DueDate,
    issueFilter_iids :: Maybe Int,
    issueFilter_in :: Maybe IssueSearchIn,
    issueFilter_iteration_id :: Maybe Int,
    issueFilter_iteration_title :: Maybe String,
    issueFilter_milestone :: Maybe String,
    issueFilter_labels :: Maybe String,
    issueFilter_my_reaction_emoji :: Maybe String,
    issueFilter_non_archived :: Maybe Bool,
    issueFilter_order_by :: Maybe IssueOrderBy,
    issueFilter_scope :: Maybe IssueScope,
    issueFilter_search :: Maybe String,
    issueFilter_sort :: Maybe IssueSortBy,
    issueFilter_state :: Maybe IssueState,
    issueFilter_updated_after :: Maybe UTCTime,
    issueFilter_updated_before :: Maybe UTCTime,
    issueFilter_with_labels_details :: Maybe Bool
  }

showIssueFilters :: IssueFilters -> [String]
showIssueFilters filters =
  catMaybes
    [ (\i -> Just ("&assignee_id=" <> show i)) =<< issueFilter_assignee_id filters,
      (\t -> Just ("&assignee_username=" <> t)) =<< issueFilter_assignee_username filters,
      (\i -> Just ("&author_id=" <> show i)) =<< issueFilter_author_id filters,
      (\i -> Just ("&author_username=" <> show i)) =<< issueFilter_author_username filters,
      (\b -> Just ("&confidential=" <> showBool b)) =<< issueFilter_confidential filters,
      (\t -> Just ("&created_after=" <> showTime t)) =<< issueFilter_created_after filters,
      (\t -> Just ("&created_before=" <> showTime t)) =<< issueFilter_created_before filters,
      (\due -> Just ("&due_date=" <> show due)) =<< issueFilter_due_date filters,
      (\iids -> Just ("&iids[]=" <> show iids)) =<< issueFilter_iids filters,
      (\issueIn -> Just ("&assignee_id=" <> show issueIn)) =<< issueFilter_in filters,
      (\i -> Just ("&iteration_id=" <> show i)) =<< issueFilter_iteration_id filters,
      (\s -> Just ("&iteration_title=" <> s)) =<< issueFilter_iteration_title filters,
      (\s -> Just ("&milestone=" <> s)) =<< issueFilter_milestone filters,
      (\s -> Just ("&labels=" <> s)) =<< issueFilter_labels filters,
      (\s -> Just ("&my_reaction_emoji=" <> s)) =<< issueFilter_my_reaction_emoji filters,
      (\b -> Just ("&non_archived=" <> showBool b)) =<< issueFilter_non_archived filters,
      (\x -> Just ("&order_by=" <> show x)) =<< issueFilter_order_by filters,
      (\x -> Just ("&scope=" <> show x)) =<< issueFilter_scope filters,
      (\s -> Just ("&search=" <> s)) =<< issueFilter_search filters,
      (\x -> Just ("&sort=" <> show x)) =<< issueFilter_sort filters,
      (\x -> Just ("&state=" <> show x)) =<< issueFilter_state filters,
      (\t -> Just ("&updated_after=" <> showTime t)) =<< issueFilter_updated_after filters,
      (\t -> Just ("&updated_before=" <> showTime t)) =<< issueFilter_updated_before filters,
      (\b -> Just ("&with_labels_details=" <> showBool b)) =<< issueFilter_with_labels_details filters
    ]
  where
    showBool :: Bool -> String
    showBool True = "true"
    showBool False = "false"
    showTime :: UTCTime -> String
    showTime = iso8601Show
