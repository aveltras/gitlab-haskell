{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.BoardsTests (boardsTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

boardsTests :: [TestTree]
boardsTests =
  [ testCase
      "board-list-projects"
      ( gitlabParseTestMany
          listProjectHaskell
          "data/api/boards/list-project.json"
      ),
    testCase
      "board-new-board"
      ( gitlabParseTestOne
          newBoardHaskell
          "data/api/boards/create-board.json"
      ),
    testCase
      "board-update-board"
      ( gitlabParseTestOne
          updateBoardHaskell
          "data/api/boards/update-board.json"
      )
  ]

listProjectHaskell :: [IssueBoard]
listProjectHaskell =
  [ IssueBoard
      { board_id = 1,
        board_name = "board1",
        board_project = ProjectBoard {project_board_id = 5, project_board_name = "Diaspora Project Site", project_board_name_with_namespace = "Diaspora / Diaspora Project Site", project_board_path = "diaspora-project-site", project_board_path_with_namespace = "diaspora/diaspora-project-site", project_board_http_url_to_repo = "http://example.com/diaspora/diaspora-project-site.git", project_board_web_url = "http://example.com/diaspora/diaspora-project-site"},
        board_milestone = Just (Milestone {milestone_project_id = Nothing, milestone_group_id = Nothing, milestone_description = Nothing, milestone_state = Nothing, milestone_due_date = Nothing, milestone_iid = Nothing, milestone_created_at = Nothing, milestone_title = "10.0", milestone_id = 12, milestone_updated_at = Nothing, milestone_web_url = Nothing}),
        board_lists = [BoardIssue {board_issue_id = 1, board_issue_label = BoardIssueLabel {board_issue_label_id = Nothing, board_issue_label_name = "Testing", board_issue_label_color = "#F0AD4E", board_issue_label_description = Nothing}, board_issue_position = 1, board_issue_max_issue_count = 0, board_issue_max_issue_weight = 0, board_issue_limit_metric = Nothing}, BoardIssue {board_issue_id = 2, board_issue_label = BoardIssueLabel {board_issue_label_id = Nothing, board_issue_label_name = "Ready", board_issue_label_color = "#FF0000", board_issue_label_description = Nothing}, board_issue_position = 2, board_issue_max_issue_count = 0, board_issue_max_issue_weight = 0, board_issue_limit_metric = Nothing}, BoardIssue {board_issue_id = 3, board_issue_label = BoardIssueLabel {board_issue_label_id = Nothing, board_issue_label_name = "Production", board_issue_label_color = "#FF5F00", board_issue_label_description = Nothing}, board_issue_position = 3, board_issue_max_issue_count = 0, board_issue_max_issue_weight = 0, board_issue_limit_metric = Nothing}],
        board_group = Nothing,
        board_assignee = Nothing,
        board_labels = Nothing,
        board_weight = Nothing
      }
  ]

newBoardHaskell :: IssueBoard
newBoardHaskell =
  IssueBoard
    { board_id = 1,
      board_name = "newboard",
      board_project = ProjectBoard {project_board_id = 5, project_board_name = "Diaspora Project Site", project_board_name_with_namespace = "Diaspora / Diaspora Project Site", project_board_path = "diaspora-project-site", project_board_path_with_namespace = "diaspora/diaspora-project-site", project_board_http_url_to_repo = "http://example.com/diaspora/diaspora-project-site.git", project_board_web_url = "http://example.com/diaspora/diaspora-project-site"},
      board_milestone = Nothing,
      board_lists = [],
      board_group = Nothing,
      board_assignee = Nothing,
      board_labels = Nothing,
      board_weight = Nothing
    }

updateBoardHaskell :: IssueBoard
updateBoardHaskell =
  IssueBoard
    { board_id = 1,
      board_name = "new_name",
      board_project = ProjectBoard {project_board_id = 5, project_board_name = "Diaspora Project Site", project_board_name_with_namespace = "Diaspora / Diaspora Project Site", project_board_path = "diaspora-project-site", project_board_path_with_namespace = "diaspora/diaspora-project-site", project_board_http_url_to_repo = "http://example.com/diaspora/diaspora-project-site.git", project_board_web_url = "http://example.com/diaspora/diaspora-project-site"},
      board_milestone = Just (Milestone {milestone_project_id = Just 15, milestone_group_id = Nothing, milestone_description = Just "Milestone 1 desc", milestone_state = Just MSActive, milestone_due_date = Nothing, milestone_iid = Just 1, milestone_created_at = Just (read "2018-07-03 06:36:42.618 UTC"), milestone_title = "Milestone 1", milestone_id = 43, milestone_updated_at = Just (read "2018-07-03 06:36:42.618 UTC"), milestone_web_url = Just "http://example.com/root/board1/milestones/1"}),
      board_lists = [],
      board_group = Nothing,
      board_assignee = Nothing,
      board_labels = Nothing,
      board_weight = Nothing
    }
