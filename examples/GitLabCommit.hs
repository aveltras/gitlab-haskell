{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Traversable
import GitLab
import GitLab.API.Projects
import System.Environment

main :: IO ()
main = do
  runGitLab
    ( defaultGitLabServer
        { url = "https://gitlab.com",
          token = T.pack "insert your token here"
        }
    )
    ( do
        projectData <- projectsAndIssues
        liftIO (print projectData)
    )

projectsAndIssues :: GitLab [(Project, [Issue])]
projectsAndIssues = do
  Just user <- searchUser "robstewart57"
  Just projects <- userProjects user
  for
    projects
    ( \project -> do
        issues <- projectIssues project defaultIssueFilters
        return (project, issues)
    )
