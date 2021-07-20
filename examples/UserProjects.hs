{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import GitLab
import GitLab.API.Projects
import System.Environment

main :: IO ()
main = do
  myProjects <-
    runGitLab
      ( defaultGitLabServer
          { url = "https://gitlab.com",
            token = T.pack "insert your token here"
          }
      )
      (searchUser "robstewart57" >>= userProjects . fromJust)
  print myProjects
