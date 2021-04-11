{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import API.BoardsTests
import Data.Maybe (fromJust)
import SystemHookTests
import Test.Tasty

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "gitlab-haskell"
        [ testGroup
            "gitlab system hook tests"
            systemHookTests,
          testGroup
            "api-boards"
            boardsTests
        ]
    )
