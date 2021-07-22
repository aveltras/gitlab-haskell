{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | internal module to support modules in GitLab.API
module GitLab.WebRequests.GitLabWebCalls
  ( GitLabParam,
    gitlabGetOne,
    gitlabGetMany,
    gitlabPost,
    gitlabPut,
    gitlabDelete,
    gitlabUnsafe,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Text.Read

newtype GitLabException = GitLabException String
  deriving (Eq, Show)

instance Exception.Exception GitLabException

type GitLabParam = (ByteString, Maybe ByteString)

gitlabGetOne ::
  (FromJSON a) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabGetOne urlPath params =
  request
  where
    request =
      gitlabHTTPOne
        "GET"
        "application/x-www-form-urlencoded"
        urlPath
        params
        []

gitlabGetMany ::
  (FromJSON a) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) [a])
gitlabGetMany urlPath params =
  gitlabHTTPMany
    "GET"
    "application/x-www-form-urlencoded"
    urlPath
    params
    []

gitlabPost ::
  (FromJSON a) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabPost urlPath params = do
  request
  where
    request =
      gitlabHTTPOne
        "POST"
        "application/x-www-form-urlencoded"
        urlPath
        []
        params

gitlabPut ::
  FromJSON a =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabPut urlPath params = do
  request
  where
    request =
      gitlabHTTPOne
        "PUT"
        "application/x-www-form-urlencoded"
        urlPath
        []
        params

gitlabDelete ::
  -- | the URL to post to
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
gitlabDelete urlPath = do
  result <- request
  case result of
    Right (Just _) -> return (Right (Just ()))
    x -> return x
  where
    request =
      gitlabHTTPOne
        "DELETE"
        "application/x-www-form-urlencoded"
        urlPath
        []
        []

-- | Assumes that HTTP error code responses, e.g. 404, 409, won't be
-- returned as (Left response) value.
gitlabUnsafe :: GitLab (Either a (Maybe b)) -> GitLab b
gitlabUnsafe query = do
  result <- query
  case result of
    Left _err -> error "gitlabUnsafe error"
    Right Nothing -> error "gitlabUnsafe error"
    Right (Just x) -> return x

---------------------
-- internal functions

gitlabHTTP ::
  -- | HTTP method (PUT, POST, DELETE, GET)
  ByteString ->
  -- | Content type (content-type)
  ByteString ->
  -- | the URL
  Text ->
  -- | the URL parameters for GET calls
  [GitLabParam] ->
  -- | the content paramters for POST, PUT and DELETE calls
  [GitLabParam] ->
  GitLab (Response BSL.ByteString)
gitlabHTTP httpMethod contentType urlPath urlParams contentParams = do
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> urlPath <> T.decodeUtf8 (renderQuery True urlParams)
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = httpMethod,
            requestHeaders =
              [ ("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)),
                ("content-type", contentType)
              ],
            requestBody = RequestBodyBS (renderQuery False contentParams)
          }
  response <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  return response

gitlabHTTPOne ::
  FromJSON a =>
  -- | HTTP method (PUT, POST, DELETE, GET)
  ByteString ->
  -- | Content type (content-type)
  ByteString ->
  -- | the URL
  Text ->
  -- | the URL query data for GET calls
  [GitLabParam] ->
  -- | the content parameters for POST, PUT and DELETE calls
  [GitLabParam] ->
  GitLab
    (Either (Response BSL.ByteString) (Maybe a))
gitlabHTTPOne httpMethod contentType urlPath urlParams contentParams = do
  response <-
    gitlabHTTP
      httpMethod
      contentType
      urlPath
      urlParams
      contentParams
  if successStatus (responseStatus response)
    then return (Right (parseOne (responseBody response)))
    else return (Left response)

gitlabHTTPMany ::
  (FromJSON a) =>
  -- | HTTP method (PUT, POST, DELETE, GET)
  ByteString ->
  -- | Content type (content-type)
  ByteString ->
  -- | the URL
  Text ->
  -- | the URL query data for GET calls
  [GitLabParam] ->
  -- | the content parameters for POST, PUT and DELETE calls
  [GitLabParam] ->
  GitLab
    (Either (Response BSL.ByteString) [a])
gitlabHTTPMany httpMethod contentType urlPath urlParams contentParams = do
  go 1 []
  where
    go :: FromJSON a => Int -> [a] -> GitLab (Either (Response BSL.ByteString) [a])
    go pageNum accum = do
      response <-
        gitlabHTTP
          httpMethod
          contentType
          urlPath
          (urlParams <> [("per_page", Just "100"), ("page", Just (T.encodeUtf8 (T.pack (show pageNum))))])
          contentParams
      if successStatus (responseStatus response)
        then do
          case parseMany (responseBody response) of
            Nothing -> return (Right accum)
            Just moreResults -> do
              let numPages = totalPages response
                  accum' = accum <> moreResults
              if pageNum == numPages
                then return (Right accum')
                else go (pageNum + 1) accum'
        else return (Left response)

totalPages :: Response a -> Int
totalPages resp =
  let hdrs = responseHeaders resp
   in findPages hdrs
  where
    findPages [] = 1 -- error "cannot find X-Total-Pages in header"
    findPages (("X-Total-Pages", bs) : _) =
      case readMaybe (T.unpack (T.decodeUtf8 bs)) of
        Just s -> s
        Nothing -> error "cannot find X-Total-Pages in header"
    findPages (_ : xs) = findPages xs

successStatus :: Status -> Bool
successStatus (Status n _msg) =
  n >= 200 && n <= 226

tryGitLab ::
  -- | the current retry count
  Int ->
  -- | the GitLab request
  Request ->
  -- | maximum number of retries permitted
  Int ->
  -- | HTTP manager
  Manager ->
  -- | the exception to report if maximum retries met
  Maybe HttpException ->
  IO (Response BSL.ByteString)
tryGitLab i request maxRetries manager lastException
  | i == maxRetries = error (show lastException)
  | otherwise =
    httpLbs request manager
      `Exception.catch` \ex -> tryGitLab (i + 1) request maxRetries manager (Just ex)

parseOne :: FromJSON a => BSL.ByteString -> Maybe a
parseOne bs =
  case eitherDecode bs of
    Left _err -> Nothing
    Right x -> Just x

parseMany :: FromJSON a => BSL.ByteString -> Maybe [a]
parseMany bs =
  case eitherDecode bs of
    Left _err -> Nothing
    Right xs -> Just xs
