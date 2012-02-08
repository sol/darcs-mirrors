{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Prelude hiding (error)
import           Data.Foldable (forM_, find)
import qualified Data.ByteString.Char8 as B
import           System.Cmd (rawSystem)
import           System.Exit

import qualified Github.Repos as Github
import           Logging
import           Config (Repository(..), repositories, organization)

-- | Create a new repository on GitHub
create :: Github.BasicAuth -> String -> IO ()
create auth name = do
  r <- Github.createOrganization auth organization (Github.repo name) {Github.createRepoHasIssues = Just False}
  case r of
    Left err -> $(error "creating repository {name} failed: {err}")
    Right repo -> let url = Github.repoUrl repo in
      $(logInfo "created repository {name} at {url}")

-- | Update repository and push to GitHub
update :: Repository -> IO ()
update (Repository name url) = do
  e <- rawSystem "./update-repo.sh" [name, url]
  case e of
    ExitSuccess -> return ()
    ExitFailure _ -> $(logError "updating {name} failed!")

main :: IO ()
main = do
  auth <- readAccountFile
  r <- Github.organizationRepos organization
  case r of
    Left err -> $(logError "fetching list of repositories failed: {err}")
    Right githubRepos -> do
      forM_ repositories $ \repo@(Repository name _) -> do
        case find ((== name) . Github.repoName) githubRepos of
          (Just _) -> return ()
          Nothing -> create auth name
        update repo

-- | Read username/password from file 'account'
readAccountFile :: IO Github.BasicAuth
readAccountFile = do
  input <- B.readFile "credentials/account"
  case B.words input of
    [user, password] -> return (user, password)
    _ -> $(error "invalid account file: {input}")
