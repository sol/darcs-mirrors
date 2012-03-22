module Config (Repository(..), repositories, organization) where

data Repository = Repository {
  repositoryName     :: String
, repositoryDarcsUrl :: String
} deriving (Eq, Show)

repositories :: [Repository]
repositories = [
    Repository "QuickCheck"       "http://code.haskell.org/QuickCheck/devel"
  , Repository "HUnit"            "http://code.haskell.org/HUnit/"
  , Repository "Cabal"            "http://darcs.haskell.org/cabal/"
  , Repository "darcs"            "http://darcs.net/"
  ]

organization :: String
organization = "darcs-mirrors"
