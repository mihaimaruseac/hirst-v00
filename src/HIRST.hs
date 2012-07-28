{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module HIRST where

import System.Console.CmdArgs

import Users

data CommandLineArgs
  = CollectUsers -- ^ collect users, allowing grouping after nicks
    { path :: FilePath
      -- ^ the optional logs location, defaults to defaultPath
    }
  deriving (Data, Typeable, Show, Eq)

-- | Build the CollectUsers mode
collectUsersMode :: CommandLineArgs
collectUsersMode = CollectUsers
  { path = defaultPath &= help "Path to log files"
  }
  &= auto
  &= details
    [ "Examples:"
    , "\t" ++ progName ++ " collectUsers logs/"
    , "\t" ++ progName ++ " co logs/"
    ]

-- Globals
defaultPath = "."
progName = "hirst"
progInfo = "HIRST :: Haskell IRC STatistics, version " ++ progVersion
progVersion = "0.1.0.0"
progCopyright = "© ROSEdu <http://rosedu.org>"
progAbout = "A simple program to generate IRC statistics"

-- | Build the arguments for the command line
arguments :: Mode (CmdArgs CommandLineArgs)
arguments = cmdArgsMode $ modes [collectUsersMode]
  &= versionArg [explicit, name "version", name "v", summary progInfo]
  &= summary (progInfo ++ ", " ++ progCopyright)
  &= help progAbout
  &= helpArg [explicit, name "help", name "h"]
  &= program progName

mainH = do
  args <- cmdArgsRun $ arguments
  optionHandler args

optionHandler args@CollectUsers{..} = collectUsers path
