module Options
  (options
  , Options(..)
  , hrafnarVersion
  ) where

import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_hrafnar       (version)

data Options = Options { optVerbose :: Bool, optVersion :: Bool }

parser :: Parser Options
parser = Options
  <$> switch (long "verbose" <> help "Enable verbose mode: verbosity level \"debug\"")
  <*> switch (long "version")

options :: IO Options
options = execParser
  $ info (parser <**> helper)
  $ fullDesc <> progDesc "Node of Hrafnar-system" <> header hrafnarVersion

hrafnarVersion :: String
hrafnarVersion = "Hrafnar " <> showVersion version
