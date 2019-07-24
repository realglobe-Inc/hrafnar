module Main
  ( main
  ) where

import           Control.Monad (join)
import           Hrafnar
import           Options

main :: IO ()
main = do
  opts <- options
  if optVersion opts then putStrLn hrafnarVersion
    else join $ inject opts takeOff

inject :: Options -> (UseDI => a) -> IO a
inject _ io = do
  sysEnv <- getEnv
  lq <- newLogQueue
  injectResources $
    give lq $
    give sysEnv io
