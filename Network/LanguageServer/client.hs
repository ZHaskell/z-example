{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Exception  (SomeException, try)
import qualified Data.List          as List
import           GHC.Generics       (Generic)
import qualified Z.Data.Builder     as Builder
import           Z.Data.CBytes      (CBytes)
import qualified Z.Data.CBytes      as CBytes
import qualified Z.Data.JSON        as JSON
import qualified Z.Data.Vector      as V
import qualified Z.Data.YAML        as YAML
import qualified Z.IO.Buffered      as Buffered
import           Z.IO.Environment   (getArgs, getCWD)
import qualified Z.IO.Logger        as Log
import           Z.IO.Network       (PortNumber, TCPClientConfig (..),
                                     initTCPClient, ipv4)
import           Z.IO.Resource      (withResource)
import           Z.IO.StdStream     (stdin, stdout)

data Project = Project
  { root    :: CBytes
  , command :: CBytes
  , args    :: [CBytes]
  } deriving (Show, Generic, JSON.JSON)

data ClientConfig = ClientConfig
  { host      :: CBytes
  , port      :: PortNumber
  , log_dest  :: CBytes
  , log_level :: Log.Level
  , projects  :: [Project]
  } deriving (Show, Generic, JSON.JSON)

main :: IO ()
main = do
  argv <- getArgs
  if length argv /= 2
     then Log.fatal $ "No sush config file, run "
                   <> (CBytes.toBuilder $ argv !! 0) <> " <your-config-file-path>."
     else do config@ClientConfig{..} <- YAML.readYAMLFile (argv !! 1)
             let logConfig = Log.defaultLoggerConfig {Log.loggerConfigLevel = log_level}
             Log.setDefaultLogger =<< Log.newFileLogger logConfig log_dest

             r <- try $ runClient config
             case r of
               Right _ -> return ()
               Left ex -> Log.fatal $ Builder.bytes . V.packASCII $ show (ex :: SomeException)

runClient :: ClientConfig -> IO ()
runClient ClientConfig{..} = Log.withDefaultLogger $ do
  Log.info "--------------------- LSP-network Client ---------------------"
  Log.info $ "Connecting to " <> CBytes.toBuilder host <> ":" <> Builder.int port
  let tcpConfig = TCPClientConfig Nothing (ipv4 host port) True 30
  withResource (initTCPClient tcpConfig) $ \io -> do
    i <- Buffered.newBufferedInput io
    o <- Buffered.newBufferedOutput io
    input <- Buffered.newBufferedInput stdin
    output <- Buffered.newBufferedOutput stdout

    -- FIXME: we use this current directory as the root of project
    cwd <- getCWD
    let m_project = findProjectByPath cwd projects
    case m_project of
      Nothing -> Log.fatal $ "No such project: " <> CBytes.toBuilder cwd
      Just project -> do
        -- first request
        Buffered.writeBuffer o (JSON.encode project) >> Buffered.flushBuffer o

        -- client request in
        _ <- forkIO $ Log.withDefaultLogger $ do
          foreverWhen (Buffered.readBuffer input) (not . V.null) "LSP Error!" $
            \input' -> do
              Log.debug $ "ClientIn: " <> Builder.bytes input'
              Buffered.writeBuffer o input' >> Buffered.flushBuffer o

        -- server response out
        foreverWhen (Buffered.readBuffer i) (not . V.null) "Server closed!" $
          \output' -> do
            Log.debug $ "ServerOut: " <> Builder.bytes output'
            Buffered.writeBuffer output output' >> Buffered.flushBuffer output

-------------------------------------------------------------------------------

foreverWhen :: IO a -> (a -> Bool) -> Builder.Builder () -> (a -> IO b) -> IO ()
foreverWhen res cond !msg f = Log.withDefaultLogger $ do
  r <- res
  if (cond r)
     then f r >> foreverWhen res cond msg f
     else Log.fatal msg

findProjectByPath :: CBytes -> [Project] -> Maybe Project
findProjectByPath path projects = List.find cond projects
  where
    cond project =
      (CBytes.toBytes $ root project) `V.isPrefixOf` (CBytes.toBytes path)
