{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main (main) where

import           Control.Concurrent  (forkIO)
import           Control.Exception   (SomeException, try)
import qualified Data.List           as List
import           GHC.Generics        (Generic)
import qualified Options.Applicative as Opt
import qualified Z.Data.Builder      as B
import           Z.Data.CBytes       (CBytes)
import qualified Z.Data.CBytes       as CBytes
import qualified Z.Data.JSON         as JSON
import qualified Z.Data.Vector       as V
import qualified Z.Data.YAML         as YAML
import qualified Z.IO.Buffered       as Buffered
import           Z.IO.Environment    (getCWD)
import qualified Z.IO.Logger         as Log
import           Z.IO.Network        (PortNumber, TCPClientConfig (..),
                                      initTCPClient, ipv4)
import           Z.IO.Resource       (withResource)
import           Z.IO.StdStream      (stdin, stdout)

data Project = Project
  { root    :: CBytes
  , command :: CBytes
  , args    :: [CBytes]
  } deriving (Show, Generic, JSON.JSON)

data ClientConfig = ClientConfig
  { host         :: CBytes
  , port         :: PortNumber
  , logLevel     :: Log.Level
  , logPath      :: CBytes
  , projectsPath :: CBytes
  , _lsp         :: Bool
  } deriving (Show)

clientConfig :: Opt.Parser ClientConfig
clientConfig = ClientConfig
      <$> Opt.strOption
          ( Opt.long "host"
         <> Opt.metavar "HOST"
         <> Opt.showDefault
         <> Opt.value "127.0.0.1"
         <> Opt.help "IP address for connecting")
      <*> Opt.option Opt.auto
          ( Opt.long "port"
         <> Opt.metavar "INT"
         <> Opt.showDefault
         <> Opt.value 3001
         <> Opt.help "Port number for listening")
      <*> Opt.option Opt.auto
          ( Opt.long "loglevel"
         <> Opt.metavar "INT"
         <> Opt.showDefault
         <> Opt.value 20
         <> Opt.help "Logging levels, see: <https://docs.python.org/3/howto/logging.html#logging-levels>")
      <*> Opt.strOption
          ( Opt.long "logpath"
         <> Opt.metavar "FILE_PATH"
         <> Opt.showDefault
         <> Opt.value "/tmp/lsp-network-client.log"
         <> Opt.help "")
      <*> Opt.strOption
          ( Opt.long "projects"
         <> Opt.short 'p'
         <> Opt.metavar "FILE_PATH"
         <> Opt.help "Projects config file.")
      <*> Opt.flag False True
          ( Opt.long "lsp"
         <> Opt.help "This is a fake options that we just igonre it. We add this because some emacs lsp plugins force append --lsp to language client.")

main :: IO ()
main = startClient' =<< Opt.execParser opts
  where
    opts = Opt.info (clientConfig Opt.<**> Opt.helper)
      ( Opt.fullDesc
     <> Opt.progDesc "Run lsp-network-client"
     <> Opt.header "LanguageServerProtocol - network" )

    startClient' config = do
      r <- try $ startClient config
      case r of
        Right _ -> return ()
        Left ex -> Log.fatal $ B.bytes . V.packASCII $ show (ex :: SomeException)


startClient :: ClientConfig -> IO ()
startClient ClientConfig{..} = do
  projects <- YAML.readYAMLFile projectsPath
  let logConfig = Log.defaultLoggerConfig {Log.loggerLevel = logLevel}
  Log.setDefaultLogger =<< Log.newFileLogger logConfig logPath

  Log.info "--------------------- LSP-network Client ---------------------"

  -- FIXME: we use this current directory as the root of project
  cwd <- getCWD
  let m_project = findProjectByPath cwd projects
  case m_project of
    Nothing -> Log.fatal $ "No such project: " <> CBytes.toBuilder cwd
    Just project -> do
      Log.info $ "Connecting to " <> CBytes.toBuilder host <> ":" <> B.int port
      let tcpConfig = TCPClientConfig Nothing (ipv4 host port) True 30

      withResource (initTCPClient tcpConfig) $ \tcp -> do
        tcpi <- Buffered.newBufferedInput tcp
        tcpo <- Buffered.newBufferedOutput tcp
        stdi <- Buffered.newBufferedInput stdin
        stdo <- Buffered.newBufferedOutput stdout
        runLangClient (tcpi, tcpo) (stdi, stdo) project

runLangClient :: (Buffered.BufferedInput, Buffered.BufferedOutput)
              -> (Buffered.BufferedInput, Buffered.BufferedOutput)
              -> Project
              -> IO ()
runLangClient (tcpi, tcpo) (stdi, stdo) project = Log.withDefaultLogger $ do
  -- first request
  Buffered.writeBuffer tcpo (JSON.encode project) >> Buffered.flushBuffer tcpo

  -- client request in
  _ <- forkIO $ Log.withDefaultLogger $ do
    foreverWhen (Buffered.readBuffer stdi) (not . V.null) "LSP Error!" $
      \input -> do
        Log.debug $ "ClientIn: " <> B.bytes input
        Buffered.writeBuffer tcpo input >> Buffered.flushBuffer tcpo

  -- server response out
  foreverWhen (Buffered.readBuffer tcpi) (not . V.null) "Server closed!" $
    \output -> do
      Log.debug $ "ServerOut: " <> B.bytes output
      Buffered.writeBuffer stdo output >> Buffered.flushBuffer stdo

-------------------------------------------------------------------------------

foreverWhen :: IO a -> (a -> Bool) -> B.Builder () -> (a -> IO b) -> IO ()
foreverWhen res cond !msg f = Log.withDefaultLogger $ do
  r <- res
  if cond r
     then f r >> foreverWhen res cond msg f
     else Log.fatal msg

findProjectByPath :: CBytes -> [Project] -> Maybe Project
findProjectByPath path = List.find cond
  where
    cond project =
      CBytes.toBytes (root project) `V.isPrefixOf` CBytes.toBytes path
