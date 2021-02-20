{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main (main) where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, void)
import           GHC.Generics        (Generic)
import qualified Options.Applicative as Opt
import qualified Z.Data.Builder      as Builder
import           Z.Data.CBytes       (CBytes)
import qualified Z.Data.CBytes       as CBytes
import qualified Z.Data.JSON         as JSON
import qualified Z.Data.Vector       as V
import           Z.Foreign           (withPrimVectorSafe)
import qualified Z.IO.Buffered       as Z
import qualified Z.IO.Logger         as Log
import qualified Z.IO.Network        as Z
import qualified Z.IO.Process        as Proc
import qualified Z.IO.Resource       as Z

data Project = Project
  { root    :: CBytes
  , command :: CBytes
  , args    :: [CBytes]
  } deriving (Show, Generic, JSON.JSON)

data ServerConfig = ServerConfig
  { host     :: CBytes
  , port     :: Z.PortNumber
  , logLevel :: Log.Level
  }

serverConfig :: Opt.Parser ServerConfig
serverConfig = ServerConfig
      <$> Opt.strOption
          ( Opt.long "host"
         <> Opt.metavar "HOST"
         <> Opt.showDefault
         <> Opt.value "127.0.0.1"
         <> Opt.help "IP address for listening, e.g., 0.0.0.0")
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

main :: IO ()
main = startServer =<< Opt.execParser opts
  where
    opts = Opt.info (serverConfig Opt.<**> Opt.helper)
      ( Opt.fullDesc
     <> Opt.progDesc "Run lsp-network-server"
     <> Opt.header "LanguageServerProtocol - network" )

startServer :: ServerConfig -> IO ()
startServer ServerConfig{..} = Log.withDefaultLogger $ do
  let logConfig = Log.defaultLoggerConfig
        { Log.loggerFormatter   = Log.defaultColoredFmt
        , Log.loggerConfigLevel = logLevel
        }
  Log.setDefaultLogger =<< Log.newStdLogger logConfig

  Log.info "--------------------- LSP-network Server ---------------------"
  Log.info $ "Listening on " <> CBytes.toBuilder host <> ":" <> Builder.int port

  let config = Z.TCPServerConfig (Z.ipv4 host port) 256 True 30
  Z.startTCPServer config $ \io -> do
    i <- Z.newBufferedInput io
    o <- Z.newBufferedOutput io

    -- The first request is project info.
    project <- Z.readParseChunks JSON.decodeChunks i
    Log.debug $ "Receive project: " <> JSON.encodeJSON project
    runLangServer (i, o) project

runLangServer :: (Z.BufferedInput, Z.BufferedOutput) -> Project -> IO ()
runLangServer (i, o) Project{..} = do
  let procOptions = Proc.defaultProcessOptions
        { Proc.processFile = command
        , Proc.processArgs = args
        , Proc.processCWD  = root
        , Proc.processStdStreams = (Proc.ProcessCreate, Proc.ProcessCreate, Proc.ProcessCreate)
        }
  Z.withResource (Proc.initProcess procOptions) $ \case
    (Just stdin, Just stdout, Just stderr, pstate) -> do
      stderr' <- Z.newBufferedInput stderr
      stdout' <- Z.newBufferedInput stdout

      void . forkIO $
        forever $ Z.readAll' stderr' >>= Log.fatal. Builder.bytes

      -- client in
      void . forkIO $ Log.withDefaultLogger $ do
        foreverWhen (Z.readBuffer i) (not . V.null) "Client closed!" $
          \input -> do
            Log.debug $ "ClientIn: " <> Builder.bytes input
            withPrimVectorSafe input (Z.writeOutput stdin)
      -- server out
      foreverWhen (Z.readBuffer stdout') (not . V.null) "Run LSP command failed!" $
        \output -> do
          Log.debug $ "ServerOut: " <> Builder.bytes output
          Z.writeBuffer o output >> Z.flushBuffer o

      void $ Proc.waitProcessExit pstate
    _ -> error "Unexpected error!"

-------------------------------------------------------------------------------

foreverWhen :: IO a -> (a -> Bool) -> Builder.Builder () -> (a -> IO b) -> IO ()
foreverWhen res cond !msg f = do
  r <- res
  if cond r
     then f r >> foreverWhen res cond msg f
     else Log.fatal msg
