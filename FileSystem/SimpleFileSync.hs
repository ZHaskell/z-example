{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar,
                                          tryPutMVar)
import           Control.Exception.Safe  (tryAny)
import           Control.Monad           (unless, void)
import           GHC.Generics            (Generic)
import qualified Z.Data.Builder          as Builder
import qualified Z.Data.CBytes           as CBytes
import           Z.Data.JSON             (JSON)
import           Z.Data.Text             (Text)
import qualified Z.Data.Text             as Text
import qualified Z.Data.Vector           as V
import           Z.Data.YAML             (readYAMLFile)
import           Z.IO.Environment        (getArgs)
import           Z.IO.FileSystem         (normalize, watchDirs)
import qualified Z.IO.Logger             as Log
import           Z.IO.LowResTimer        (throttleTrailing_)
import qualified Z.IO.Process            as Proc

data Project = Project
  { src_path   :: Text
  , dest_path  :: Text
  , rsync_opts :: [Text]
  , ignores    :: [Text]
  } deriving (Show, Generic, JSON)

main :: IO ()
main = Log.withDefaultLogger $ do
  argv <- getArgs
  if length argv /= 2
     then Log.fatal $ "No sush config file, run "
                   <> CBytes.toBuilder (head argv) <> " <your-config-file-path>."
     else do
       let configPath = argv !! 1
       config <- readYAMLFile configPath
       projects <- mapM (\p -> (p, ) <$> newEmptyMVar) config
       if null projects
          then Log.fatal "Empty project!"
          else do
            Log.info "Starting watching threads..."
            mapM_ (\(p, f) -> forkIO $ foreverRun $ watchProject f p) projects
            Log.info "Starting synchronizing threads..."
            let (first:ps) = projects
            mapM_ (\(p, f) -> forkIO $ foreverRun $ runRsync f p) ps
            -- we pick our first project to run on main thread
            foreverRun $ runRsync (snd first) (fst first)

watchProject :: MVar () -> Project -> IO ()
watchProject flagChange Project{..} = do
  -- no matter how many FileChange events are popped, we will notify only once
  -- after (2/10)s.
  throttledNotify <- throttleTrailing_ 2 (void $ tryPutMVar flagChange ())
  watchDirs [CBytes.fromText src_path] True $ const throttledNotify

runRsync :: MVar () -> Project -> IO ()
runRsync flagChange Project{..} = do
  src_path' <- (<> "/") <$> normalize (CBytes.fromText src_path)     -- add a trailing slash
  dest_path' <- (<> "/") <$> normalize (CBytes.fromText dest_path)   -- add a trailing slash
  let args = ["-azH", "--delete", "--partial"]
          ++ concatMap (map CBytes.fromText . Text.words) rsync_opts
          ++ concatMap (\i -> ["--exclude", CBytes.fromText i]) ignores
          ++ [src_path', dest_path']
  takeMVar flagChange
  Log.debug $ "Run: rsync with args: " <> Text.toUTF8Builder args
  (_out, err, _code) <- Proc.readProcess
    Proc.defaultProcessOptions { Proc.processFile = "rsync"
                               , Proc.processArgs = args
                               }
    ""
  unless (V.null err) $ Log.fatal $ Builder.bytes err

foreverRun :: IO () -> IO ()
foreverRun f = do
  result <- tryAny f
  case result of
    Left e -> do Log.fatal $ Builder.stringUTF8 (show e)
                 foreverRun f
    Right _ -> foreverRun f
