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
import           Control.Monad           (join, void, when)
import           GHC.Generics            (Generic)
import qualified Z.Data.Builder          as Builder
import           Z.Data.CBytes           (CBytes)
import qualified Z.Data.CBytes           as ZC
import qualified Z.Data.Vector           as ZV
import qualified Z.Data.YAML             as YAML
import           Z.IO.BIO                (runBIO, sinkToIO, (>|>))
import qualified Z.IO.Environment        as Z
import qualified Z.IO.FileSystem         as Z
import qualified Z.IO.Logger             as Log
import qualified Z.IO.LowResTimer        as Z
import qualified Z.IO.Process            as Z

data Project = Project
  { src_path  :: CBytes
  , dest_path :: CBytes
  , ignores   :: [CBytes]
  } deriving (Show, Generic, YAML.JSON)

main :: IO ()
main = Log.withDefaultLogger $ do
  argv <- Z.getArgs
  if length argv /= 2
     then Log.fatal $ "No sush config file, run "
                   <> (ZC.toBuilder $ argv !! 0) <> " <your-config-file-path>."
     else do
       let configPath = argv !! 1
       config <- YAML.readYAMLFile configPath
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
  src <- join $ snd <$> Z.watchDirsRecursively [src_path]
  -- no matter how many FileChange events are popped, we will notify only once
  -- after (2/10)s.
  throttledNotify <- Z.throttleTrailing_ 2 (void $ tryPutMVar flagChange ())
  runBIO $ src >|> sinkToIO (const throttledNotify)

runRsync :: MVar () -> Project -> IO ()
runRsync flagChange Project{..} = do
  src_path' <- (<> "/") <$> Z.normalize src_path     -- add a trailing slash
  dest_path' <- (<> "/") <$> Z.normalize dest_path   -- add a trailing slash
  let args = ["-azH", "--delete", "--partial"]
          ++ (concat $ map (\i -> ["--exclude", i]) ignores)
          ++ [src_path', dest_path']

  takeMVar flagChange
  Log.debug $ "Run: rsync "
           <> foldr Builder.append "" (map ((<> " "). ZC.toBuilder) args)
  (_out, err, _code) <- Z.readProcess
    Z.defaultProcessOptions { Z.processFile = "rsync" , Z.processArgs = args }
    ""
  when (not $ ZV.null err) $ Log.fatal $ Builder.bytes err

foreverRun :: IO () -> IO ()
foreverRun f = do
  result <- tryAny f
  case result of
    Left e -> do Log.fatal $ Builder.stringUTF8 (show e)
                 foreverRun f
    Right _ -> foreverRun f
