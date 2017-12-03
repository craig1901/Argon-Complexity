module Main where

import Lib
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                   (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                              (createTransport, defaultTCPParameters)
import System.Environment                                 (getArgs)
import System.Exit
import System.FilePath
import System.Directory


getFiles :: FilePath -> IO [FilePath]
getFiles path = fmap (\f -> path ++ "/" ++ f) . filter (\p -> head p /= '.' && p /= "argon") <$> getDirectoryContents path

main :: IO ()
main = do
    args <- getArgs
    curr <- getCurrentDirectory
    files <- getFiles curr
    case args of
      ["manager", host, port] -> do
        putStrLn "Starting Node as Manager"
        backend <- initializeBackend host port rtable
        startMaster backend $ \workers -> do
          result <- manager files workers
          liftIO $ putStr result
          liftIO $ putStrLn "Terminating all slaves"
          terminateAllSlaves backend
      ["worker", host, port] -> do
        putStrLn "Starting Node as Worker"
        backend <- initializeBackend host port rtable
        startSlave backend
      _ -> putStrLn "Bad parameters"
