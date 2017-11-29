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


getFiles :: [String]
getFiles = ["FILE 1 TEXT", "FILE 2 TEXT", "FILE 3 TEXT", "FILE 4 TEXT", "FILE 5 TEXT"]

main :: IO ()
main = do
    args <- getArgs
    let files = getFiles
    case args of
      ["manager", host, port] -> do
        putStrLn "Starting Node as Manager"
        backend <- initializeBackend host port rtable
        startMaster backend $ \workers -> do
          result <- manager files workers
          liftIO $ print result
      ["worker", host, port] -> do
        putStrLn "Starting Node as Worker"
        backend <- initializeBackend host port rtable
        startSlave backend
      _ -> putStrLn "Bad parameters"
