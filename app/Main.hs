module Main where

import Lib
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import ArgonWork
import Data.List.Split


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (\f -> head f /= '.' && f /= "argon") names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)


main :: IO ()
main = do
    args <- getArgs
    curr <- getCurrentDirectory
    case args of
      ["manager", host, port, repo] -> do
        putStrLn "Starting Node as Manager"
        liftIO $ gitClone repo
        let r = splitOn "/" repo
        let repoFolderName = last r
        let workFolder = curr ++ "/" ++ repoFolderName
        putStrLn workFolder
        files <- getRecursiveContents workFolder
        backend <- initializeBackend host port rtable
        startMaster backend $ \workers -> do
          result <- manager files workers
          liftIO $ putStr result
          liftIO $ putStrLn "Terminating all slaves"
          terminateAllSlaves backend
          liftIO $ removeRepo repoFolderName
      ["worker", host, port] -> do
        putStrLn "Starting Node as Worker"
        backend <- initializeBackend host port rtable
        startSlave backend
      _ -> putStrLn "Bad parameters"
