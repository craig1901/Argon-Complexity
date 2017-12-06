module ArgonWork where

import System.IO
import Data.List.Split
import System.Process
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)

sendCommand :: (String, String) -> IO String
sendCommand (cmd, args) = do
    (_, Just hout, _, _) <- createProcess(proc cmd $ words args){ std_out = CreatePipe }
    hGetContents hout

runArgon :: String -> IO String
runArgon file = sendCommand("stack", "exec -- argon " ++ file)

getCommits :: String -> IO [String]
getCommits repo =  do
    commits <- sendCommand("git", "--git-dir " ++ repo ++ "/.git log --pretty=format:'%H' ")
    return $ map strip $ words commits

fetchCommit :: String -> String -> IO ()
fetchCommit commit repo = do
    readCreateProcess ((proc "git" ["reset", "--hard", commit]){ cwd = Just repo}) ""
    return ()

gitClone :: String -> IO ()
gitClone repo = callProcess "git" ["clone", repo]

removeRepo :: String -> IO ()
removeRepo repo = do
    callProcess "rm" ["-rf", repo]
    putStrLn $ repo ++ " removed now that the work is done.\n"

strip :: [a] -> [a]
strip [] = []
strip [x] = []
strip xs = tail $ init xs
