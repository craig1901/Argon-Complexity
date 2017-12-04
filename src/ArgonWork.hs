module ArgonWork where

import System.IO
import System.Process


runCommandTest :: IO String
runCommandTest = do
    (_, Just hout, _, _) <- createProcess(proc "ls" []){ std_out = CreatePipe }
    hGetContents hout

sendCommand :: (String, String) -> IO String
sendCommand (cmd, args) = do
    (_, Just hout, _, _) <- createProcess(proc cmd $ words args){ std_out = CreatePipe }
    hGetContents hout

runArgon :: String -> IO String
runArgon file = sendCommand("stack", "exec -- argon " ++ file)

gitClone :: String -> IO ()
gitClone repo = callProcess "git" ["clone", repo]

removeRepo :: String -> IO ()
removeRepo repo = do
    callProcess "rm" ["-rf", repo]
    putStrLn $ repo ++ " removed."
