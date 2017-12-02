module ArgonWork where

import System.IO
import System.FilePath
import System.Process


runCommandTest :: IO String
runCommandTest = do
    (_, Just hout, _, _) <- createProcess(proc "ls" []){ std_out = CreatePipe }
    hGetContents hout

-- runArgonCommand :: IO String
-- runArgonCommand = do
--     (_, Just hout, _, ph) <- createProcess(proc "stack exec argon src" []){ std_out = CreatePipe }
--     hGetContents hout

sendCommand :: (String, String) -> IO String
sendCommand (cmd, args) = do
    (_, Just hout, _, _) <- createProcess(proc cmd $ words args){ std_out = CreatePipe }
    hGetContents hout

runArgon :: IO String
runArgon = sendCommand("stack", "exec -- argon --json app/")
