module ArgonWork where

import System.IO
import System.FilePath
import System.Process


runCommandTest :: IO String
runCommandTest = do
    (_, Just hout, _, _) <- createProcess(proc "ls" []){ std_out = CreatePipe }
    hGetContents hout
