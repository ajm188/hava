module IOUtils
( getInput
) where

import System.Environment
import System.IO

argf :: IO [Handle]
argf = do
    args <- getArgs
    case args of
        [] -> return [stdin]
        _ -> mapM (\f -> openFile f ReadMode) args

getInput :: IO [String]
getInput = do
    args <- argf
    mapM hGetContents args
