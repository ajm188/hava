import System.Environment
import System.IO

import Lex hiding (main)
import Parse hiding (main)

main = do
    filehandles <- getFileHandles
    filecontents <- mapM hGetContents filehandles
    print $ parse $ tokenize $ concat filecontents
    mapM hClose filehandles

getFileHandles = do
    filepaths <- getArgs
    mapM (\f -> openFile f ReadMode) filepaths
