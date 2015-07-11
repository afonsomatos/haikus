-- Format src.txt so that it gives us a list of words and correspondig syllabes
-- divided by 1 space

import System.IO
import System.Directory
import System.Environment

main = do
    -- Open input and output files
    srcHandle <- openFile "src.txt" ReadMode
    (outName, outHandle) <- openTempFile "." "temp"
    -- Filter out the dashes, spaces and hiphens
    contents <- hGetContents srcHandle
    let separators = "¥- "
        getSyllabes line = filter (`notElem` separators) line ++ " " ++ show (1 + sum [ 1 | x <- line, x `elem` separators])
        output = map getSyllabes $ lines contents
    -- Save everything to the output file
    hPutStr outHandle $ (init . unlines) output
    hClose srcHandle
    hClose outHandle
    -- Rename the output file
    renameFile outName "out.txt"