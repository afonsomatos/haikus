-- Format src.txt so that it gives us a list of words and correspondig syllabes
-- divided by 1 space

import System.IO
import System.Directory
import System.Environment

main = do
    -- Get contents from the source file
    contents <- readFile "src.txt"

    -- Get handle to a temporary file
    (outName, outHandle) <- openTempFile "." "temp"

    -- Filter out the dashes, spaces and hiphens
    let separators      = "¥- "

        countSyllables  = show . (+1) . length . filter (`elem` separators)
        formatLine line = filter (`notElem` separators) line ++ " " ++ countSyllables line

        output = map formatLine (lines contents)

    -- Save everything to the temporary file
    hPutStr outHandle (init $ unlines output)

    -- Close the temporary file
    hClose outHandle

    -- Move the temporary file to the output file
    renameFile outName "out.txt"