-- Generates haikus based on list of english words and
-- random number of syllabes between ranges

import System.Random
import System.IO
import Data.Char
import Data.List.Split

line :: Integer -> [(String, Integer)] -> IO String
line s xs = do
    if s == 0 then return ""
    else do
        gen <- getStdGen
        let p = filter ((<=s) . snd) xs
            (randomNum, _) = randomR (0, length p - 1) gen :: (Int, StdGen)
            w = p !! randomNum
        -- Make the line recursively
        rest <- line (s - snd w) p
        newStdGen
        return $ (fst w) ++ " " ++ rest

main = do
    -- Open words list
    handle <- openFile "words/out.txt" ReadMode
    list <- hGetContents handle
    let listToTuple [x, y] = (x, read y :: Integer)
        wordList = [ (fst s, snd s) | x <- (lines list), let s = (listToTuple . splitOn " ") x]
        format (x:xs) = toUpper x : map toLower xs
    output <- mapM (\x -> line x wordList) [5, 7, 5]
    let [a, b, c] = map format output
    -- Output haiku lines
    putStrLn a
    putStrLn b
    putStrLn c