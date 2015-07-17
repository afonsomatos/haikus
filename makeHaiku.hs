-- Generates haikus based on list of english words and
-- random number of syllabes between ranges

import System.Random
import System.IO
import Data.Char
import Data.List.Split

line :: [(String, Integer)] -> Integer -> IO String
line wordList syllablesLeft
    | syllablesLeft <= 0 = return ""
    | otherwise = do
            -- Get a new StdGen for generating a random number
            gen <- newStdGen

            -- Get all words that have <= syllables than what it's needed
            let candidates = filter ((<= syllablesLeft) . snd) wordList

                -- Generate a random index from the list
                (randomIndex, _) = randomR (0, length candidates - 1) gen :: (Int, StdGen)

                -- Select the word with the random index ^
                word = candidates !! randomIndex

            -- Get other words to fill the remaining syllables
            rest <- line candidates (syllablesLeft - snd word)

            return $ unwords [fst word, rest]

main = do
    -- Open words list
    list <- readFile "words/out.txt"

    let listToTuple [x, y] = (x, read y :: Integer)
        wordList           = map (listToTuple . splitOn " ") (lines list)
        capitalize (x:xs)  = toUpper x : map toLower xs

    -- Print them capitalized
    mapM_ ((>>= putStrLn . capitalize) . line wordList) [5, 7, 5]