{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Text.MoodleMD.Reader
import Test.Framework
import System.Directory
import Data.List (isSuffixOf)

exampleFiles :: IO [FilePath]
exampleFiles = do
        cont <- getDirectoryContents "test/example-input/"
        return . fmap ("test/example-input/" ++) . filter (isSuffixOf ".md") $ cont

test_parseAllExamples = do
        examples <- exampleFiles
        results <- sequence $ fmap readMoodleMDFile examples
        sequence $ fmap assertRight results

test_checkMultiQuestion = do
    result <- readMoodleMDFile "test/example-input/reuse-body.md"
    questions <- assertRight result
    assertEqual 4 $ length questions

main = htfMain htf_thisModulesTests