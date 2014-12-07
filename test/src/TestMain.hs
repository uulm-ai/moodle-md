{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Text.MoodleMD.Reader
import Test.Framework
import System.Directory
import Text.MoodleMD.Types
import Data.List (isSuffixOf)
import Control.Applicative ((<$>))

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
    assertEqual 3 $ length questions

test_parseQuestionFeedback = do
    result <- assertRightVerbose "failed parsing" <$> readMoodleMDFile "test/example-input/feedback-question.md"
    let apHasFeedback (AnswerProp _ fb) = not $ null fb
    let getAns (Question _ _ ans) = ans
    hasFeedback <- any apHasFeedback. either (snd <$>) (snd <$>) . answerContent . getAns . head <$> result
    assertBoolVerbose "answer option has no feedback" hasFeedback

main = htfMain htf_thisModulesTests