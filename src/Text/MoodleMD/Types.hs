module Text.MoodleMD.Types where

import Text.Pandoc.Definition

type NumType = Float
type Text = [Block]

-- |Properties that all Answers share: truth, feedback (0 >= fb <= 100)
data AnswerProp = AnswerProp Int [Block] deriving Show

-- |A set of answers that can be used to build a question.
data Answers 
    = Multichoice [(Text,AnswerProp)]
-- not correctly implemented
--    | TrueFalse   [(Text,AnswerProp)]
    -- |Numerical answer with (value,tolerance) tuples as answer options.
    | Numerical   [((NumType,NumType),AnswerProp)]
    deriving Show

-- |Return the answer type as a String.
answerType :: Answers -> String
answerType (Multichoice _) = "multichoice"
-- answerType (TrueFalse _) = "truefalse"
answerType (Numerical _) = "numerical"

-- |Return the list of answers, independent of question type.
answerContent :: Answers -> Either [(Text,AnswerProp)] [((NumType,NumType),AnswerProp)]
answerContent (Multichoice x) = Left x
-- answerContent (TrueFalse x) = Left x
answerContent (Numerical x) = Right x

makeStringAnswers :: String -> [(Text,AnswerProp)] -> Either String Answers
makeStringAnswers "multichoice" as = Right $ Multichoice as
-- makeStringAnswers "truefalse"   as = Right $ TrueFalse as
makeStringAnswers t _ = Left $ "unknown question type: " ++ t

-- |A question body consisting of a title, a Body, and an Answer option.
data Question = Question String Text Answers deriving Show

