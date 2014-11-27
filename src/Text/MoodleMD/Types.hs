module Text.MoodleMD.Types where

import Text.Pandoc.Definition

type NumType = Float
type Text = [Block]

-- |Properties that all Answers share: truth, feedback (0 >= fb <= 100)
data AnswerProp = AnswerProp Int [Block] deriving Show

-- |A set of answers that can be used to build a question.
data Answers 
    = Multichoice [(Text,AnswerProp)]
    | ShortAnswer [(Text,AnswerProp)]
    | TrueFalse   [(Text,AnswerProp)]
    | Numerical   [((NumType,NumType),AnswerProp)] -- ^Numerical answer with (value,tolerance) tuples as answer options.
    deriving Show

-- |Return the answer type as a String.
answerType :: Answers -> String
answerType (Multichoice _) = "multichoice"
answerType (ShortAnswer _) = "shortanswer"
answerType (TrueFalse _) = "truefalse"
answerType (Numerical _) = "numerical"

-- |Return the list of answers, independent of question type.
answerContent :: Answers -> Either [(Text,AnswerProp)] [((NumType,NumType),AnswerProp)]
answerContent (Multichoice x) = Left x
answerContent (ShortAnswer x) = Left x
answerContent (TrueFalse x) = Left x
answerContent (Numerical x) = Right x

-- |A question body consisting of a title, a Body, and an Answer option.
data Question = Question String Text Answers deriving Show

