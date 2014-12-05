module Text.MoodleMD.Reader (pandoc2questions, parseMoodleMD, readMoodleMDFile) where

import Text.MoodleMD.Types
import Text.Pandoc
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)
import Control.Arrow
import Data.Tuple (swap)
import GHC.Float
import Data.Maybe (fromMaybe)
import Control.Applicative (pure,(<$>))

type BlockP a = Parsec [Block] () a

-- |Helper to increment Source position parsing arbitrary lists with Parsec
incPos pos _ _ = incSourceColumn pos 1

tokenParser = makeTokenParser javaStyle

-- |Convert to normal string.
textToString :: Text -> String
textToString = writePlain def . Pandoc nullMeta

-- |For converting the question title to a normal string.
inlinesToString :: [Inline] -> String
inlinesToString = textToString . pure . Plain

seqFirst :: Monad m => (m a,b) -> m (a,b)
seqFirst (ma,b) = do a <- ma; return (a,b)

seqSecond :: Monad m => (a,m b) -> m (a,b)
seqSecond (a,mb) = do b <- mb; return (a,b)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right r) = Right r

numericBlock :: BlockP (NumType,NumType)
numericBlock = undefined

-- |lifts a parse result back into the parser
reparse = either (unexpected . show) return

answerDefList :: String -> BlockP Answers
answerDefList qType = defList >>= (reparse . parseAnswers)
    where withFeedback :: BlockP a -> BlockP (a,Text) -- could be improved by checking that a doesn't consume block quotes
          withFeedback pa = do
            a <- pa
            fb <- fromMaybe [] <$> optionMaybe blockQuote
            return (a,fb)

          -- |parse one entry for a numerical question
          parseNumericAnswerOpt :: BlockP ((NumType,NumType),Text)
          parseNumericAnswerOpt = withFeedback numericBlock

          parseStringAnswerOpt :: BlockP (Text,Text)
          parseStringAnswerOpt = withFeedback noBlockQuotes

          parseAnswers :: [([Inline],[Text])] -> Either String Answers
          parseAnswers = (>>= constructQuestion) . mapLeft show . parseStringAnswers
          constructQuestion :: [(Text,AnswerProp)] -> Either String Answers
          constructQuestion = if qType == "numerical" then parseNumericAnswers else makeStringAnswers qType
          parseNums :: Text -> Either String (NumType,NumType)
          parseNums = mapLeft show . parse numPair "numerical answer" . textToString
            where buildFloat = either fromIntegral double2Float
                  numPair = do
                    targ <- buildFloat <$> naturalOrFloat tokenParser
                    tol <- optionMaybe $ many (char ' ') >> string "+-" >> many (char ' ') >> (buildFloat <$> naturalOrFloat tokenParser)
                    return (targ, fromMaybe 0.01 tol)
          parseNumericAnswers :: [(Text,AnswerProp)] -> Either String Answers
          parseNumericAnswers = fmap Numerical . sequence . fmap (seqFirst . first parseNums)
          parseStringAnswers :: [([Inline],[Text])] -> Either ParseError [(Text,AnswerProp)]
          parseStringAnswers = fmap (fmap (swap . first (flip AnswerProp []))) . parseFractionPart
          parseFractionPart :: [([Inline],[Text])] -> Either ParseError [(Int,Text)]
          parseFractionPart = freak . fmap (first parseAnswerFraction)
          freak :: Monad m => [(m a,[b])] -> m [(a,b)]
          freak = sequence . fmap seqFirst . concat . fmap seqSecond
          parseAnswerFraction :: [Inline] -> Either ParseError Int
          parseAnswerFraction = parse fraction "answer header" . inlinesToString
              where fraction = fracTrue <|> fracFalse <|> numericFraction
                    fracTrue  = (string "true" <|> string "True" <|> string "correct" <|> string "Correct") >> return 100
                    fracFalse = (string "false" <|> string "False" <|> string "wrong" <|> string "Wrong") >> return 0
                    numericFraction = read <$> many1 digit

defList :: BlockP [([Inline],[Text])]
defList = tokenPrim show incPos (\blk -> case blk of
                  DefinitionList defs -> Just defs
                  _                   -> Nothing)

noHeader :: BlockP Block
noHeader = tokenPrim show incPos (\blk -> case blk of
    Header {} -> Nothing
    x         -> Just x)

noHeaderNoDefList :: BlockP Block
noHeaderNoDefList = tokenPrim show incPos (\blk -> case blk of
    Header {}        -> Nothing
    DefinitionList _ -> Nothing
    x                -> Just x)

headerN :: Int -> BlockP (Attr, String)
headerN level = second inlinesToString <$> tokenPrim show incPos (\blk -> case blk of
    Header lvl' attr inls | lvl' == level -> Just (attr,inls)
    _                                     -> Nothing)

blockQuote :: BlockP Text
blockQuote = tokenPrim show incPos (\blk -> case blk of
              BlockQuote x -> Just x
              _            -> Nothing)

noBlockQuotes :: BlockP Text
noBlockQuotes = many $ tokenPrim show incPos (\blk -> case blk of
              BlockQuote _ -> Nothing
              x            -> Just x)


answersFromAttr :: Attr -> String
answersFromAttr (_,classes,_) = head classes

pandoc2questions :: Pandoc -> Either ParseError [Question]
pandoc2questions (Pandoc _ text) = concat <$> parse (many pQuestionBlock) "input" text
    where

          -- returns (answer title, answer prefix, answers)
          pAnswerSection :: BlockP (String, [Block], Answers)
          pAnswerSection = do
            (aAttr, aTitle) <- headerN 2
            aBody <- many noHeaderNoDefList
            answers <- answerDefList $ answersFromAttr aAttr
            skipMany noHeader
            return (aTitle,aBody,answers)

          pQuestionBlock :: BlockP [Question]
          pQuestionBlock = do
            (_, questionTitle) <- headerN 1
            qBody <- many noHeader
            answerSections <- many pAnswerSection
            return $ fmap (buildQuestion questionTitle qBody) answerSections
          -- Make a question from: question title, question body, (answer title, answer prefix, answers)
          buildQuestion :: String -> Text -> (String, Text, Answers) -> Question
          buildQuestion qTitle qBody (aTitle,aPrefix,aBody) = Question (qTitle ++ " - " ++ aTitle) (qBody ++ aPrefix) aBody

parseMoodleMD :: String -> Either ParseError [Question]
parseMoodleMD = pandoc2questions . readMarkdown def

readMoodleMDFile :: FilePath -> IO (Either ParseError [Question])
readMoodleMDFile file = parseMoodleMD <$> readFile file
