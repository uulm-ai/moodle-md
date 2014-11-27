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

-- |Helper to increment Source position parsing arbitrary lists with Parsec
incPos pos _ _ = incSourceColumn pos 1

-- |For converting the question title to a normal string.
inlinesToString :: [Inline] -> String
inlinesToString inls = writeAsciiDoc def $ Pandoc nullMeta [Plain inls]

textToString :: Text -> String
textToString = writeAsciiDoc def . Pandoc nullMeta

parseAnswerFraction :: [Inline] -> Either ParseError Int
parseAnswerFraction = parse fraction "answer header" . inlinesToString
    where fraction = fracTrue <|> fracFalse <|> numericFraction
          fracTrue  = (string "true" <|> string "True" <|> string "correct" <|> string "Correct") >> return 100
          fracFalse = (string "false" <|> string "False" <|> string "wrong" <|> string "Wrong") >> return 0
          numericFraction = fmap read $ many1 digit 

seqFirst :: Monad m => (m a,b) -> m (a,b)
seqFirst (ma,b) = do a <- ma; return (a,b)

seqSecond :: Monad m => (a,m b) -> m (a,b)
seqSecond (a,mb) = do b <- mb; return (a,b)

freak :: Monad m => [(m a,[b])] -> m [(a,b)]
freak = sequence . fmap seqFirst . concat . fmap seqSecond

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft f (Right r) = Right r

tokenParser = makeTokenParser javaStyle

answerDefList :: String -> Parsec [Block] () Answers
answerDefList qType = tokenPrim show incPos (\blk -> case blk of
        DefinitionList defs -> Just $ parseAnswers defs
        _                   -> Nothing) >>= either (unexpected.show) return
    where parseAnswers :: [([Inline],[Text])] -> Either String Answers
          parseAnswers = (>>= constructQuestion) . mapLeft show . parseStringAnswers
          constructQuestion :: [(Text,AnswerProp)] -> Either String Answers
          constructQuestion = if qType == "numerical" then parseNumericAnswers else makeStringAnswers qType
          parseNums :: Text -> Either String (NumType,NumType)
          parseNums = mapLeft show . parse numPair "numerical answer" . textToString
            where buildFloat = either fromIntegral double2Float
                  numPair = do
                    targ <- fmap buildFloat $ naturalOrFloat tokenParser
                    tol <- optionMaybe $ many (char ' ') >> string "+-" >> many (char ' ') >> (fmap buildFloat $ naturalOrFloat tokenParser)
                    return (targ, fromMaybe 0.01 tol)
          parseNumericAnswers :: [(Text,AnswerProp)] -> Either String Answers
          parseNumericAnswers = fmap Numerical . sequence . fmap seqFirst . fmap (parseNums *** id)
          parseStringAnswers :: [([Inline],[Text])] -> Either ParseError [(Text,AnswerProp)]
          parseStringAnswers = fmap (fmap ((id *** (flip AnswerProp) []) . swap)) . freak . fmap (parseAnswerFraction *** id)

answersFromAttr :: Attr -> String
answersFromAttr (_,classes,_) = head classes

pandoc2questions :: Pandoc -> Either ParseError [Question]
pandoc2questions (Pandoc _ text) = parse (many question) "input" text
    where noHeader = tokenPrim show incPos (\blk -> case blk of
              Header _ _ _ -> Nothing
              x            -> Just x)
          noHeaderNoDefList = tokenPrim show incPos (\blk -> case blk of
              Header _ _ _     -> Nothing
              DefinitionList _ -> Nothing
              x                -> Just x)
          headerN :: Int -> Parsec [Block] () (Attr, [Inline])
          headerN level = tokenPrim show incPos (\blk -> case blk of
                Header lvl' attr inls | lvl' == level -> Just (attr,inls)
                _                                     -> Nothing)
          question = do
            (_, tInlines) <- headerN 1
            qBody <- many noHeader
            (aAttr, _) <- headerN 2
            questionHeader <- many noHeaderNoDefList
            answers <- answerDefList $ answersFromAttr aAttr
--            skipMany noHeader
            return $ Question (inlinesToString tInlines) qBody answers

parseMoodleMD :: String -> Either ParseError [Question]
parseMoodleMD = pandoc2questions . readMarkdown def

readMoodleMDFile :: FilePath -> IO (Either ParseError [Question])
readMoodleMDFile file = fmap parseMoodleMD $ readFile file
