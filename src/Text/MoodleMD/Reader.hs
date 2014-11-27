module Text.MoodleMD.Reader (parseMoodle) where

import Text.MoodleMD.Types
import Text.Pandoc
import Text.Parsec
import Control.Applicative ((<*))
import Control.Arrow
import Data.Tuple (swap)

-- |Helper to increment Source position parsing arbitrary lists with Parsec
incPos pos _ _ = incSourceColumn pos 1

-- |For converting the question title to a normal string.
inlinesToString :: [Inline] -> String
inlinesToString inls = writeAsciiDoc def $ Pandoc nullMeta [Plain inls]

-- |Parse some blocks (this will be changed)
readStringAnswer :: [Block] -> Maybe (Text,AnswerProp)
readStringAnswer ((Plain ((Str score):rest)):rrest) = either (const Nothing) Just. fmap (\sc -> (Plain rest:rrest,AnswerProp sc [])) $ parse scoreP "score block" score
    where scoreP = fmap read $ many1 digit <* char ':'
readStringAnswer _ = Nothing

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
     
answerDefList :: String -> Parsec [Block] () Answers
answerDefList qType = tokenPrim show incPos (\blk -> case blk of
        DefinitionList defs -> Just $ parseAnswers defs
        _                   -> Nothing) >>= either (unexpected.show) return
    where parseAnswers :: [([Inline],[Text])] -> Either ParseError Answers
          parseAnswers = fmap constructQuestion . parseStringAnswers
          constructQuestion :: [(Text,AnswerProp)] -> Answers
          constructQuestion = if qType == "numerical" then Numerical . fmap (parseNums *** id) else makeStringAnswers qType 
          parseNums :: Text -> (NumType,NumType)
          parseNums _ = (0,0)
          parseStringAnswers :: [([Inline],[Text])] -> Either ParseError [(Text,AnswerProp)]
          parseStringAnswers = fmap (fmap ((id *** (flip AnswerProp) []) . swap)) . freak . fmap (parseAnswerFraction *** id)

answersFromAttr :: Attr -> String
answersFromAttr (_,classes,_) = head classes

parseMoodle :: Pandoc -> Either ParseError [Question]
parseMoodle (Pandoc _ text) = parse (many question) "input" text
    where noHeader = tokenPrim show incPos (\blk -> case blk of
            Header _ _ _ -> Nothing
            x            -> Just x)
          headerN :: Int -> Parsec [Block] () (Attr, [Inline])
          headerN level = tokenPrim show incPos (\blk -> case blk of
                Header lvl' attr inls | lvl' == level -> Just (attr,inls)
                _                                     -> Nothing)
          question = do
            (tAttr, tInlines) <- headerN 1
            qBody <- many noHeader
            (aAttr, aInlines) <- headerN 2
--            skipMany noHeader
            answers <- answerDefList $ answersFromAttr aAttr
--            skipMany noHeader
            return $ Question (inlinesToString tInlines) qBody answers
