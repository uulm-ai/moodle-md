module Text.MoodleMD.Reader (pandoc2questions, parseMoodleMD, readMoodleMDFile) where

import Text.MoodleMD.Types
import Text.Pandoc
import Text.Parsec hiding ((<*>))
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)
import Control.Arrow
import Data.Tuple (swap)
import GHC.Float
import Data.Maybe (fromMaybe)
import Control.Applicative (pure,(<$>), (<*>))
import Control.Monad  (mfilter)
import Text.Pandoc.Shared (stringify)

type BlockP a = Parsec [Block] () a

-- |Helper to increment Source position parsing arbitrary lists with Parsec
incPos pos _ _ = incSourceColumn pos 1

tokenParser = makeTokenParser javaStyle

-- |Convert to normal string.
textToString :: Text -> String
textToString = stringify

-- |For converting the question title to a normal string.
inlinesToString :: [Inline] -> String
inlinesToString = stringify

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

-- |the direct parse result for answer sections, common to all answer types
data AnswerBlock = AnswerBlock { abTitle :: String                  -- ^Answer title, will be appended to question title
                               , abType :: String                   -- ^type of question
                               , abPrefix :: Text                   -- ^specific answer text, will be appended to question body
                               , abAnswers :: [(Text,AnswerProp)] } -- ^fraction, then answer/feedback combinations

pNumericAnswer :: Text -> Either ParseError (NumType,NumType)
pNumericAnswer = error "numeric parser not implemented"

processAnswerBlock :: AnswerBlock -> Either ParseError (String,Text,Answers)
processAnswerBlock (AnswerBlock aTitle aType aPrefix aAnswers)
    |aType == "numerical" = let x3 f (ma,b,c) = do a <- ma; return (f a,b,c) in
                            fmap ((\a -> (aTitle,aPrefix,a)) . Numerical) . sequence . fmap seqFirst $ first pNumericAnswer <$> aAnswers
    |otherwise            = Right (aTitle, aPrefix, either (error "unknown question type") id $ makeStringAnswers aType aAnswers)

pAnswerBlock :: BlockP AnswerBlock
pAnswerBlock = do
        ((_,classes,_),title) <- headerN 2
        qClass <- reparse $ parse pClass "question type" classes --parse a [String] and convert back to BlockP parser
        prefix <- many noHeaderNoDefList
        definitions <- defList :: BlockP [([Inline],[Text])]
        answers <- reparse $ do
            d <- Right definitions
            withFracs <- convertFractions d :: Either ParseError [(Int,[Text])]
            let withFeedback = second (fmap splitFeedback) <$> withFracs :: [(Int,[(Text,Text)])]
            return $ makeAnswerProp <$> distributeFractions withFeedback
        return $ AnswerBlock title (head classes) prefix answers
    where convertFractions :: [([Inline],a)] -> Either ParseError [(Int,a)]
          convertFractions = sequence . fmap (seqFirst . first parseFraction)
          splitFeedback :: Text -> (Text,Text)
          splitFeedback = either (error "could not split feedback") id . parse pSplitFeedback "answer with feedback"
                where pSplitFeedback = do
                        answer <- noBlockQuotes
                        feedback <- optionMaybe blockQuote
                        return (answer, fromMaybe [] feedback)
          distributeFractions :: [(a,[(b,c)])] -> [(a,b,c)]
          distributeFractions = fmap flatTup . concat . (seqSecond <$>) where flatTup (a,(b,c)) = (a,b,c)
          makeAnswerProp :: (Int,Text,Text) -> (Text,AnswerProp)
          makeAnswerProp (f,a,fb) = (a,AnswerProp f fb)
          -- |parse the list of header classes to find the question type
          pClass :: Parsec [String] () String
          pClass = tokenPrim show incPos $ mfilter (`elem` questionClasses) . Just
          -- |parse the faction definition (the amount of points for an answer)
          parseFraction :: [Inline] -> Either ParseError Int
          parseFraction = parse fraction "answer header" . inlinesToString
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

pandoc2questions :: Pandoc -> Either ParseError [Question]
pandoc2questions (Pandoc _ text) = concat <$> parse (many pQuestionBlock) "input" text
    where
          -- returns (answer title, answer prefix, answers)
          pAnswerSection :: BlockP (String, [Block], Answers)
          pAnswerSection = reparse =<< (processAnswerBlock <$> pAnswerBlock)

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
