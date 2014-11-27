module Text.MoodleMD.Reader (parseMoodle) where

import Text.MoodleMD.Types
import Text.Pandoc
import Text.Parsec
import Control.Applicative ((<*))

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

parseStringAnswers :: [Text] -> Maybe [(Text,AnswerProp)]
parseStringAnswers bullets = sequence $ fmap readStringAnswer bullets

parseMoodle :: Pandoc -> Either ParseError [Question]
parseMoodle (Pandoc _ text) = parse (many question) "input" text
    where noHeader = tokenPrim show incPos (\blk -> case blk of
            Header _ _ _ -> Nothing
            x            -> Just x)
          headerN :: Int -> Parsec [Block] () (Attr, [Inline])
          headerN level = tokenPrim show incPos (\blk -> case blk of
                Header lvl' attr inls | lvl' == level -> Just (attr,inls)
                _                                     -> Nothing)
          bulletList :: Parsec [Block] () [Text]
          bulletList = tokenPrim show incPos (\blk -> case blk of
                BulletList bullets -> Just bullets
                _                  -> Nothing)
          question = do
            (tAttr, tInlines) <- headerN 1
            qBody <- many noHeader
            (aAttr, aInlines) <- headerN 2
--            skipMany noHeader
            answers <- fmap parseStringAnswers bulletList
            ans' <- maybe (unexpected "couldn't parse answer options") (return.ShortAnswer) answers 
--            skipMany noHeader
            return $ Question (inlinesToString tInlines) qBody ans'
