{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Text.Pandoc hiding (Attr)
import qualified Text.Pandoc.Definition as PD
import Text.Pandoc.Writers.AsciiDoc
import Data.Either (rights)
import Control.Applicative hiding (many)
import Text.XML.Light
import Text.Highlighting.Kate.Styles (kate)
import Data.Monoid (mempty)

type NumType = Float
type Text = [Block]

-- truth, feedback
data AnswerProp = AnswerProp Int [Block] deriving Show

data Answers 
    = Multichoice [(Text,AnswerProp)]
    | ShortAnswer [(Text,AnswerProp)]
    | TrueFalse   [(Text,AnswerProp)]
    | Numerical   [((NumType,NumType),AnswerProp)] -- ^Numerical answer with (value,tolerance) tuples as answer options.
    deriving Show

answerType :: Answers -> String
answerType (Multichoice _) = "multichoice"
answerType (ShortAnswer _) = "shortanswer"
answerType (TrueFalse _) = "truefalse"
answerType (Numerical _) = "numerical"

answerContent :: Answers -> Either [(Text,AnswerProp)] [((NumType,NumType),AnswerProp)]
answerContent (Multichoice x) = Left x
answerContent (ShortAnswer x) = Left x
answerContent (TrueFalse x) = Left x
answerContent (Numerical x) = Right x

renderAnswers :: Answers -> [Element]
renderAnswers = either (fmap procTxt) (fmap procNum) . answerContent
    where procTxt (txt,(AnswerProp fraction fb)) = add_attrs [uAttr "format" "html", uAttr "fraction" $ show fraction]. unode "answer" $ makeCData txt
          procNum ((target,tolerance),props) = undefined

-- |A question body consisting of a title, a Body, and an Answer option.
data Question = Question String Text Answers deriving Show

md2moodle :: String -> String
md2moodle = show.readMarkdown def

-- |Check whether a block is a header of given level.
isHeader :: Int -> Block -> Bool
isHeader lvl (Header lvl' _ _) = lvl == lvl' 
isHeader _ _ = False

-- |Construct a token from a boolean predicate.
myToken ::  (Show a) => (a -> Bool) -> Parsec [a] () a
myToken test = tokenPrim show incPos $ justIf test where
  justIf test x = if (test x) then Just x else Nothing

incPos pos _ _ = incSourceColumn pos 1

inlinesToString :: [Inline] -> String
inlinesToString inls = writeAsciiDoc def $ Pandoc nullMeta [Plain inls]

blocksToString :: Text -> String
blocksToString blks = writeAsciiDoc def $ Pandoc nullMeta blks

readStringAnswer :: [Block] -> Maybe (Text,AnswerProp)
readStringAnswer ((Plain ((Str score):rest)):rrest) = either (const Nothing) Just. fmap (\sc -> (Plain rest:rrest,AnswerProp sc [])) $ parse scoreP "score block" score
    where scoreP = fmap read $ many1 digit <* char ':'

readStringAnswer _ = Nothing

parseStringAnswers :: [Text] -> Maybe [(Text,AnswerProp)]
parseStringAnswers bullets = sequence $ fmap readStringAnswer bullets

parseMoodle :: Pandoc -> Either ParseError [Question]
parseMoodle (Pandoc meta text) = parse (many question) "input" text
    where noHeader = tokenPrim show incPos (\blk -> case blk of
            Header _ _ _ -> Nothing
            x            -> Just x)
          headerN :: Int -> Parsec [Block] () (PD.Attr, [Inline])
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

uAttr :: String -> String -> Attr
uAttr un = Attr $ unqual un

makeCData :: Text -> Element
makeCData txt = unode "text" (CData CDataVerbatim asHtml Nothing)
    where asHtml = writeHtmlString myWriterOptions (Pandoc nullMeta txt)

renderQs :: [Question] -> String            
renderQs = ppTopElement . unode "quiz" . fmap renderQ
    where renderQ (Question title body answers) = add_attr typeAttr . unode "question" $ [qName,qText] ++ renderAnswers answers
            where typeAttr = Attr (unqual "type") (answerType answers)
                  qName = unode "name" $ unode "text" title
                  qText = add_attr (uAttr "format" "html") $ unode "questiontext" $ makeCData body

readExample :: IO Pandoc
readExample = do
    txt <- readFile "testsuite/example-input/example.md"
    return $ readMarkdown def txt
    
main ::  IO ()
main = do
    pd <- readExample
    putStrLn . either show renderQs $ parseMoodle pd

myWriterOptions = WriterOptions { writerStandalone       = False
        , writerTemplate         = ""
        , writerVariables        = []
        , writerTabStop          = 4
        , writerTableOfContents  = False
        , writerSlideVariant     = NoSlides
        , writerIncremental      = False
        , writerHTMLMathMethod   = MathML Nothing
        , writerIgnoreNotes      = False
        , writerNumberSections   = False
        , writerNumberOffset     = [0,0,0,0,0,0]
        , writerSectionDivs      = False
        , writerExtensions       = pandocExtensions
        , writerReferenceLinks   = False
        , writerWrapText         = True
        , writerColumns          = 72
        , writerEmailObfuscation = JavascriptObfuscation
        , writerIdentifierPrefix = ""
        , writerSourceURL        = Nothing
        , writerUserDataDir      = Nothing
        , writerCiteMethod       = Citeproc
        , writerHtml5            = False
        , writerHtmlQTags        = False
        , writerBeamer           = False
        , writerSlideLevel       = Nothing
        , writerChapters         = False
        , writerListings         = False
        , writerHighlight        = False
        , writerHighlightStyle   = kate
        , writerSetextHeaders    = True
        , writerTeXLigatures     = True
        , writerEpubVersion      = Nothing
        , writerEpubMetadata     = ""
        , writerEpubStylesheet   = Nothing
        , writerEpubFonts        = []
        , writerEpubChapterLevel = 1
        , writerTOCDepth         = 3
        , writerReferenceODT     = Nothing
        , writerReferenceDocx    = Nothing
        , writerMediaBag         = mempty
}
