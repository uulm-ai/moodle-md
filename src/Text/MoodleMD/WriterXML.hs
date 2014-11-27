module Text.MoodleMD.WriterXML where

import Text.MoodleMD.Types
import Text.Pandoc hiding (Attr)
import Text.XML.Light
import Text.Highlighting.Kate.Styles (kate)
import Data.Monoid (mempty)

renderAnswers :: Answers -> [Element]
renderAnswers = either (fmap procTxt) (fmap procNum) . answerContent
    where procTxt (txt,(AnswerProp fraction _)) =
                add_attrs [uAttr "format" "html", uAttr "fraction" $ show fraction] $
                        unode "answer" $ makeCData txt
          procNum :: ((NumType,NumType),AnswerProp) -> Element
          procNum ((target,tolerance),(AnswerProp fraction _)) =
                add_attrs [uAttr "fraction" $ show fraction] $
                        unode "answer" $ [unode "text" $ show target, unode "tolerance" $ show tolerance]

blocksToString :: Text -> String
blocksToString blks = writeAsciiDoc def $ Pandoc nullMeta blks

uAttr :: String -> String -> Attr
uAttr un = Attr $ unqual un

makeCData :: Text -> Element
makeCData txt = unode "text" (CData CDataVerbatim asHtml Nothing)
    where asHtml = writeHtmlString myWriterOptions (Pandoc nullMeta txt)

renderQs :: [Question] -> String            
renderQs = ppTopElement . unode "quiz" . fmap renderQ
    where renderQ (Question title body answers) = add_attr typeAttr . unode "question" $ [questionName,questionText] ++ renderAnswers answers
            where typeAttr = Attr (unqual "type") (answerType answers)
                  questionName = unode "name" $ unode "text" title
                  questionText = add_attr (uAttr "format" "html") $ unode "questiontext" $ makeCData body

-- |Rendering options for producing HTML with Pandoc.
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
