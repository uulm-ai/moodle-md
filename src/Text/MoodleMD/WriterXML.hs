module Text.MoodleMD.WriterXML where

import Text.MoodleMD.Types
import Text.Pandoc hiding (Attr)
import Text.XML.Light
import Text.Pandoc.SelfContained (makeSelfContained)

renderAnswers :: Answers -> IO [Element]
renderAnswers = either (sequence . fmap procTxt) (return . fmap procNum) . answerContent
    where procTxt (txt,AnswerProp fraction _) = do
                cdata <- makeCData txt
                let attributes = [uAttr "format" "html", uAttr "fraction" $ show fraction]
                return . add_attrs attributes . unode "answer" $ cdata
          procNum :: ((NumType,NumType),AnswerProp) -> Element
          procNum ((target,tolerance),AnswerProp fraction _) =
                add_attrs [uAttr "fraction" $ show fraction] $
                        unode "answer" [unode "text" $ show target, unode "tolerance" $ show tolerance]

blocksToString :: Text -> String
blocksToString blks = writeAsciiDoc def $ Pandoc nullMeta blks

uAttr :: String -> String -> Attr
uAttr un = Attr $ unqual un

makeCData :: Text -> IO Element
makeCData txt = do
        content <- asHtml
        return $ unode "text" (CData CDataVerbatim content Nothing)
    where asHtml = makeSelfContained def $ writeHtmlString myWriterOptions (Pandoc nullMeta txt)

renderQs :: [Question] -> IO String
renderQs = fmap (ppTopElement . unode "quiz") . sequence . fmap renderQ
    where renderQ (Question title body answers) = do
            renderedAnswers <- renderAnswers answers
            let typeAttr = Attr (unqual "type") (answerType answers)
            let questionName = unode "name" $ unode "text" title
            qBody <- makeCData body
            let questionText = add_attr (uAttr "format" "html") $ unode "questiontext" qBody
            return $ add_attr typeAttr . unode "question" $ [questionName,questionText] ++ renderedAnswers

-- |Rendering options for producing HTML with Pandoc.
myWriterOptions :: WriterOptions
myWriterOptions = def {writerHTMLMathMethod   = MathML Nothing}