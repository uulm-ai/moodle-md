{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc
import Text.XML

data Answer = Multichoice 
            | Numerical
            | ShortAnswer
            | TrueFalse deriving (Show)

data Question = Question { qname :: Maybe String
                         , qbody :: [Block]
                         , qfeedback :: Maybe String } deriving (Show)
                     
-- parsePandoc :: Pandoc -> [Question]
-- parsePandoc pd = () 

md2moodle :: String -> String
md2moodle = show.readMarkdown def

main ::  IO ()
main = do
    interact md2moodle
    print "done"


-- xmlTest = renderText def d
--     where d = 
