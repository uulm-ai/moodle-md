{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc
import Text.Pandoc.Definition

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

isHeader :: Int -> Block -> Bool
isHeader lvl (Header lvl' _ _) = lvl == lvl' 
isHeader _ _ = False

extractHeaders :: Int -> [Block] -> ([Block],[(Block, [Block])])
extractHeaders level doc = (pre, shatter [] rest)
    where spanH = span (not.isHeader level)
          (pre,rest) = spanH doc
          shatter :: [(Block,[Block])] -> [Block] -> [(Block,[Block])] 
          shatter acc [] = reverse acc
          shatter acc (h:r) = 
                let (pre',rest') = spanH r 
                in shatter ((h,pre'):acc) rest'
    

readExample :: IO Pandoc
readExample = do
    txt <- readFile "testsuite/example-input/example.md"
    return $ readMarkdown def txt
    
main ::  IO ()
main = do
    (Pandoc m bs) <- readExample
    print $ extractHeaders 1 bs
