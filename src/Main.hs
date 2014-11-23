{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
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


myToken ::  (Show a) => (a -> Bool) -> Parsec [a] () a
myToken test = tokenPrim show incPos $ justIf test where
  incPos pos _ _ = incSourceColumn pos 1
  justIf test x = if (test x) then Just x else Nothing

extractHeaders :: Int -> [Block] -> [(Block, [Block])]
extractHeaders level doc = case parse (many docs) "your list" doc of
        Left err -> error $ show err
        Right result -> result
    where h1 = myToken $ isHeader level
          notH1 = myToken $ not . isHeader level
          docs = do
            header <- h1
            rest <- many notH1
            return (header,rest)
    

readExample :: IO Pandoc
readExample = do
    txt <- readFile "testsuite/example-input/example.md"
    return $ readMarkdown def txt
    
main ::  IO ()
main = do
    (Pandoc m bs) <- readExample
    print $ extractHeaders 1 bs
