{-# LANGUAGE OverloadedStrings #-}

import Text.MoodleMD.Types
import Text.MoodleMD.Reader

import Text.Pandoc hiding (Attr)
import qualified Text.Pandoc.Definition as PD
import Text.Pandoc.Writers.AsciiDoc
import Data.Either (rights)
import Control.Applicative hiding (many)

readExample :: IO Pandoc
readExample = do
    txt <- readFile "testsuite/example-input/example.md"
    return $ readMarkdown def txt
    
main ::  IO ()
main = do
    pd <- readExample
    putStrLn . either show renderQs $ parseMoodle pd

