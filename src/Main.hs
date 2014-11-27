import Text.MoodleMD.Types
import Text.MoodleMD.Reader
import Text.MoodleMD.WriterXML
import Text.Pandoc

readExample :: IO Pandoc
readExample = do
    txt <- readFile "testsuite/example-input/example.md"
    return $ readMarkdown def txt
    
main ::  IO ()
main = do
    pd <- readExample
    putStrLn . either show renderQs $ parseMoodle pd

