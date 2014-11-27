import Text.MoodleMD.Types
import Text.MoodleMD.Reader
import Text.MoodleMD.WriterXML
import Text.Pandoc
import System.Environment

main ::  IO ()
main = do
    args <- getArgs
    fileContent <- readFile $ head args
    putStrLn . either show renderQs . parseMoodle $ readMarkdown def fileContent

