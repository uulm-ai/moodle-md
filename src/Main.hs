import Text.MoodleMD.Types
import Text.MoodleMD.Reader
import Text.MoodleMD.WriterXML
import Text.Pandoc
import System.Environment

main ::  IO ()
main = do
    f <- fmap head getArgs
    result <- readMoodleMDFile f
    putStrLn . either show renderQs $ result

