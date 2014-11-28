import Text.MoodleMD.Types
import Text.MoodleMD.Reader
import Text.MoodleMD.WriterXML
import System.Environment

main ::  IO ()
main = do
    f <- fmap head getArgs
    result <- fmap (either (error.show) id) $ readMoodleMDFile  f
    rendered <- renderQs $ result
    putStrLn $ rendered

