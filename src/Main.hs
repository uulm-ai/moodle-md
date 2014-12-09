import Text.MoodleMD.Types
import Text.MoodleMD.Reader
import Text.MoodleMD.WriterXML
import System.Environment
import Control.Applicative
import System.Console.ArgParser

data Config = Config {input :: FilePath} deriving Show

cliParser :: ParserSpec Config
cliParser = Config
    `parsedBy` optPos "" "input-file" `Descr` "input file name; leave blank for reading from stdin"

main ::  IO ()
main = withParseResult cliParser run

run :: Config -> IO ()
run (Config inFile) = do
    input <- if inFile == "" then getContents else readFile inFile
    let result = either (error.show) id . parseMoodleMD $ input
    rendered <- renderQs result
    putStrLn rendered

