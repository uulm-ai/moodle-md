import Test.HUnit
import Text.MoodleMD.Reader

exampleFiles :: [String]
exampleFiles = fmap ("example-input/" ++) ["example.md","no-numerical.md","small-numerical.md"]

parseFile f = TestCase $ do
    result <- readMoodleMDFile f
    assertBool ("file " ++ f ++ " could not be parsed") $ either (const False) (const True) result

main = runTestTT . TestList $ fmap parseFile exampleFiles