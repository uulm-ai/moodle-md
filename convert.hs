import Text.Pandoc

data Blck = Pln [String] | MD [String] deriving (Show)

-- parse the input and extract blocks that must be converted by pandoc (interpreted as markdown)
-- those blocks start with `<**` on a single line and end with `**>` on a single line
blockify :: [String] -> [Blck]
blockify = takePlain
    where takePlain ls = let (pl,rest) = span (/= "<**") ls in Pln pl : (if rest == [] then [] else takeMD.tail$rest)
          takeMD ls    = let (md,rest) = span (/= "**>") ls in MD md : (if rest == [] then [] else takePlain.tail$rest)

convertLines :: String -> String
convertLines ls = unlines.map procBlck.blockify.lines$ls

procBlck :: Blck -> String
procBlck (Pln x) = unlines x
procBlck (MD x)  = "[html] " ++ (writeHtmlString def.readMarkdown def.unlines$x)

main = interact convertLines
