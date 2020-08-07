import NFA (fromRegex, writeNFA)
import Regex (parse)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putTextLn "Not enough arguments."
    s : _ -> program (toText s)

program :: Text -> IO ()
program input = case parse input of
  Left err -> putTextLn "Error:" >> print err
  Right reg -> do
    let nfa = fromRegex reg
    writeNFA one nfa
