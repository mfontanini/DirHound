import System.Environment
import DirHound.Request
import DirHound.Wordlist
import Network.URI

main = do
        args <- getArgs
        wordlist <- readWordlist "wordlist"
        case parseURI (head args) of 
            Just x -> processLoop (makeCrawler x wordlist)
