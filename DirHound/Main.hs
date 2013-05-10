import DirHound.Request
import DirHound.Wordlist
import Network.URI
import System.Console.CmdArgs


data Args = Args {
                wordlist :: String,
                url :: String
            }
            deriving (Show, Data, Typeable)


arguments = Args{wordlist = "wordlist" &= typFile,
                url = def &= args &= typ "URL"
                } &= program "DirHound" &= summary "DirHound web server directory bruteforcer"

main = do
        parsed <- cmdArgs arguments
        let wordlist_name = wordlist parsed
            uri = url parsed
            in do wlist <- readWordlist wordlist_name
                  case parseURI uri of 
                    Just x -> processLoop (makeCrawler x wlist)
                    Nothing -> error "Not a valid URL"
