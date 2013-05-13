import DirHound.Request
import DirHound.Wordlist
import Network.URI
import System.Console.CmdArgs
import System.IO 

data Args = Args {
                wordlist :: String,
                url :: String,
                outfile :: String
            }
            deriving (Show, Data, Typeable)


arguments = Args{wordlist = "wordlist" &= typFile,
                url = def &= args &= typ "URL",
                outfile = "dirhound.out" &= typFile
                } &= program "DirHound" &= summary "DirHound web server directory bruteforcer"

main = do
        parsed <- cmdArgs arguments
        let wordlist_name = wordlist parsed
            uri = url parsed
            in do wlist <- readWordlist wordlist_name
                  hfile <- openFile (outfile parsed) WriteMode
                  case parseURI uri of 
                    Just x -> processLoop (makeCrawler x wlist) hfile
                    Nothing -> error "Not a valid URL"
                  hClose hfile
