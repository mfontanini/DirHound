-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
-- MA 02110-1301, USA.

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
