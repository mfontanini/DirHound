module DirHound.Wordlist(readWordlist) where

import Network.URI
import Data.Maybe

makeURIList all = let x = map parseURIReference all
                    in case length (filter isNothing x) of
                        0 -> map fromJust x
                        _ -> error "Invalid URL in file" 

readWordlist path = fmap makeURIList (fmap lines (readFile path))
