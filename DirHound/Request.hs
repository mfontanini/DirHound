module DirHound.Request() where

import Network.HTTP
import Network.URI
import Text.Regex.Posix
import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data CrawlInstance = CrawlInstance URI [URI] (Set URI)

baseURI (CrawlInstance base _ _) = base

makeCrawler base wordlist = CrawlInstance base wordlist Set.empty

regex = "<a.+href=\"([^\"]+)|<img.+src=\"([^\"]+)\"|<iframe.+src=\"([^\"]+)\"|<form.+action=\"([^\"]+)\""

findLinks :: String -> [String]
findLinks html = map (concat . tail) (html =~ regex :: [[String]])


-- Makes an HTTP GET request to the given URI
makeHTTPRequest :: URI -> IO (String)
makeHTTPRequest s = simpleHTTP (mkRequest GET s) >>= getResponseBody

-- Parses a list of Strings, parses them as URIs and returns those
-- that are parsed correctly
parseAllURIs = map fromJust . filter isJust . (map parseURIReference)

-- Takes a list of Strings representing URIs, parsed them as such
-- and returns a list containing those URIs, interpretted as
-- relative to a base URI
makeRelativeURIs crawler = map (\x -> relativeTo x (baseURI crawler)) . parseAllURIs

-- Takes a list of Strings representing URIs, parsed them and keeps
-- those that actually represent a relative URI
filterExternalURIs crawler = filter ((==) (uriAuthority (baseURI crawler)) . uriAuthority) . (makeRelativeURIs crawler)

-- Makes a request to the given URI, taking "base" as the base URI
processURI crawler uri = fmap (filterExternalURIs crawler) (fmap findLinks (makeHTTPRequest uri))



testMe = case parseURI "http://www2.cert.unlp.edu.ar/" of 
            Just x -> processURI (makeCrawler x []) x
