module DirHound.Request() where

import Network.HTTP.Base
import Network.HTTP
import Network.URI
import Network.Stream
import Text.Regex.Posix
import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- Crawler data

data CrawlInstance = CrawlInstance URI [URI] (Set URI)

baseURI (CrawlInstance base _ _) = base

makeCrawler base wordlist = CrawlInstance base wordlist Set.empty

filterVisited (CrawlInstance _ _ set) urls = Set.toList (Set.difference (Set.fromList urls) set)

markVisited (CrawlInstance a b set) url = CrawlInstance a b (Set.insert url set)

visitedURLs (CrawlInstance _ _ set) = Set.toList set

wordlist (CrawlInstance _ wlist _) = wlist

-- Regexes 

regex = "<a.+href=\"([^\"]+)|<img.+src=\"([^\"]+)\"|<iframe.+src=\"([^\"]+)\"|<form.+action=\"([^\"]+)\""

findLinks :: String -> [String]
findLinks html = map (concat . tail) (html =~ regex :: [[String]])


-- Makes an HTTP GET request to the given URI
makeHTTPRequest :: URI -> IO (String)
makeHTTPRequest uri = simpleHTTP (mkRequest GET uri) >>= getResponseBody

badResponses = [404, 302]

responseCodeFromXYZ (x, y, z) = x * 100 + y * 10 + z

appendURIPath uri suffix = URI (uriScheme uri) (uriAuthority uri) (uriPath uri ++ suffix) "" ""

-- Appends a "/" at the end of the URI if it's not present yet.
normalizeURIDir uri = if isSuffixOf "/" (uriPath uri) 
                      then uri
                      else appendURIPath uri "/"

-- Checks whether an URI is a directory

isDirectory uri = uriExists (normalizeURIDir uri)

-- Checks whether an URI exists
uriExists :: URI -> IO (Bool)
uriExists uri = do rsp <- simpleHTTP ((mkRequest HEAD uri) :: Request String) >>= getResponseCode
                   let code = responseCodeFromXYZ rsp
                    in do return (notElem code badResponses)

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
retrieveLinks crawler uri = fmap (filterExternalURIs crawler) (fmap findLinks (makeHTTPRequest uri))

-- Processes an URI, marking it as visited.
-- Returns a tuple IO (CrawlInstance, [URI])
processURI crawler uri = do links <- retrieveLinks crawler uri
                            let c = markVisited crawler uri
                                new_links = filterVisited c links
                                in return (foldr (flip markVisited) c new_links, new_links)

-- bruteforces a directory
bruteforceDir wlist uri = filterM uriExists (map (\x -> appendURIPath uri (uriPath x)) wlist)

tryBruteforceDir crawler uri = do exists <- isDirectory uri
                                  if exists
                                  then bruteforceDir (wordlist crawler) uri
                                  else return []
                                

processLoop' :: CrawlInstance -> [URI] -> IO (CrawlInstance)
processLoop' crawler [] = return crawler
processLoop' crawler (x:xs) = do (c, u) <- processURI crawler x
                                 processLoop' c (xs ++ u)

processLoop crawler = processLoop' crawler ([baseURI crawler])



doTestMe = case parseURI "http://www2.cert.unlp.edu.ar/" of 
            Just x -> processLoop (makeCrawler x [])
            
--testMe = do x <- doTestMe
--            visitedURLs x
