module DirHound.Request() where

import Network.HTTP.Base
import Network.HTTP
import Network.URI
import Network.Stream
import Text.Regex.Posix
import Control.Monad
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

-- Regexes 

regex = "<a.+href=\"([^\"]+)|<img.+src=\"([^\"]+)\"|<iframe.+src=\"([^\"]+)\"|<form.+action=\"([^\"]+)\""

findLinks :: String -> [String]
findLinks html = map (concat . tail) (html =~ regex :: [[String]])


-- Makes an HTTP GET request to the given URI
makeHTTPRequest :: URI -> IO (String)
makeHTTPRequest uri = simpleHTTP (mkRequest GET uri) >>= getResponseBody

-- Checks whether a URI is a directory.
isDirectory :: URI -> IO (Bool)
isDirectory uri = do http <- simpleHTTP ((mkRequest HEAD uri) :: Request String) 
                     rsp <- getResponseCode http
                     let (x,y,z) = rsp
                        in do return (x == 5)

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

processLoop' :: CrawlInstance -> IO [URI] -> IO (CrawlInstance)
processLoop' crawler io_lst = do 
                                lst <- io_lst
                                if null lst
                                    then return crawler
                                    else do (c, u) <- processURI crawler (head lst)
                                            processLoop' c (return ((tail lst) ++ u))

--processLoop' crawler lst = do 
--                            (c, u) <- fmap processURI (return crawler) (fmap head lst)
--                            processLoop' c (fmap ((++) (tail lst)) u)

processLoop crawler = processLoop' crawler (return [baseURI crawler])



doTestMe = case parseURI "http://www2.cert.unlp.edu.ar/" of 
            Just x -> processLoop (makeCrawler x [])
            
--testMe = do x <- doTestMe
--            visitedURLs x
