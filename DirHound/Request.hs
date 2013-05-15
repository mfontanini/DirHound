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

module DirHound.Request(processLoop, makeCrawler) where

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
import DirHound.Wordlist
import System.IO  

-- Crawler data

data Crawler = Crawler URI [URI] (Set URI) (Set URI)

baseURI (Crawler base _ _ _) = base

makeCrawler base wordlist = Crawler base wordlist Set.empty Set.empty

filterVisited (Crawler _ _ set _) urls = Set.toList (Set.difference (Set.fromList urls) set)

markVisited (Crawler a b set c) uri = Crawler a b (Set.insert uri set) c

visitedURLs (Crawler _ _ set _) = Set.toList set

wordlist (Crawler _ wlist _ _) = wlist

markDirVisited (Crawler a b c set) uri = Crawler a b c (Set.insert uri set)

isDirVisited (Crawler _ _ _ set) uri = Set.member uri set


-- Regexes 

regex = "<a.+href=\"([^\"]+)|<img.+src=\"([^\"]+)\"|<iframe.+src=\"([^\"]+)\"|<form.+action=\"([^\"]+)\""

findLinks :: String -> [String]
findLinks html = map (concat . tail) (html =~ regex :: [[String]])


-- Makes an HTTP GET request to the given URI
makeHTTPRequest :: URI -> IO (String)
makeHTTPRequest uri = simpleHTTP (mkRequest GET uri) >>= getResponseBody

badResponses = [404]

responseCodeFromXYZ :: (Int, Int, Int) -> Int
responseCodeFromXYZ (x, y, z) = x * 100 + y * 10 + z

appendURIPath :: URI -> String -> URI
appendURIPath uri suffix = URI (uriScheme uri) (uriAuthority uri) (uriPath uri ++ suffix) "" ""

replaceURIPath :: URI -> String -> URI
replaceURIPath uri path = URI (uriScheme uri) (uriAuthority uri) path "" ""

-- Appends a "/" at the end of the URI if it's not present yet.
normalizeURIDir :: URI -> URI
normalizeURIDir uri = if isSuffixOf "/" (uriPath uri) 
                      then uri
                      else appendURIPath uri "/"

-- Checks whether an URI is a directory

isDirectory :: URI -> IO (Bool)
isDirectory uri = uriExists (normalizeURIDir uri)

-- Checks whether an URI exists
uriExists :: URI -> IO (Bool)
uriExists uri = do rsp <- simpleHTTP ((mkRequest HEAD uri) :: Request String) >>= getResponseCode
                   let code = responseCodeFromXYZ rsp
                    in do return (notElem code badResponses)

-- Retrieves the base dir name

dirnameURI' path = case elemIndices '/' path
                    of (x:xs) -> fst (splitAt (last (x:xs) + 1) path)
                       [] -> "/"

dirnameURI :: URI -> URI
dirnameURI uri = replaceURIPath uri (dirnameURI' (uriPath uri))

-- Parses a list of Strings, parses them as URIs and returns those
-- that are parsed correctly
parseAllURIs :: [String] -> [URI]
parseAllURIs = map fromJust . filter isJust . (map parseURIReference)

-- Takes a list of Strings representing URIs, parsed them as such
-- and returns a list containing those URIs, interpretted as
-- relative to a base URI
makeRelativeURIs :: URI -> [String] -> [URI]
makeRelativeURIs uri = map (\x -> relativeTo x uri) . parseAllURIs

-- Takes a list of Strings representing URIs, parsed them and keeps
-- those that actually represent a relative URI
filterExternalURIs :: URI -> [String] -> [URI]
filterExternalURIs uri = filter ((==) (uriAuthority uri) . uriAuthority) . (makeRelativeURIs uri)

-- Makes a request to the given URI, retrieves the links and filters 
-- those than don't belong to the domain being crawled.
retrieveLinks :: URI -> IO [URI]
retrieveLinks uri = fmap (filterExternalURIs uri . findLinks) (makeHTTPRequest uri)

-- Processes an URI, marking it as visited.
-- Returns a tuple IO (CrawlInstance, [URI])
processURI :: Crawler -> URI -> IO (Crawler, [URI])
processURI crawler uri = do links <- retrieveLinks uri
                            let c = markVisited crawler uri
                                new_links = filterVisited c links
                                in return (foldr (flip markVisited) c new_links, new_links)

-- bruteforces a directory
bruteforceDir :: [URI] -> URI -> IO [URI]
bruteforceDir wlist uri = filterM uriExists (map (\x -> appendURIPath uri (uriPath x)) wlist)

tryBruteforceDir :: Crawler -> URI -> IO [URI]
tryBruteforceDir crawler uri = do exists <- isDirectory uri
                                  if exists
                                    then bruteforceDir (wordlist crawler) uri
                                    else return []

performBruteforce' :: Crawler -> URI -> IO (Crawler, [URI])
performBruteforce' crawler uri = if isDirVisited crawler uri
                                 then return (crawler, [])
                                 else do
                                      uris <- tryBruteforceDir crawler uri
                                      return (foldr (flip markDirVisited) crawler uris, uris)

performBruteforce :: Crawler -> URI -> IO (Crawler, [URI])
performBruteforce crawler uri = do 
                                    isDir <- isDirectory uri
                                    if isDir
                                        then performBruteforce' crawler uri
                                        else performBruteforce' crawler (dirnameURI uri)

printFoundURIs :: Handle -> [URI] -> IO ()
printFoundURIs _ [] = return ()
printFoundURIs log (x:xs) = let x' = show x
                                in do 
                                      hPutStrLn log ("[+] Path exists: " ++ x')
                                      putStrLn ("[+] " ++ x')
                                      printFoundURIs log xs

processLoop' :: Crawler -> Handle -> [URI] -> IO ()
processLoop' _ _ [] = return ()
processLoop' crawler log (x:xs) = do 
                                 putStrLn (show x)
                                 hPutStrLn log (show x)
                                 (c, u) <- performBruteforce crawler x
                                 let u' = filterVisited c u
                                     c' = foldr (flip markVisited) c u'
                                    in do 
                                          printFoundURIs log u'
                                          (c'', u'') <- processURI c' x
                                          processLoop' c'' log (xs ++ u' ++ u'')

processLoop :: Crawler -> Handle -> IO ()
processLoop crawler log = processLoop' crawler log ([baseURI crawler])
