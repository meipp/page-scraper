module Lib (crawlPages, selector, attributes) where

import Text.HTML.Scalpel
import Control.Applicative ((<|>))
import Data.List.Extra (split)
import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class (liftIO)
import GHC.Base (Applicative(liftA2))
import Data.List (isPrefixOf)
import Data.List (intercalate)

crawlPages :: URL -> String -> String -> Scraper String a -> IO (Maybe [a])
crawlPages = crawlPages'

crawlPages' :: (IsSelector sel1, IsSelector sel2)  => URL -> sel1 -> sel2 -> Scraper String a -> IO (Maybe [a])
crawlPages' startURL select nextPage extractor = do
    -- scrapeURL startURL (chroots (selector select) extractor)
    rawCrawlPages startURL (selector select) (selector nextPage) extractor

-- rawCrawlPages :: URL -> Selector -> Selector -> Scraper String a -> IO (Maybe ([a], Maybe URL))
-- rawCrawlPages url select nextPage extractor = do
--     putStrLn url
--     scrapeURL url ((,) <$> chroots select extractor <*> fmap listToMaybe (attrs "href" nextPage))

rawCrawlPage :: URL -> Selector -> Selector -> Scraper String a -> IO (Maybe ([a], Maybe URL))
rawCrawlPage url select nextPage extractor = do
    putStrLn url
    -- scrapeURLWithConfig Config{decoder=utf8Decoder, manager=Nothing} url scraper
    scrapeURL url scraper
        where
            scraper = do
                elements <- chroots select extractor
                nextURL <- listToMaybe <$> attrs "href" nextPage
                return (elements, nextURL)

rawCrawlPages :: URL -> Selector -> Selector -> Scraper String a -> IO (Maybe [a])
rawCrawlPages url select nextPage extractor = do
    crawled <- rawCrawlPage url select nextPage extractor
    case crawled of
      Nothing            -> return Nothing
      Just (xs, Nothing) -> return $ Just xs
      Just (xs, Just u)  -> fmap (fmap (xs ++)) (rawCrawlPages u' select nextPage extractor)
        where
            u' = absoluteURL url u

-- d: displayName
-- s: selector
-- a: attributeName
attribute :: IsSelector sel => String -> sel -> String -> Scraper String (String, String)
attribute d s "_text" = (,) d <$> text (selector s)
attribute d s "_html" = (,) d <$> html (selector s)
attribute d s a       = (,) d <$> attr a (selector s)

attributes :: [(String, String, String)] -> Scraper String [(String, String)]
attributes []           = return []
attributes ((d,s,a):xs) = (:) <$> attribute d s a <*> attributes xs

class IsSelector a where
    selector :: a -> Selector

instance IsSelector Selector where
    selector = id

instance IsSelector AttributePredicate where
    selector p = AnyTag @: [p]

instance IsSelector [AttributePredicate] where
    selector ps = AnyTag @: ps

instance IsSelector String where
    selector s = foldl (//) anySelector (map simpleSelector (words s))
        where
            simpleSelector s = case splitOn '.' s of
                []            -> anySelector
                ("":classes)  -> AnyTag @: map hasClass classes
                (tag:classes) -> TagString tag @: map hasClass classes

absoluteURL :: URL -> URL -> URL
absoluteURL base relative
    | not ("https://" `isPrefixOf` base) && not ("http://" `isPrefixOf` base) = error $ "Bad URLs: " ++ show (base, relative)
    | "https://" `isPrefixOf` relative || "http://" `isPrefixOf` relative = relative
    | otherwise = intercalate "/" (take 3 (splitOn '/' base)) ++ "/" ++ relative

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = split (== d)