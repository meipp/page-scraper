module Main where

import Lib

import Text.HTML.Scalpel
import Control.Applicative ((<*>))
import System.Environment

main :: IO ()
main = do
    args <- getArgs 
    case args of
        (url:objectSelector:newPageSelector:xs) -> crawlPages
                url
                objectSelector
                newPageSelector
                (attributes (takeTriplets xs))
                -- (text (tagSelector "h2"))
            >>= print
        _ -> error "Wrong number of arguments"

takeTriplets :: [a] -> [(a, a, a)]
takeTriplets (x1:x2:x3:xs) = (x1, x2, x3) : takeTriplets xs
takeTriplets _          = []

-- crawlPages
--     "https://www.amazon.com/s?k=whamisa&ref=nb_sb_noss"
--     -- "div.product"
--     "div.sg-col-inner"
--     "a.next"
--     -- (text (selector "p.product-title"))
--     -- ((,) <$> attr "href" (selector "a.woocommerce-LoopProduct-link") <*> attr "src" (selector "img.attachment-woocommerce_thumbnail"))
--     ((,) <$> text (selector "span.a-text-normal") <*> attr "src" (selector "img"))
-- >>= print