{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import System.Process (rawSystem)


type Title = String

main :: IO ()
main = do
    mb <- scrapeURL "https://www.spiegel.de" headlines
    let (ts, hs) = unzip $ fromMaybe [] mb
        -- ts = numberedList $ map fst ls
        -- hs = map snd ls

    sequence $ map putStrLn (numberedList ts)

    l <- getLine
    let i = read l :: Int
    openURL $ hs !! i

    return ()

openURL url = rawSystem "sh" ["-c", "xdg-open " ++  url]

headlines :: Scraper String [(Title, URL)]
headlines =
  chroots ("div" @: ["data-area" @= "article_teaser"] // "article") $ do
    t <- attr "title" "a"
    href <- attr "href" "a"
    return (t, href)

numberedList :: [String] -> [String]
numberedList xs = zipWith (++) (map (trailSp . show) [0..]) xs
  where
    trailSp = (++ " ")
