{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

ps ++> f = mapM_ (\p -> match p f) ps

main :: IO ()
main = hakyll $ do
    -- Favicon
    ["favicon.ico"] ++> copy
    -- Images
    ["images/**"]   ++> copy
    -- Static files
    ["static/**"]   ++> copy
    -- Javascript files
    ["js/**"]       ++> copy
    -- CSS
    ["styles/*.css"]   ++> css

    where
        css = undefined        
        copy = undefined