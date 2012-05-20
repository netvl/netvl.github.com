{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Hakyll

ps ++> f = mapM_ (\p -> match p f) ps

main :: IO ()
main = hakyll $ do
    -- Favicon
    ["favicon.ico"]       
        ++> copy
    -- Images
    ["images/**"]         
        ++> copy
    -- Static files
    ["static/**"]         
        ++> copy
    -- Javascript files
    ["js/**"]             
        ++> copy
    -- CSS
    ["styles/*.css"]      
        ++> css
    -- Templates
    ["templates/*"]       
        ++> templates
    -- Posts
    ["posts/**"]          
        ++> posts
    -- Toplevel
    ["*.md", "*.html", "*.lhs"]
        ++> toplevel

    where
        css = route (setExtension "css") >> compile compressCssCompiler

        copy = route idRoute >> compile copyFileCompiler

        templates = compile templateCompiler

        posts = undefined

        toplevel = undefined

