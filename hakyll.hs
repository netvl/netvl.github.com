{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc (ParserState, WriterOptions(..))           
import Data.Monoid
import Control.Arrow
import Hakyll
import Hakyll.Core.Util.Arrow
import Data.List.Split (splitOn)
import Data.List

ps ++> f = mapM_ (\p -> match p f) ps

parserState :: ParserState
parserState = defaultHakyllParserState

writerOptions :: WriterOptions 
writerOptions = defaultHakyllWriterOptions {
    writerHtml5 = True
}

recentPostNumber :: Int
recentPostNumber = 8

newestFirst :: [Page String] -> [Page String]
newestFirst = sortBy compareDates
    where
        pagePath = getField "path"
        pageOrd p = case getFieldMaybe "ord" p of
            Just ord -> ord
            Nothing  -> "0"

        compareDates p1 p2 =
            let (path1, ord1) = (pagePath p1, pageOrd p1)
                (path2, ord2) = (pagePath p2, pageOrd p2)
                f1@[y1, m1, d1, name1, o1] = splitOn "/" path1 ++ [ord1]
                f2@[y2, m2, d2, name2, o2] = splitOn "/" path2 ++ [ord2]
            in compare f1 f2


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
    -- Generate index file
    create "index.html" indexFile
    -- Generate posts list file
    create "posts.html" postsFile

    where
        css = route (setExtension "css") >> compile compressCssCompiler

        copy = route idRoute >> compile copyFileCompiler

        templates = compile templateCompiler

        posts = do
            route $ setExtension "html"
            compile $ pageCompilerWith parserState writerOptions
                >>> arr (copyBodyToField "content")
                >>> applyTemplateCompiler "templates/post.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        toplevel = do
            route $ setExtension "html"
            compile $ pageCompilerWithFields parserState writerOptions id postFields
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        postFields = smallRecentPostsList

        smallRecentPostsList =
            setFieldPageList (take recentPostNumber . newestFirst)
                "templates/post-item-small.html" "recentPosts" "posts/**"

        --- Index file definitions

        indexFile = constA mempty
            >>> indexFields
            >>> applyTemplateCompiler "templates/index.html"
            >>> relativizeUrlsCompiler

        indexFields = bigRecentPostsList

        bigRecentPostsList =
            setFieldPageList (take recentPostNumber . newestFirst)
                "templates/post.html" "posts" "posts/**"

        --- Posts file definition

        postsFile = constA mempty
            >>> allPostsFields
            >>> applyTemplateCompiler "templates/posts.html"
            >>> relativizeUrlsCompiler

        allPostsFields = fullPostsList

        fullPostsList =
            setFieldPageList newestFirst
                "templates/post-item.html" "posts" "posts/**"
