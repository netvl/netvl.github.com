{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc (ParserState, WriterOptions(..))           
import Data.Monoid
import Control.Arrow
import Hakyll
import Hakyll.Core.Util.Arrow
import Data.List.Split (splitOn)
import Data.List
import System.IO.Unsafe

p :: (Show a) => a -> a
p x = unsafePerformIO $ print x >> return x

ps ++> f = mapM_ (\p -> match p f) ps

-- Custom parser state for page reader
parserState :: ParserState
parserState = defaultHakyllParserState

-- Custom writer options for page generator
writerOptions :: WriterOptions 
writerOptions = defaultHakyllWriterOptions {
    writerHtml5 = True
}

-- A number of recent posts to show
recentPostNumber :: Int
recentPostNumber = 8

-- Sorts post pages by their path and possibly ordering field in ascending order
oldestFirst :: [Page a] -> [Page a]
oldestFirst = sortBy compareDates
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

-- Sorts post pages by their path and possibly ordering field in descending order
newestFirst :: [Page a] -> [Page a]
newestFirst = reverse . oldestFirst

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

        --- Post files

        posts = do
            route $ setExtension "html"
            compile $ pageCompilerWith parserState writerOptions
                >>> arr (copyBodyToField "content")   -- Save body to metadata to extract it in other pages
                >>> applyTemplateCompiler "templates/post.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        --- Different top-level pages

        toplevel = do
            route $ setExtension "html"
            compile $ pageCompilerWithFields parserState writerOptions id toplevelFields
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        toplevelFields = smallRecentPostsList

        --- Index file definitions

        indexFile = constA mempty
            >>> indexFields
            >>> applyTemplateCompiler "templates/index.html"
            >>> relativizeUrlsCompiler

        indexFields = smallRecentPostsList 
            >>> bigRecentPostsList

        --- Posts file definition

        postsFile = constA mempty
            >>> postsFields
            >>> applyTemplateCompiler "templates/posts.html"
            >>> relativizeUrlsCompiler

        postsFields = fullPostsList

        --- Different post lists

        -- A list of all posts with larger links and with dates
        fullPostsList =
            setFieldPageList newestFirst
                "templates/post-item.html" "posts" "posts/**"

        -- A list of post bodies of recentPostNumber length
        bigRecentPostsList =
            setFieldPageList (take recentPostNumber . newestFirst)
                "templates/post.html" "posts" "posts/**"

        -- A list of posts with smaller links ans without dates
        smallRecentPostsList =
            setFieldPageList (take recentPostNumber . newestFirst)
                "templates/post-item-small.html" "recentPosts" "posts/**"

