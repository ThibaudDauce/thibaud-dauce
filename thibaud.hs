--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (liftM)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "videos/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/font-awesome/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/thibaud.scss" $ do
        route   $ setExtension "css"
        compile $ getResourceFilePath
            >>= \fp -> unixFilter "sassc" [fp] ""
            >>= makeItem
            >>= return . fmap compressCss

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
            >>= relativizeUrls

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    createFeed "feed.xml" renderRss
    createFeed "atom.xml" renderAtom

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext

feedCtx :: Context String
feedCtx =
    postCtx `mappend`
    bodyField "description"

type RenderingFunction = FeedConfiguration
           -> Context String
           -> [Item String]
           -> Compiler (Item String)

createFeed :: Identifier -> RenderingFunction -> Rules ()
createFeed name renderingFunction = create [name] $ do
      route idRoute
      compile $ do
          posts <- fmap (take 10) . recentFirst =<<
              loadAllSnapshots "posts/*" "content"
          renderingFunction myFeedConfiguration feedCtx posts


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Thibaud Dauce's blog"
    , feedDescription = "All Thibaud Dauce's articles"
    , feedAuthorName  = "Thibaud Dauce"
    , feedAuthorEmail = "thibaud@dauce.fr"
    , feedRoot        = "https://thibaud.dauce.fr"
    }
