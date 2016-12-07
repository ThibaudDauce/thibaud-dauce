--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (empty, (<$>))
import           Control.Monad (liftM)
import           Data.Monoid   (mappend)
import           Hakyll
import           System.FilePath
import           Data.List

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    imageProcessor "images/*" [ ("thumbnail" , Just 310) ]

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "videos/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/font-awesome/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "talks/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "sass/app.scss" $ do
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


    match "talks.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Talks"                `mappend`
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


--------------------------------------------------------------------------------
-- Image processing
--------------------------------------------------------------------------------

type ImageProcessing = [(String, Maybe Int)]

-- | Process image files according to a specification.
--
-- The 'Rules' and 'Context'  returned can be used to output and
imageProcessor :: Pattern -- ^ Images to process.
               -> ImageProcessing -- ^ Processing instructions.
               -> Rules ()
imageProcessor pat procs = imageRules pat procs

-- | Generate 'Rules' to process images.
imageRules :: Pattern -- ^ Pattern to identify images.
           -> ImageProcessing -- ^ Versions to generate.
           -> Rules ()
imageRules pat procs = match pat $ do
  sequence_ $ map processImage procs
  where
    imageRoute name ident = let path = toFilePath ident
                                base = takeFileName path
                                name' = name ++ "-" ++ base
                            in replaceFileName path name'
    -- Process an image with no instructions.
    processImage (name, Nothing) = version name $ do
        route $ customRoute (imageRoute name)
        compile $ copyFileCompiler
    -- Process with scale and crop instructions.
    processImage (name, Just width) = version name $ do
        route $ customRoute (imageRoute name)
        let cmd = "convert"
        let args = [ "-"
                   , "-background"
                   , "#F6F8F9"
                   , "-transparent"
                   , "white"
                   , "-gravity"
                   , "center"
                   , "-resize"
                   , show width ++ "x" ++ show width
                   , "-extent"
                   , show width ++ "x" ++ show width
                   , "-"
                   ]
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)
