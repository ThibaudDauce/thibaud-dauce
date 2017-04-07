--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty, (<$>))
import Control.Monad (liftM)
import Data.List
import Data.Monoid (mappend)
import Data.Time (TimeLocale(..), utc)
import Hakyll
import System.FilePath

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "images/**" $ do
      route idRoute
      compile copyFileCompiler
    imageProcessor "images/**.png" [("thumbnail", Just 260)]
    imageProcessor "images/**.jpg" [("thumbnail", Just 260)]
    match "files/**/*" $ do
      route idRoute
      compile copyFileCompiler
    match "videos/*" $ do
      route idRoute
      compile copyFileCompiler
    match "js/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/font-awesome/fonts/*" $ do
      route idRoute
      compile copyFileCompiler
    match "fonts/**" $ do
      route idRoute
      compile copyFileCompiler
    match "talks/*.md" $ do
      route idRoute
      compile $ pandocCompiler >>= relativizeUrls
    match "talks/**" $ do
      route idRoute
      compile copyFileCompiler
    match "sass/app.sass" $ do
      route $ setExtension "css"
      compile $
        getResourceFilePath >>= \fp ->
          fmap (fmap compressCss) (unixFilter "sassc" [fp] "" >>= makeItem)
    match "posts/*.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= saveSnapshot "content" >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $
        do let indexCtx =
                 constField "title" "Home" `mappend` constField "home" "true" `mappend`
                 defaultContext
           getResourceBody >>= applyAsTemplate indexCtx >>=
             loadAndApplyTemplate "templates/default.html" indexCtx >>=
             relativizeUrls
           >>= relativizeUrls
    match "blog.html" $ do
      route idRoute
      compile $
        do posts <- recentFirst =<< loadAll "posts/*"
           let (recents, olds) = splitAt 10 posts
           let indexCtx =
                 listField "recents" postCtx (return recents) `mappend`
                 listField "olds" postCtx (return olds) `mappend`
                 constField "title" "Blog" `mappend`
                 constField "blog" "true" `mappend`
                 defaultContext
           getResourceBody >>= applyAsTemplate indexCtx >>=
             loadAndApplyTemplate "templates/default.html" indexCtx >>=
             relativizeUrls
           >>= relativizeUrls
    match "talks.html" $ do
      route idRoute
      compile $
        do talks <- recentFirst =<< loadAll "talks/*.md"
           let indexCtx =
                 listField "talks" talkCtx (return talks) `mappend`
                 constField "title" "Talks" `mappend`
                 defaultContext
           getResourceBody >>= applyAsTemplate indexCtx >>=
             loadAndApplyTemplate "templates/default.html" indexCtx >>=
             relativizeUrls
           >>= relativizeUrls
    match "hiking.html" $ do
      route idRoute
      compile copyFileCompiler
    match "templates/*" $ compile templateCompiler
    match "icons/*" $ compile templateCompiler
    createFeed "feed.xml" renderRss
    createFeed "atom.xml" renderAtom

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend` teaserField "teaser" "content" `mappend`
  constField "blog" "true" `mappend`
  constField "post" "true" `mappend`
  defaultContext

talkCtx :: Context String
talkCtx = dateFieldWith frTimeLocale "date" "%e %B %Y" `mappend` defaultContext

feedCtx :: Context String
feedCtx = postCtx `mappend` bodyField "description"

type RenderingFunction = FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)

createFeed :: Identifier -> RenderingFunction -> Rules ()
createFeed name renderingFunction =
  create [name] $ do
    route idRoute
    compile $ do
      posts <-
        fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
      renderingFunction myFeedConfiguration feedCtx posts

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
  { feedTitle = "Thibaud Dauce's blog"
  , feedDescription = "All Thibaud Dauce's articles"
  , feedAuthorName = "Thibaud Dauce"
  , feedAuthorEmail = "thibaud@dauce.fr"
  , feedRoot = "https://thibaud.dauce.fr"
  }

--------------------------------------------------------------------------------
-- Image processing
--------------------------------------------------------------------------------
type ImageProcessing = [(String, Maybe Int)]

-- | Process image files according to a specification.
--
-- The 'Rules' and 'Context'  returned can be used to output and
imageProcessor
  :: Pattern -- ^ Images to process.
  -> ImageProcessing -- ^ Processing instructions.
  -> Rules ()
imageProcessor = imageRules

-- | Generate 'Rules' to process images.
imageRules
  :: Pattern -- ^ Pattern to identify images.
  -> ImageProcessing -- ^ Versions to generate.
  -> Rules ()
imageRules pat procs = match pat $ mapM_ processImage procs
  where
    imageRoute name ident =
      let path = toFilePath ident
          base = takeFileName path
          name' = name ++ "-" ++ base
      in replaceFileName path name'
    -- Process an image with no instructions.
    processImage (name, Nothing) =
      version name $ do
        route $ customRoute (imageRoute name)
        compile copyFileCompiler
    -- Process with scale and crop instructions.
    processImage (name, Just width) =
      version name $ do
        route $ customRoute (imageRoute name)
        let cmd = "convert"
        let args =
              [ "-"
              , "-background"
              , "white"
              , "-transparent"
              , "white"
              , "-gravity"
              , "center"
              , "-resize"
              , show width ++ "x" ++ show width
              , "-extent"
              , "300x260"
              , "-"
              ]
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)

frTimeLocale :: TimeLocale
frTimeLocale =
  TimeLocale
  { wDays =
      [ ("dimanche", "dim")
      , ("lundi", "lun")
      , ("mardi", "mar")
      , ("mercredi", "mer")
      , ("jeudi", "jeu")
      , ("vendredi", "ven")
      , ("samedi", "sam")
      ]
  , months =
      [ ("janvier", "jan")
      , ("février", "fev")
      , ("mars", "mar")
      , ("avril", "avr")
      , ("mai", "mai")
      , ("juin", "juin")
      , ("juillet", "juil")
      , ("août", "août")
      , ("septembre", "sep")
      , ("octobre", "oct")
      , ("novembre", "nov")
      , ("décembre", "dec")
      ]
  , amPm = (" du matin", " de l'après-midi")
  , dateTimeFmt = "%a %e %b %Y, %H:%M:%S %Z"
  , dateFmt = "%d-%m-%Y"
  , timeFmt = "%H:%M:%S"
  , time12Fmt = "%I:%M:%S %p"
  , knownTimeZones = [utc]
  }
