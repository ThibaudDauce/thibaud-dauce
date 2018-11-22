--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Time (TimeLocale(..), utc)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route idRoute
        compile copyFileCompiler
    match "videos/**" $ do
        route idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let (recents, olds) = splitAt 10 posts
            let indexCtx = mconcat
                    [ listField "recents" postCtx (return recents)
                    , listField "olds" postCtx (return olds)
                    , constField "blog" "true"
                    , constField "subtitle" "Thibaud's blog"
                    , constField "title" "My latest posts"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    
    match "talks/*.md" $ do
        route idRoute
        compile $ pandocCompiler >>= relativizeUrls
    match "talks/**" $ do
        route idRoute
        compile copyFileCompiler

    match "talks.html" $ do
        route idRoute
        compile $ do
            talks <- recentFirst =<< loadAll "talks/*.md"
            let indexCtx = mconcat
                    [ listField "talks" talkCtx (return talks)
                    , constField "talks" "true"
                    , constField "subtitle" "Thibaud's blog"
                    , constField "title" "My latest talks"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
           
    match "traces/*.gpx" $ do
        route idRoute
        compile copyFileCompiler

    match "hiking.html" $ do
        route idRoute
        compile $ do
            let indexCtx = mconcat
                    [ constField "hiking" "true"
                    , constField "subtitle" "Thibaud's blog"
                    , constField "title" "My latest hikes"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "about.html" $ do
        route idRoute
        compile $ do
            let indexCtx = mconcat
                    [ constField "about" "true"
                    , constField "subtitle" "Thibaud's blog"
                    , constField "title" "About me"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    createFeed "feed.xml" renderRss
    createFeed "atom.xml" renderAtom

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat 
    [ dateField "date" "%B %e, %Y"
    , teaserField "teaser" "content"
    , constField "blog" "true"
    , constField "subtitle" "Thibaud's blog"
    , defaultContext
    ]

talkCtx :: Context String
talkCtx = mconcat 
    [ dateFieldWith frTimeLocale "date" "%e %B %Y"
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat 
    [ bodyField "description"
    , postCtx
    ]

type RenderingFunction = FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)

createFeed :: Identifier -> RenderingFunction -> Rules ()
createFeed name renderingFunction =
    create [name] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
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
