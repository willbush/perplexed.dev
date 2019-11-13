{-# LANGUAGE OverloadedStrings #-}

import           Hakyll

main :: IO ()
main = hakyll $ do
  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("perplexed.dev" :: String)

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "script/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.org", "contact.org"]) $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  let postCtx = mkPostCtx tags

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   fmap demoteHeaders
      <$> pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  tagsRules tags $ \tag postPattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postPattern
      let ctx =
            constField "title" ("Posts tagged " <> tag)
              <> listField "posts" postCtx (pure posts)
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"   ctx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (pure posts)
              <> constField "title" "Archives"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  compileFeed "rss.xml" renderRss
  compileFeed "atom.xml" renderAtom

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (pure posts)
              <> constField "title" "Home"
              <> field "tagcloud" (\_ -> renderTagCloud 100.0 165.0 tags)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

-- | Make the post context from the given collection of tags.
mkPostCtx :: Tags -> Context String
mkPostCtx tags =
  mconcat [dateField "date" "%B %e, %Y", tagsField "tags" tags, defaultContext]

-- | Interesting thing of note is if defaultContext is m-appended after
-- bodyField "description", then the RSS/Atom feed description/summary gets
-- bloated with the content of the blog instead of using the description meta
-- data.
feedCtx :: Context String
feedCtx = defaultContext <> bodyField "description"

feedConfiguration :: FeedConfiguration
feedConfiguration  = FeedConfiguration
  { feedTitle       = "Perplexed Dev Blog"
  , feedDescription = "Personal website of Will Bush"
  , feedAuthorName  = "Will Bush"
  , feedAuthorEmail = "will.g.bush@gmail.com"
  , feedRoot        = "https://perplexed.dev"
  }

type FeedRenderer
  =  FeedConfiguration
  -> Context String
  -> [Item String]
  -> Compiler (Item String)

compileFeed :: Identifier -> FeedRenderer -> Rules ()
compileFeed feedName renderer =
  create [feedName] $ do
    route idRoute
    compile
      $   loadAllSnapshots "posts/*" "content"
      >>= fmap (take 10) . recentFirst
      >>= renderer feedConfiguration feedCtx
