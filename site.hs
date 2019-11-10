{-# LANGUAGE OverloadedStrings #-}

import           Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
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

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (pure posts)
              <> constField "title" "Home"
              <> field "tagcloud" (\_ -> renderTagCloud 85.0 165.0 tags)
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
