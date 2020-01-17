{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

postForm :: Maybe Post -> UTCTime -> (Key User) -> Maybe (Key Post) -> Html -> MForm Handler (FormResult Post, Widget)
postForm mpost now user parent =  renderDivs $ Post
                            <$> pure parent
                            <*> pure now
                            <*> pure user
                            <*> areq textField "Title" (fmap postTitle mpost)
                            <*> areq htmlField "Body" (fmap postBody mpost)
