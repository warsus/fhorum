{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.RootPost where

import Forum
import Control.Applicative
import Yesod.Form.Nic
import Data.Text (Text)
import Data.Time
import Model

postForm mpost parent now user = fieldsToTable $ Post 
                            <$> pure Nothing 
                            <*> pure now
                            <*> pure (Just user)
                            <*> stringField "Title" (fmap postTitle mpost)
                            <*> htmlField "Body" { ffsId = Just "body"
                                                    } (fmap postBody mpost)

-- postFormlet mpost = fieldsToTable $ PostTemp
--     <$> stringField "Title" (fmap title mpost)
--     <*>	nicHtmlField "Body"
--     	    { ffsId = Just "body"
-- 	    } (fmap body mpost)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Forum.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootPostR :: Handler RepHtml
getRootPostR = do
    u <- requireAuth
    now <- liftIO getCurrentTime
    (res, form, enctype) <- runFormPostNoNonce $ postForm Nothing Nothing now (fst u)
    let mpost = case res of
                    FormSuccess x -> Just x
                    _ -> Nothing
    threads <- runDB $ selectList [PostTitleEq "2"] [] 0 0
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "postpage")

postRootPostR :: Handler RepHtml
postRootPostR = do
  u <- requireAuth
  now <- liftIO getCurrentTime
  (res,form,enctype) <- runFormPostNoNonce $ postForm Nothing Nothing now (fst u)
  let mpost = case res of 
                FormSuccess p -> Just p
                _ -> Nothing
  case res of 
    FormSuccess post -> do
                  pid <- runDB $ insert post
                  setMessage "Posted"
                  redirect RedirectTemporary $ PostIdR pid
    _ -> return ()
  
  defaultLayout $ do                     
                  h2id <- lift newIdent
                  setTitle "forum homepage"
                  addWidget $(widgetFile "postpage")
                     
-- -- This is a handler function for the GET request method on the RootR
-- -- resource pattern. All of your resource patterns are defined in
-- -- Forum.hs; look for the line beginning with mkYesodData.
-- --
-- -- The majority of the code you will write in Yesod lives in these handler
-- -- functions. You can spread them across multiple files if you are so
-- -- inclined, or create a single monolithic file.
-- postRootPostR :: Handler RepHtml
-- postRootPostR = do
--     --_ <- runDB $ insert (Post ("2"::Text) ("3"::Text)) 
--     mu <- maybeAuth
--     let mpost = Nothing
--     threads <- liftIO $ query >>= \x -> return (map (show.head) x)
--     defaultLayout $ do
--         h2id <- lift newIdent
--         setTitle "forum homepage"
--         addWidget $(widgetFile "postpage")
