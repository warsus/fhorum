{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Forum
import Model
import Database.Persist.Base

import Data.Time
import Data.Either
import Data.List 
import qualified Data.Text as T

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Control.Monad (forM_)
import Control.Applicative
import Database.Persist.GenericSql.Raw
import Control.Monad.Loops
import Data.Tree
import qualified Data.Text as T
import Data.Maybe

postPerPage = 20

allPosts = runDB $ selectList ([]::[Filter Post]) [] 0 0
intervallPosts n m = runDB $ selectList ([]::[Filter Post]) [] n m
pagePosts n = runDB $ selectList ([]::[Filter Post]) [] postPerPage (n*postPerPage)

parents id = runDB $ selectList ([PostParentEq id]) [] 0 0
children id = withStmt ("SELECT parent,created,title,body FROM \"Post\" WHERE node_path ~ '*." `T.append` ((T.pack.show.toPersistValue) id) `T.append` ".*';") [] $ \pop -> (whileJust pop (return.fromPersistValues))

isChild :: (Key Post,Post) -> (Key Post,Post) -> Bool
isChild (k1,p1) (k2,p2) = fromMaybe False $ (==) <$> postParent p2 <*> pure k1 

buildTree (x:xs) = Just $ unfoldTree (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) x
buildTree _ = Nothing

-- buildTreeState :: State [x] (Tree x)
-- buildTreeState = do 
--   unfoldTree (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) x

--TODO:Optimize
buildForest l@(x:xs) = Just $ unfoldForest (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) (filter (\(k,p) -> rootPost p || parentNotInList p ) l)
                       where rootPost p = postParent p == Nothing
                             parentNotInList p1 = isNothing (find (\(k,v) -> Just k == postParent p1) l)
buildForest _ = Nothing


renderPostLink url (k,v) = do
  (a ! (href.toValue.url.PostIdR) k $ toHtml $ postTitle v) 
  toHtml $ fromMaybe "Not" $ fmap show $ postUser $  v
  toHtml $ " " ++ (show $  postCreated v)
  

renderTree' url (Node v []) = ul $ p $ renderPostLink url v
renderTree' url (Node v ts) = ul $ p $ renderPostLink url v >> forM_ ts (renderTree' url)

--TODO Fix Nothing case
renderTree url (Just node)  = renderTree' url node
renderTree url Nothing = return ()

renderForest url (Just xs) = forM_ xs (renderTree' url)
renderForest url Nothing = return ()

tree threads = do
  url <- getUrlRender
  return $ renderForest url (buildForest threads)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Forum.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    threads <- allPosts
    threadsTree <- tree threads
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "homepage")

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Forum.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
-- Shows Parent Post Children Replyform?
getPostIdR :: PostId -> Handler RepHtml
getPostIdR id = do
    mu <- maybeAuth
    liftIO $ print $ show $ toPersistValue id
    now <- liftIO getCurrentTime
    post <- runDB $ get404 id
    --tchilds <- runDB $ (children id)
    --let childs = rights tchilds
    url <- getUrlRender
    --threadsTree <- return $ renderForest url (buildForest threads)
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "post")

getPage :: Int -> Handler RepHtml
getPage n = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    threads <- intervallPosts 10 (n*10)
    threadsTree <- tree threads
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "homepage")
