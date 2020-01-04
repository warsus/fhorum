{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Model

import Prelude hiding ((++), filter, map)
import Data.Time
import qualified Data.Text as T

import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Control.Monad (forM_)
import Database.Persist.Postgresql
import Data.Tree
import Data.Maybe

import Import hiding ((.), forM_)
import Text.RawString.QQ

postPerPage :: Integer
postPerPage = 20

allPosts :: HandlerFor App [Entity Post]
allPosts = runDB $ selectList ([]::[Filter Post]) []
intervallPosts :: (PersistQueryRead (YesodPersistBackend site), YesodPersist site, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => p1 -> p2 -> HandlerFor site [Entity Post]
intervallPosts n m = runDB $ selectList ([]::[Filter Post]) []
pagePosts :: (PersistQueryRead (YesodPersistBackend site), YesodPersist site, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => p -> HandlerFor site [Entity Post]
pagePosts n = runDB $ selectList ([]::[Filter Post]) []

parents :: (PersistQueryRead (YesodPersistBackend site), YesodPersist site, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => Maybe (Key Post) -> HandlerFor site [Entity Post]
parents pid = runDB $ selectList ([PostParent ==. pid]) []
children :: (MonadResource m1, MonadReader env m1, HasPersistBackend env, Monad m2, PersistEntity record, PersistField a, BaseBackend env ~ SqlBackend) => a -> ConduitM () c m1 [m2 (Either Text record)]
children pid = rawQuery ("SELECT parent,created,title,body FROM \"Post\" WHERE node_path ~ '*." `T.append` ((T.pack.show.toPersistValue) pid) `T.append` ".*';") [] .| mapC (return.fromPersistValues) .| sinkList

recursive_children :: Text
recursive_children = T.pack [r|WITH RECURSIVE sub_posts(id, parent, created, "user", title, body) AS (
    SELECT * FROM post WHERE id = 1
  UNION ALL
    SELECT ps.id, ps.parent, ps.created, ps.user, ps.title, ps.body
    FROM sub_posts sps, post ps
    WHERE ps.parent = sps.id
)
SELECT *
FROM sub_posts;|]


children2 :: (MonadResource m1, MonadReader env m1, HasPersistBackend env, Monad m2, PersistEntity record, BaseBackend env ~ SqlBackend) => p -> ConduitM () c m1 [m2 (Either Text record)]
children2 pid = rawQuery recursive_children [] .| mapC (return.fromPersistValues) .| sinkList

isChild :: (Key Post,Post) -> (Key Post,Post) -> Bool
isChild (k1,p1) (k2,p2) = fromMaybe False $ (==) <$> postParent p2 <*> pure k1 

buildTree :: [(Key Post, Post)] -> Maybe (Tree (Key Post, Post))
buildTree (x:xs) = Just $ unfoldTree (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) x
buildTree _ = Nothing

-- buildTreeState :: State [x] (Tree x)
-- buildTreeState = do 
--   unfoldTree (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) x

--TODO:Optimize
buildForest :: [(Key Post, Post)] -> Maybe (Forest (Key Post, Post))
buildForest l@(x:xs) = Just $ unfoldForest (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) (filter (\(k,p) -> rootPost p || parentNotInList p ) l)
                       where rootPost p = postParent p == Nothing
                             parentNotInList p1 = isNothing (find (\(k,v) -> Just k == postParent p1) l)
buildForest _ = Nothing


renderPostLink :: ToValue a => (Route App -> a) -> (PostId, Post) -> Markup
renderPostLink url (k,v) = do
  (a ! (href.toValue.url.PostIdR) k $ toHtml $ postTitle v) 
  --toHtml $ postUser $ v
  toHtml $ " " ++ (show $  postCreated v)
  

renderTree' :: ToValue a => (Route App -> a) -> Tree (PostId, Post) -> Markup
renderTree' url (Node v []) = ul $ p $ renderPostLink url v
renderTree' url (Node v ts) = ul $ p $ renderPostLink url v >> forM_ ts (renderTree' url)

--TODO Fix Nothing case
renderTree :: ToValue a => (Route App -> a) -> Maybe (Tree (PostId, Post)) -> Markup
renderTree url (Just node)  = renderTree' url node
renderTree url Nothing = return ()

renderForest :: (Foldable t, ToValue a) => (Route App -> a) -> Maybe (t (Tree (PostId, Post))) -> Markup
renderForest url (Just xs) = forM_ xs (renderTree' url)
renderForest url Nothing = return ()

tree :: (MonadHandler m, HandlerSite m ~ App) => [Entity Post] -> m (Markup)
tree threads = do
  url <- getUrlRender
  return $ renderForest url (buildForest (map (\(Entity k v) -> (k,v)) threads))

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
        --h2id <- lift newIdent
        setTitle "forum homepage"
        toWidget $(widgetFile "forumpage")

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
    --liftIO $ print $ show $ toPersistValue id
    now <- liftIO getCurrentTime
    post <- runDB $ get404 id
    --tchilds <- runDB $ (children id)
    --let childs = rights tchilds
    url <- getUrlRender
    --threadsTree <- return $ renderForest url (buildForest threads)
    defaultLayout $ do
        -- h2id <- lift newIdent
        setTitle "forum homepage"
        toWidget $(widgetFile "post")

getPage :: Int -> Handler RepHtml
getPage n = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    threads <- intervallPosts 10 (n*10)
    threadsTree <- tree threads
    defaultLayout $ do
        -- h2id <- lift newIdent
        setTitle "forum homepage"
        toWidget $(widgetFile "forumpage")
