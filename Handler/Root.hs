{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Forum
import Model
import Database.Persist.Base

import Data.Monoid
import Data.Time
import Data.Either
import Data.List (find)
import qualified Data.Text as T

import Text.Blaze.Html5 
import Text.Blaze.Html5.Attributes
import Control.Monad (forM_)
import Control.Applicative
import Database.Persist.GenericSql.Raw
import Control.Monad.Loops
import Data.Tree
import Data.Maybe
import System.Locale
import Data.Time

postPerPage = 20

allPostsJoinUser :: Handler [((PostId,Post),Maybe User)]
allPostsJoinUser = do
  posts <- runDB $ selectList ([]::[Filter Post]) [] 0 0
                   >>= mapM (\(pid,p) -> do 
                               pu <- get (postUser p)
                               return ((pid,p),pu))
  return posts

allPosts :: Handler [(PostId,Post)]
allPosts = runDB $ selectList ([]::[Filter Post]) [] 0 0 
intervallPosts n m = runDB $ selectList ([]::[Filter Post]) [] n m
pagePosts n = runDB $ selectList ([]::[Filter Post]) [] postPerPage (n*postPerPage)

showPersist (PersistInt64 x) = show x

fromEither ((x,(Left a)):xs) = fromEither xs
fromEither ((x,(Right a)):xs) = (x,a) : fromEither xs                             
fromEither _ = []

toPost (x:xs) = (PostId x,fromPersistValues xs)
parents id = runDB $ selectList ([PostParentEq id]) [] 0 0
children id = do
  let stmt ="SELECT id,parent,created,user,title,body FROM \"Post\" WHERE node_path ~ '*." `T.append` ((T.pack.showPersist.toPersistValue) id) `T.append` ".*';"
  liftIO $ print stmt
  childs <-runDB $ withStmt stmt [] $ \pop -> (whileJust pop (return.toPost))
  let childPosts = fromEither childs
  liftIO $ print $ length childs 
  return (fromEither childs)

isChild :: (Key Post,Post) -> (Key Post,Post) -> Bool
isChild (k1,p1) (k2,p2) = fromMaybe False $ (==) <$> postParent p2 <*> pure k1 

buildTree (x:xs) = Just $ unfoldTree (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) x
buildTree _ = Nothing

buildTreeU (x:xs) = Just $ unfoldTree (\(p,u) -> ((p,u),filter (\(p1,u1) -> (isChild p p1)) xs)) x
buildTreeU _ = Nothing

-- buildTreeState :: State [x] (Tree x)
-- buildTreeState = do 
--   unfoldTree (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) x

--TODO:Optimize
buildForest l@(x:xs) = Just $ unfoldForest (\(k,p) -> ((k,p),filter (isChild (k,p)) xs)) (filter (\(k,p) -> rootPost p || parentNotInList p ) l)
                       where rootPost p = postParent p == Nothing
                             parentNotInList p1 = isNothing (find (\(k,v) -> Just k == postParent p1) l)
buildForest _ = Nothing

buildForestU l@(x:xs) = Just $ unfoldForest (\(p,u) -> ((p,u),filter (\(p1,u1) -> (isChild p p1)) xs)) (filter (\(p,u) -> rootPost (snd p) || parentNotInList (p,u) ) l)
                       where rootPost p = postParent p == Nothing
                             parentNotInList p1 = isNothing (find (\(p,u) -> Just (fst p) == postParent (snd (fst p1))) l)
buildForestU _ = Nothing

renderPostLink url (k,v) = do
  (a ! (href.toValue.url.PostIdR) k $ toHtml $ postTitle v) 
  toHtml $ show $ postUser v
  toHtml $ " " ++ (show $  postCreated v)

renderPostLinkU url ((k,v),u) = do
  (a ! (href.toValue.url.PostIdR) k $ toHtml $ postTitle v) 
  toHtml $ T.pack " - "
  toHtml $ case u of Just user -> userIdent user
                     Nothing -> ""
  toHtml $ T.pack ", "
  toHtml $ formatTime defaultTimeLocale rfc822DateFormat $ postCreated v

renderTree' url (Node v []) = ul $ p $ renderPostLink url v
renderTree' url (Node v ts) = ul $ p $ renderPostLink url v >> forM_ ts (renderTree' url)

renderTreeU' url (Node v []) = ul $ p $ renderPostLinkU url v
renderTreeU' url (Node v ts) = ul $ p $ renderPostLinkU url v >> forM_ ts (renderTreeU' url)


--TODO Fix Nothing case
renderTree url (Just node)  = renderTree' url node
renderTree url Nothing = return ()

renderTreeU url (Just node)  = renderTreeU' url node
renderTreeU url Nothing = return ()

renderForest url (Just xs) = forM_ xs (renderTree' url)
renderForest url Nothing = return ()

renderForestU url (Just xs) = forM_ xs (renderTreeU' url)
renderForestU url Nothing = return ()

tree threads = do
  url <- getUrlRender
  return $ renderTree url (buildTree threads)

treeU threads = do
  url <- getUrlRender
  return $ renderTreeU url (buildTreeU threads)

forest threads = do
  url <- getUrlRender
  return $ renderForest url (buildForest threads)

forestU threads = do
  url <- getUrlRender
  return $ renderForestU url (buildForestU threads)

getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    threads <- allPostsJoinUser
    threadsTree <- forestU threads
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "homepage")

getRootPostR :: Handler RepHtml
getRootPostR = do
  u <- requireAuth
  now <- liftIO getCurrentTime
  (res, form, enctype) <- runFormGet $ postForm Nothing Nothing now (fst u)
  defaultLayout $ do
                 h2id <- lift newIdent
                 addWidget $(widgetFile "post")

getPostIdR :: PostId -> Handler RepHtml
getPostIdR id = do
    let parent = id
    u <- requireAuth
    liftIO $ print $ show $ toPersistValue parent
    now <- liftIO getCurrentTime
    post <- runDB $ get404 parent
    (res, form, enctype) <- runFormPostNoNonce $ postForm Nothing (Just parent) now (fst u)
--    threads <- children parent
--    threadsTree <- tree (fromEither threads::[(PostId,Post)])
    let mpost = case res of 
                FormSuccess x -> Just x
                _ -> Nothing
    --tchilds <- runDB $ (children id)
    --let childs = rights tchilds
    url <- getUrlRender
    --threadsTree <- return $ renderForest url (buildForest threads)
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "postpage")

getPage :: Int -> Handler RepHtml
getPage n = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    threads <- intervallPosts 10 (n*10)
    threadsTree <- forest threads
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "homepage")

postForm mpost parent now user = fieldsToTable $ Post 
                            <$> pure parent
                            <*> pure now
                            <*> pure user
                            <*> stringField "Title" (fmap postTitle mpost)
                            <*> htmlField "Body" { ffsId = Just "body"
                                                    } (fmap postBody mpost)

postPostIdR :: PostId -> Handler RepHtml
postPostIdR parent = do
  u <- requireAuth
  now <- liftIO getCurrentTime
  (res, form, enctype) <- runFormPostNoNonce $ postForm Nothing (Just parent) now (fst u)
  let mpost = case res of 
                FormSuccess x -> Just x
                _ -> Nothing
  post <- runDB $ get404 parent
  defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "postpage")

postReplyR :: PostId -> Handler RepHtml
postReplyR id = do
  let parent = id
  u <- requireAuth
  now <- liftIO getCurrentTime
  (res,form,enctype) <- runFormPostNoNonce $ postForm Nothing (Just parent) now (fst u)
  let mpost = case res of 
                FormSuccess p -> Just p
                _ -> Nothing
  case res of 
    FormSuccess post -> do
                  pid <- runDB $ insert post
                  setMessage "Posted"
                  redirect RedirectTemporary $ PostIdR pid
    _ -> return ()
  post <- runDB $ get404 parent
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
                  addWidget $(widgetFile "rootpost")
                     
getChildrenR :: PostId -> Handler RepHtml
getChildrenR id = do
    mu <- maybeAuth
    now <- liftIO getCurrentTime
    --    testChildren id
    threads <- children id
    threadsTree <- tree threads
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "forum homepage"
        addWidget $(widgetFile "children")  

      
