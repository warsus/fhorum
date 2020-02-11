{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
-- | Migration from freiwirtschaft forum
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute, toSqlKey)


import qualified Data.EDN as EDN
import Data.EDN (Tagged(..), Value(..))
import Data.EDN.Class.Parser (Parser)
import qualified Data.Text.IO as Text
import Data.Text as T
import Text.Blaze.Html5 hiding (map, main)
import Text.Blaze.Html5.Attributes
import Text.Parsec (parse)

rightToMaybe (Right a) = Just a
rightToMaybe _ = Nothing

main2 :: IO ()
main2 = do
  edn <- Text.readFile "ham2.edn"
  let postLines = T.lines edn
  mapM_ Text.putStr postLines
  --either error print $ EDN.parseText "ham2.edn" edn

insertUser :: MonadIO m => (Text, Maybe Text) -> ReaderT SqlBackend m ()
insertUser (ident, passwd) =
  insert_ $ User ident passwd

insertPost :: MonadIO m => UTCTime -> (Text, Text, Int64) -> ReaderT SqlBackend m ()
insertPost time (title, body, user) = insert_ $ Post Nothing time (toSqlKey user) title body

tupleToPost t = Post t

users :: [(Text, Maybe Text)]
users =
  [ ("bob",     Just "s3cr3t")
  , ("charles", Just "passw0rd")
  ]

--NoTag (Map (fromList [(NoTag (Keyword "beitrag/antworten"),
-- NoTag (List [NoTag (Integer 4075),NoTag (Integer 2)])),
--(Keyword (NoTag "beitrag/datum"),
--Tagged "" "inst" (String "2000-08-15T13:40:40.000-00:00")),
--(NoTag (Keyword "beitrag/id"),NoTag (Integer 1)),
--(NoTag (Keyword "beitrag/text"),NoTag (String "<div class=\"beitragstext_1\">Da habe ich es doch noch geschafft gerade vor der Abfahrt noch den ersten Beitrag zu posten<br />\nUnd er ist ganz kurz. \"Gutes Gelingen, w\252nscht Hans aus Canada</div>")),
--(NoTag (Keyword "beitrag/titel"),NoTag (String "Gl\252ckwunsch")),
--(NoTag (Keyword "beitrag/user"),NoTag (String "Hans\160Eisenkolb"))]))
--
data EdnPost = EdnPost { ednPostParent :: Maybe Text, ednPostCreated :: UTCTime, ednPostUser :: Text, ednPostTitle :: Text, ednPostBody :: Text
             } deriving (Show, Eq)

instance {-# OVERLAPPING #-} EDN.FromEDN EdnPost where
  parseEDN = EDN.withNoTag . EDN.withMap $ \m -> do
    parent <- return Nothing
    created <- EDN.mapGetKeyword "beitrag/datum" m
    user <- EDN.mapGetKeyword "beitrag/user" m
    titel <- EDN.mapGetKeyword "beitrag/titel" m
    body <- EDN.mapGetKeyword "beitrag/text" m
    return (EdnPost parent created user titel body) -- <$> pure parent <*> pure created <*> pure user <*> pure titel <*> pure (body::Text))

parseUser :: EDN.EDNMap -> Parser (Maybe (Key User))
parseUser m = EDN.mapGetKeyword "beitrag/user" m >>= fn
  where fn :: Text -> Parser (Maybe (Key User))
        fn t = (return . rightToMaybe . keyFromValues . return . toPersistValue) t


postFromEdn :: EDN.TaggedValue -> Maybe (Text, Text, Int64)
--postFromEdn (NoTag (NoTag (Keyword "beitrag/id"), (NoTag (Integer i))))) = Just ("", "", i)
postFromEdn (NoTag (Map m)) = Just ("", "", 1)
postFromEdn _ = Nothing




insertEdnPost :: MonadIO m => EdnPost -> ReaderT SqlBackend m ()
insertEdnPost (EdnPost parent created user title body) = do
  userdb <- selectList [UserIdent ==. user ] []
  userKey <- case userdb of [] -> insert $ User user (Just "")
                            a:_ -> return (entityKey a)

  insert $ Post Nothing created userKey title body
  return ()

main :: IO ()
main = do
  now <- getCurrentTime
  settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv

  --either error print $ edn

  post <- Text.readFile "ham.edn"
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    let lines = T.lines post
    (mapM_ (insertPost.parsePost) lines)
  where parsePost ::Text -> Maybe EdnPost
        parsePost post = EDN.parseText "ham.edn" post >>= EDN.fromEDN
        --insertPost :: Maybe EdnPost -> a
        insertPost (Just post) = insertEdnPost post >>= const (return ())
        insertPost _ = return ()
