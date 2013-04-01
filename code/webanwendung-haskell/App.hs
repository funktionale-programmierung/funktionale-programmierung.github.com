{-# LANGUAGE OverloadedStrings, FlexibleContexts, DoAndIfThenElse, GADTs,
             TypeFamilies, BangPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}
module App
    ( launchServer
    )
where

import Types

import Web.Scotty
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as J

import qualified Database.Persist as SQL
import qualified Database.Persist.MySQL as SQL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import Web.PathPieces (fromPathPiece)
import Data.Maybe (fromJust)

import Network.Wai.Middleware.RequestLogger

instance Parsable T.Text where parseParam = Right . TL.toStrict

data ContentType = CtHtml | CtJavaScript deriving (Show, Eq, Enum)

ctToLText :: ContentType -> TL.Text
ctToLText CtHtml = "text/html"
ctToLText CtJavaScript = "text/javascript"

mysqlInfo = SQL.defaultConnectInfo
            { SQL.connectDatabase = "blog"
            , SQL.connectPassword = ""
            , SQL.connectUser = "root"
            , SQL.connectHost = "127.0.0.1"
            , SQL.connectPort = 3306
            }

runDB x = liftIO $ do runResourceT $ SQL.withMySQLConn mysqlInfo $ SQL.runSqlConn x

launchServer port =
    do runResourceT $ SQL.withMySQLConn mysqlInfo $ SQL.runSqlConn $ SQL.runMigrationUnsafe migrateAll
       scotty port $ do
         middleware logStdoutDev -- just for debugging

         defineStatic "/" "static/index.html" CtHtml
         defineStatic "/jquery.min.js" "static/jquery.min.js" CtJavaScript
         defineStatic "/templates.js" "static/templates.js" CtJavaScript
         defineStatic "/soyutils.js" "static/soyutils.js" CtJavaScript
         defineStatic "/app.js" "static/app.js" CtJavaScript

         get "/news" $ do
             response <- runDB $ do newsEntries <- SQL.selectList [] [SQL.Desc NewsItemId]
                                    return newsEntries

             json response

         get "/comments/:id" $ \newsId -> do
             response <- runDB $ do comments <- SQL.selectList [NewsCommentNews SQL.==. ((fromJust $ fromPathPiece newsId) :: NewsItemId)]
                                                               [SQL.Desc NewsCommentId]
                                    return comments

             json response

         post "/news" $ do -- this should be password protected
              news <- parseNews
              runDB $ SQL.insert news
              json $ J.Bool True

         post "/comments" $ do
              comment <- parseComment
              runDB $ SQL.insert comment
              json $ J.Bool True

    where
      parseComment :: ActionM NewsComment
      parseComment =
          do comment <- jsonData
             return $ comment

      parseNews :: ActionM NewsItem
      parseNews =
          do news <- jsonData
             return $ news

      defineStatic path f ctype =
          get path $ do
            header "Content-Type" (TL.concat [ctToLText ctype, ";charset=utf-8;"])
            liftIO $ putStrLn $ "will show: " ++ (show f)
            file f
