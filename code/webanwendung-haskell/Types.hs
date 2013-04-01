{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}

module Types where

import Database.Persist
import Database.Persist.TH

import Data.Aeson

import qualified Data.Text as T

import Control.Applicative
import Control.Monad

import Web.PathPieces (fromPathPiece)
import Data.Maybe (fromJust)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
NewsItem
    title T.Text
    content T.Text
    tags [T.Text]
    author T.Text
    deriving Show Eq

NewsComment
    author T.Text
    comment T.Text
    news NewsItemId
    deriving Show Eq
|]

instance ToJSON (Entity NewsItem) where
    toJSON (Entity nid (NewsItem title content tags author)) =
        object
        [ "id" .= nid
        , "title" .= title
        , "content" .= content
        , "tags" .= tags
        , "author" .= author
        ]

instance FromJSON NewsItem where
    parseJSON (Object v) =
        NewsItem <$> v .: "title"
                 <*> v .: "content"
                 <*> v .: "tags"
                 <*> v .: "author"
    parseJSON _ = mzero

instance ToJSON (Entity NewsComment) where
    toJSON (Entity cid (NewsComment author comment news)) =
        object
        [ "id" .= cid
        , "author" .= author
        , "comment" .= comment
        , "news" .= news
        ]

parseNewsId :: T.Text -> NewsItemId
parseNewsId =
    fromJust . fromPathPiece

mkNewsComment author comment newsId = NewsComment author comment (parseNewsId newsId)

instance FromJSON NewsComment where
    parseJSON (Object v) =
        mkNewsComment <$> v .: "author"
                      <*> v .: "comment"
                      <*> v .: "news"
    parseJSON _ = mzero
