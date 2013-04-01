---
layout: post
description: Moderne Webanwendungen mit Haskell
title: "Moderne Webanwendungen mit Haskell"
author: alexander-thiemann
tags: ["web", "Haskell", "JavaScript", "SOY", "HTTP", "PHP"]
---
Moderne Webanwendungen mit Haskell

Derzeit sind ein Großteil aller Webanwendungen in *PHP* geschrieben. Die Gründe dafür liegen auf der Hand: Die Entwicklung geht meist sehr schnell,
*PHP* ist einfach zu erlernen und fast alle Webhoster haben mittlerweile Webserver mit *PHP*-Unterstützung installiert. Allerdings bringt die Verwendung
von *PHP* auch einige Probleme mit sich. Damit eine *PHP*-Anwendung gut skaliert, sind viele aufwendige Optimierungen notwendig (siehe zB *HipHop* von Facebook).
Außerdem ist *PHP* eine dynamische Sprache, und damit ist die Validierung und das Escapen von Ausgaben dem Programmierer selbst überlassen (SQL-Injections,
XSS, ... wird nicht „by design“ verhindert). Deshalb möchte ich an einem kleinen Beispiel erläutern, wie man mit *Haskell* relativ einfach eine performante,
sichere und moderne Webanwendung schreibt. Hierzu werde ich ein einfaches News-System implementieren.

Um dem Artikel gut folgen zu können sind Grundlagen zu *JavaScript*, *HTML*, *HTTP* und *Haskell* hilfreich.

<!-- more start -->

Es empfiehlt sich in dank breiter *AJAX*-Unterstützung der Browser sowohl die Views als auch die Controller clientseitig zu Implementieren. Daher müssen wir
in *Haskell* nur das Modell, dh. eine Komponente entwickeln die Daten akzeptiert und ausgibt (über eine *REST-API*). Für die Views verwenden wir die funktionale
(Google) Soy-Templates Sprache, diese wird dann nach *JavaScript* kompiliert sodass wir unsere Views mit unserer *JavaScript*-Controller Logik ansteuern können.

Beginnen wir nun mit der *REST-API*, die in Haskell geschrieben wird. Als Web-framework verwenden wir *scotty*, als Datenbankabstraktionsschicht verwenden wir
*persistent(-mysql)*. Als Protokoll verwenden wir *JSON*. Diese Pakete sollten in den entsprechenden Versionen installiert sein (siehe *cabal* Datei). Definieren
wir zunächst unsere Typen:

{% highlight haskell %}
-- Datei Types.hs
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
{% endhighlight %}

Wir verwenden *TemplateHaskell*-Funktionen aus *persistent* für unsere Typ-Definitionen, da für diese dann direkt entsprechende Instanzen für die Verwendung mit
persistent generiert werden. Außerdem implementieren wir für beide Typen noch `ToJSON`/`FromJSON` Instanzen, sodass wir die Daten später einfach (de-)serialisieren
können.

Nun können wir den eigentlichen Server implementieren. Hierzu habe ich als Framework *scotty* gewählt, weil es sehr klein, einfach und, meiner Meinung nach, perfekt
geeignet ist um einen einfachen Server mit *REST-API* zu implementieren.

{% highlight haskell %}
-- Datei App.hs
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
            file f
{% endhighlight %}

Der Code ist eigentlich relativ selbsterklärend: In der *scotty*-Monade definieren wir zu sogenannten Routes eine Action. Zuerst fügen wir ein paar Routes
hinzu um die statischen *HTML*/*JavaScript*-Dateien zu laden. Dann kommt die *REST-API*: Zunächst definieren wir zwei GET-Routes `/news` und `/comments/:id` um
aus der Datenbank News-Einträge und deren Kommentare abzufragen. Mit `selectList` aus Persistent können wir sehr einfach entsprechende Anfrage durchführen.
Die Funktion nimmt als ersten Parameter `Filter` und als zweite weitere Optionen wie zum Beispiel sortieren oder Limits. Bei den Kommentaren beispielsweise
suchen wir nach allen Kommentaren, die zu der News mit der ID `newsId` gehören. Mit `fromPathPiece` wandeln wir die Eingabe in eine Datenbank ID um - das
`fromJust` ist an dieser Stelle auch nicht gefährlich, da jedes Request in seinem eigenen Thread lebt, und falls dieser per Exception beendet wird bekommt
unser *JavaScript* später einen HTTP-Fehlercode. Der Server läuft einfach weiter.

Das `runDB` sorgt dafür, dass unsere *persistent*-Aktionen in der richtigen Monade laufen - letztendlich wird pro Request eine neue Datenbankverbindung geöffnet
und dann wieder beendet. Man könnte hier übrigens noch eine Optimierung durchführen und einige Verbindungen bereits beim Start des Servers öffnen und offen
halten (*ConnectionPool*), sodass dann bei einem Request zur Antwortzeit nicht noch die Verbindungszeit zur Datenbank hinzukommt.

`json` serialisiert dann das Ergebnis unserer Datenbank-Abfrage (was Dank unseren oben definierten Instanzen ohne Probleme möglich ist) und erzeugt
dann eine Antwort.

Nun implementieren wir noch das Hinzufügen von News und Kommentaren. Hierzu sind zwei neue POST-Routes notwendig: `/news` und `/comments`. Die `parseComment`/`parseNews`
Funktion nimmt den POST-Body und parst diese als *JSON* in unsere Datentypen. Mit `insert` aus *persistent* speichern wir dann den Kommentar bzw. den Newsbeitrag.
Ein *Forgein-Key Constraint* sorgt dafür, dass wir nur Kommentare zu existierenden News speichern können. Wenn das JSON-Parsen oder das Speichern fehlschlägt,
dann wird der Thread wieder beendet und unser JavaScript erhält einen Fehlercode. Für unsere *REST*-Schnittstelle gilt also: Wenn der Server ein Request
beantwortet, gab es keine Fehler. Ansonsten ist etwas mit der Eingabe falsch. Das ist zugegebenermaßen nicht optimal, da man zum Beispiel keine näheren
Informationen zum Fehler bekommt, aber genauere Fehlerbehandlung würde an dieser Stelle den Rahmen sprengen.

Das war's eigentlich schon - unser Server ist „fertig“! Natürlich fehlen hier noch Sachen wie zum Beispiel Authentifizierung (damit nicht jeder News verfassen
kann), eine Suchfunktion, etc., aber auch das geht über den Umfang dieses Beitrags hinaus. Jetzt können wir noch eine Main-Datei:

{% highlight haskell %}
-- Datei main.hs
module Main
( main )
where

import App

main = launchServer 8085
{% endhighlight %}

Eine Cabal-Datei:

{% highlight haskell %}
Name:                Blog
Version:             0.1
Synopsis:            Very simple REST-API server for a blog
Author:              Alexander Thiemann
Maintainer:          Alexander Thiemann <mail@agrafix.net>
Build-Type:          Simple

Executable:          Blog
Main-is:             Main.hs
Build-Depends:       base>=4.5, aeson>=0.6, bytestring, text, wai-extra, mtl, persistent-template<=1.1.2.1,
                     persistent<=1.1.4, persistent-mysql<=1.1.2, transformers, scotty<=0.4.6, resourcet, path-pieces
{% endhighlight %}

und eine Setup.hs-Datei:

{% highlight haskell %}
module Setup where

import Distribution.Simple
main = defaultMain
{% endhighlight %}

erstellen, und unseren Server bauen:
{% highlight bash %}
runhaskell Setup configure --prefix=$HOME --user
runhaskell Setup build
{% endhighlight %}

Dann starten wir den Server:
{% highlight bash %}
dist/build/Blog/Blog
{% endhighlight %}

Rufen wir im Browser nun `http://localhost:8085` auf, bekommen wir *File not found*. Nun schreiben wir also den Code für die index.html:

{% highlight html %}
<!DOCTYPE html>
<html>
<head>
    <title>Haskell Blog</title>
    <script src="/jquery.min.js" type="text/javascript"></script>
    <script src="/soyutils.js" type="text/javascript"></script>
    <script src="/templates.js" type="text/javascript"></script>
    <script src="/app.js" type="text/javascript"></script>

    <style type="text/css">
    body {
        font: 9pt Verdana;
    }
    #container {
        width: 800px;
        margin: 10px auto;
    }

    .newsBit {
        border: 1px solid black;
        padding: 10px;
        margin-bottom: 10px;
    }

    .newsBit h2 {
        margin: 0;
    }
    </style>
</head>
<body>

<div id="container">
    <h1>Haskell Blog</h1>
    <div id="main">
        Lade ...
    </div>

    <h1>Neuer Beitrag</h1>
    <form id="newNews">
        <label for="author">
            Dein Name:
        </label>
        <input type="text" placeholder="Autor" id="author" /> <br />
        <label for="author">
            Überschrift:
        </label>
        <input type="text" placeholder="Titel" id="title" /> <br />
        <label for="content">
            Beitrag:
        </label> <br />
        <textarea id="content"></textarea> <br />

        <label for="tags">
            Tags:
        </label>
        <input type="text" placeholder="Tags (mit Komma trennen)" id="tags" /> <br />
        <input type="submit" value="Speichern" />
    </form>
</div>

</body>
</html>
{% endhighlight %}

Zum *HTML* Code gibt‘s denke ich nicht viel zu sagen, es ist lediglich ein einfaches Grundgerüst was alle notwendigen *JavaScript*s lädt und einen sehr
einfachen *CSS*-Style enthält.

Schreiben wir nun mit der *SOY*-Template Sprache unsere Templates:

{% highlight html %}
{namespace BlogUI autoescape="true"}

/**
 * Render a list of news
 *
 * @param news
 */
{template .news}
{foreach $entry in $news}
    {call .newsBit data="$entry" /}
{/foreach}
{if (length($news) == 0)}
<i>Keine Beiträge vorhanden.</i>
{/if}
{/template}

/**
 * Render a news entry
 *
 * @param id
 * @param title
 * @param content
 * @param tags
 * @param author
 */
{template .newsBit}
<div class="newsBit">
    <h2>{$title}</h2>
    <p>
        <i>geschreiben von {$author}</i> <br />
        {$content|changeNewlineToBr}
    </p>
    <span class="tags">
        Tags: {foreach $tag in $tags}{$tag}{if (not isLast($tag))}, {/if}{/foreach}
    </span>
    <h3>Kommentar verfassen:</h3>
    <form id="addCommentFor{$id}">
        <input type="text" placeholder="Ihr Name" id="commentAuthor{$id}" /> <br />
        <textarea id="commentText{$id}"></textarea> <br />
        <input type="submit" value="Speichern" />
    </form>
    <br />
    <a href="javascript:Blog.showCommentsClick({$id});" id="commentLink{$id}" class="showComments commentsClosed">
        Kommentare anzeigen
    </a>
    <div id="commentsFor{$id}" style="display:none;"></div>
</div>

<script>
{literal}$('#addCommentFor{/literal}{$id}{literal}').submit(function (e) {
   e.preventDefault();
   Blog.addComment({/literal}{$id}{literal});
});{/literal}
</script>
{/template}

/**
 * Render a list of comments
 *
 * @param comments
 */
{template .comments}
{foreach $comment in $comments}
    {call .commentBit data="$comment" /}
{/foreach}
{if (length($comments) == 0)}
<i>Keine Kommentare vorhanden.</i>
{/if}
{/template}

/**
 * Render a comment
 *
 * @param comment
 * @param author
 */
{template .commentBit}
<div class="commentBit">
    <p>
        <b>{$author}:</b> {$comment|changeNewlineToBr}
    </p>
</div>
{/template}
{% endhighlight %}

Ich denke auch hier versteht mal relativ schnell wie *SOY*-Templates funktionieren: Zunächst wird ein namespace angegeben, in dem die kompilierten
Template-Funktionen später landen sollen. Dann definieren wir unsere Template-Funktionen. Über die Kommentare werden die Parameter gesteuert (`@param xx`)
und dann folgt eben das Template. Mit dem Soy-Compiler können wir nun daraus JavaScript machen:

{% highlight bash %}
java -jar SoyToJsSrcCompiler.jar --outputPathFormat static/templates.js static/templates.soy
{% endhighlight %}

Jetzt fehlt nur noch die Logik, die die *REST-API* mit unseren Templates/Views verbindet. Das geht mit Hilfe von *jQuery* auch relativ einfach:

{% highlight javascript %}
/**
 * Haskell Blog
 * app.js
 */

var Blog = {};

Blog.loadEntries = function () {
    $.get("/news", function (n) {
        var html = BlogUI.news({"news": n});
        $('#main').html(html);
    });
};

Blog.addEntry = function (author, title, content, tags) {
    $.ajax({
        type: "POST",
        url: "/news",
        data: JSON.stringify({
            author: author,
            title: title,
            content: content,
            tags: tags
        }),
        success: function () {
            Blog.resetAddForm();
            Blog.loadEntries();
        },
        error: function () {
            alert("Beitrag konnte nicht angelegt werden");
        }
    });
};

Blog.storeComment = function (newsId, author, text, onOk) {
    $.ajax({
        type: "POST",
        url: "/comments",
        data: JSON.stringify({
            author: author,
            comment: text,
            news: "" + newsId
        }),
        success: onOk,
        error: function () {
            alert("Kommentar konnte nicht gespeichert werden");
        }
    });
};

Blog.resetAddForm = function () {
    $('#author').val(""); $('#title').val(""); $('#content').val(""); $('#tags').val("");
};

Blog.showComments = function (id) {
    $.get("/comments/" + id, function (c) {
        var html = BlogUI.comments({"comments": c});
        $('#commentsFor' + id).html(html).slideDown();
    });
};

Blog.showCommentsClick = function (id) {
    var el = $('#commentLink' + id);

    if (el.hasClass('commentsClosed')) {
        Blog.showComments(id);
        el.text("Kommentare verbergen");
        el.removeClass('commentsClosed');
    } else {
        el.text("Kommentare anzeigen");
        $('#commentsFor' + id).slideUp();
        el.addClass('commentsClosed');
    }
};

Blog.addComment = function (id) {
    var authorEl = $('#commentAuthor' + id);
    var textEl = $('#commentText' + id);

    Blog.storeComment(id, authorEl.val(), textEl.val(), function () {
        authorEl.val(""); textEl.val("");
        Blog.showComments(id);
    });

};

$(function () {
    Blog.loadEntries();
    $('#newNews').submit(function (e) {
        e.preventDefault();
        Blog.addEntry($('#author').val(),
                      $('#title').val(),
                      $('#content').val(),
                      $('#tags').val().split(",")
        );
    });
});
{% endhighlight %}

Nun wäre der Blog also komplett! Wir haben also mit rund 200 Zeilen *Haskell*, etwa 100 Zeilen *JavaScript* und 150 Zeilen *HTML* einen kleinen Blog
implementiert, der gut skaliert und sicher gegen XSS und SQL-Injections ist.
