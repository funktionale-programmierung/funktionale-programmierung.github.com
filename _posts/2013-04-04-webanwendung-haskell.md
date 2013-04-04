---
layout: post
description: Moderne Webanwendungen mit Haskell
title: "Moderne Webanwendungen mit Haskell"
author: alexander-thiemann
tags: ["web", "Haskell", "JavaScript", "SOY", "HTTP", "PHP"]
---
Derzeit sind viele Webanwendungen in PHP geschrieben. Die Gründe dafür liegen auf der Hand: Die Entwicklung geht meist sehr schnell,
PHP ist einfach zu erlernen und fast alle Webhoster haben mittlerweile Webserver mit PHP-Unterstützung installiert. Allerdings bringt die Verwendung
von PHP auch einige Probleme mit sich. Damit eine PHP-Anwendung gut skaliert, sind viele aufwendige Optimierungen notwendig (siehe z.B. [HipHop von Facebook](https://github.com/facebook/hiphop-php)).
Außerdem ist PHP eine dynamische Sprache, und damit treten viele Fehler erst zur Laufzeit auf.
Schließlich ist auch die Validierung und das Escapen von Ausgaben zumeist dem Programmierer selbst überlassen: SQL-Injections, XSS (Einschleusen von Code in fremde Webseiten durch Dritte) und andere Sicherheitslücken werden nicht auf Ebene der Programmiersprache verhindert (siehe zum Beispiel [hier](http://www.tizag.com/mysqlTutorial/mysql-php-sql-injection.php)).

Deshalb möchte ich an einem kleinen Beispiel erläutern, wie man mit [Haskell](http://haskell.org) relativ einfach eine performante,
sichere und moderne Webanwendung schreibt. Hierzu werde ich ein einfaches Blog implementieren. Das Blog wird das erstellen und anzeigen von Beiträgen, sowie das kommentieren von Beiträgen unterstützen.

Um dem Artikel gut folgen zu können sind Grundlagen zu JavaScript, HTML, HTTP und Haskell hilfreich.

<!-- more start -->

Bei einer modernen Webanwendung ist möglichst viel Logik direkt im Client, also im Browser in JavaScript, implementiert. Dank breiter AJAX-Unterstützung in den gängigen Browsern ist eine Kommunikation die ausschließlich Daten zwischen Server und Client überträgt auch problemlos möglich. Auch in unserem Blog werden wir deshalb alle Views und Controller in JavaScript implementieren. In Haskell müssen wir dann nur nur das Modell, eine Komponente die die Daten akzeptiert und ausgibt (über eine *REST-API*: HTTP-GET um Objekte zu laden, HTTP-POST um neue Objekte anzulegen.), entwickeln. Für die Views verwenden wir die funktionale
[(Google) Soy-Templates-Sprache von Google](https://developers.google.com/closure/templates/), diese wird dann nach JavaScript kompiliert sodass wir unsere Views mit unserer JavaScript-Controller Logik ansteuern können.

Beginnen wir nun mit der Haskell Komponente, dem Modell. Als Web-framework verwenden wir [scotty](http://hackage.haskell.org/packages/archive/scotty/0.4.6/doc/html/Web-Scotty.html), als Datenbankabstraktionsschicht [persistent(-mysql)](http://hackage.haskell.org/packages/archive/persistent/1.1.5.1/doc/html/Database-Persist.html). Die Blogeinträge und Kommentare werden nach *JSON* ([aeson](http://hackage.haskell.org/packages/archive/aeson/0.6.1.0/doc/html/Data-Aeson.html)) serialisiert. Die entsprechenden Haskell-Pakete sollten in den entsprechenden Versionen installiert sein (siehe *cabal*-Datei unten). Für *persistent* gibt es noch weitere Datenbankbackends neben MySQL, hier könnte man also ebenfalls SQLite oder PostgreSQL verwenden.

Definieren wir zunächst unsere Typen und deren Serialisierung:

{% highlight haskell %}
-- Datei Types.hs
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}
{% endhighlight %}

Die LANGUAGE-Pragmas sind notwendig, damit fortgeschrittene Sprachfeatures angeschaltet sind, damit *persistent* und *scotty* richtig funktionieren.

{% highlight haskell %}
module Types where

import Database.Persist
import Database.Persist.TH

import Data.Aeson

import qualified Data.Text as T

import Control.Monad

import Web.PathPieces (fromPathPiece)
import Data.Maybe (fromJust)
{% endhighlight %}

Hier importieren wir die Module aus *persistent*, damit wir mit Hilfe von [TemplateHaskell](http://www.haskell.org/haskellwiki/Template_Haskell) später neben den Typ-Definitionen automatisch auch die entsprechenden Instanzen für die Verwendung mit *persistent* generiert bekommen. Das *aeson*-Modul verwenden um Instanzen für die JSON-Serialisierung definieren zu können. *text* verwenden wir für die Representation von Texten.

{% highlight haskell %}
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
{% endhighlight %}

Das Datenmodell ist denkbar einfach: Ein Blogbeitrag hat einen Titel, einen Inhalt, Tags und einen Autor. Ein Kommentar hat einen Autor, einen Inhalt und referenziert einen Blogbeitrag. Das `mkMigrate` generiert zusätzlich noch eine Migrationsfunktion, dazu später mehr. `persistUpperCase` benennt die Felder, indem der Name der Tabelle mit dem Namen des Feldes konkateniert wird, und der erste Buchstabe des Feldnamen wird groß geschrieben. (zB
*NewsItemTitle*) Mehr Informationen über was hier genau von *TemplateHaskell* generiert wird findet man [hier (Abschnitt Code Generation)](http://www.yesodweb.com/book/persistent)

{% highlight haskell %}
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
        do title <- v .: "title"
           content <- v .: "content"
           tags <- v .: "tags"
           author <- v .: "author"
           return $ NewsItem title content tags author
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

instance FromJSON NewsComment where
    parseJSON (Object v) =
        do author <- v .: "author"
           comment <- v .: "comment"
           newsId <- v .: "news"
           return $ NewsComment author comment (parseNewsId newsId)
    parseJSON _ = mzero
{% endhighlight %}

Damit wir später unsere Haskell-Typen einfach nach JSON serialisieren und von JSON deserialisieren können, müssen wir die Instanzen `FromJSON` und `ToJSON` aus *aeson*
implementieren. Die `ToJSON` Instanzen beziehen sich allerdings nicht direkt auf den eigentlichen Typ, sondern auf die entsprechende Datenbank-Entity mit ID. Der Grund hierfür liegt
auf der Hand: Das *persistent*-Framework liefert als Antwort auf zum Beispiel `selectList` eine Liste von solchen Entities. Da *aeson* bereits mit einer Serialisierung für `[a]` kommt, können wir also unsere Liste von Entities dann ganz einfach serialisieren. Da wir beim Erzeugen von Kommentaren/Beiträgen dessen ID noch nicht kennen, und wir zum Einfügen (`insert`) in *persistent* den "rohen" Typ benötigen, schreiben wir hierfür eine `FromJSON` Instanz. Wir können nun also zum Beispiel folgendes parsen:
{% highlight javascript %}
{
  "title": "Hallo Blog",
  "content": "Das hier ist mein erster Beitrag",
  "author": "Alexander Th",
  "tags": ["blog", "haskell", "toll"]
}
{% endhighlight %}

Nun können wir den eigentlichen Server implementieren. Hierzu habe ich als Framework *scotty* gewählt, weil es sehr klein, einfach und, meiner Meinung nach, perfekt
geeignet ist um einen einfachen Server mit *REST-API* zu implementieren.

{% highlight haskell %}
-- Datei ServerApp.hs
{-# LANGUAGE OverloadedStrings, FlexibleContexts, DoAndIfThenElse, GADTs,
             TypeFamilies, BangPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}
module ServerApp
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
{% endhighlight %}

Die meisten Module sind bereits aus *Types.hs* bekannt. `Web.Scotty` ist das Hauptmodul des *scotty*-Webframeworks.
`Web.PathPieces` benötigen wir später um Parameter in eine Datenbank-Id zu parsen. Der *RequestLogger* aus
`Network.Wai.Middleware.RequestLogger` ist zu Debug-Zwecken: Es ist eine [Middleware](http://de.wikipedia.org/wiki/Middleware), die
alle HTTP-Requests, die an unseren Server gehen, in die Konsole loggt. Da die SQL-Verbindung aus *persistent* in der `ResourceT` Monade
laufen muss, benötigen wir die Funktion `runResourceT` aus *Control.Monad.Trans.Resource*.

{% highlight haskell %}
instance Parsable T.Text where parseParam = Right . TL.toStrict
{% endhighlight %}

Diese Instanz ist notwendig, damit *scotty* Parameter in *text* parsen kann.

{% highlight haskell %}
data ContentType = CtHtml | CtJavaScript deriving (Show, Eq, Enum)

ctToLText :: ContentType -> TL.Text
ctToLText CtHtml = "text/html"
ctToLText CtJavaScript = "text/javascript"
{% endhighlight %}

Hier definieren wir eine Hilfsfunktion, um einfach den HTML-*Content-Type* für statische
Dateien angeben zu können.

{% highlight haskell %}
mysqlInfo = SQL.defaultConnectInfo
            { SQL.connectDatabase = "blog"
            , SQL.connectPassword = ""
            , SQL.connectUser = "root"
            , SQL.connectHost = "127.0.0.1"
            , SQL.connectPort = 3306
            }
{% endhighlight %}

Die Konfiguration für die Datenbank - MySQL-Benutzername, Passwort, Host und Datenbank.

{% highlight haskell %}
runDB x = liftIO $ do runResourceT $ SQL.withMySQLConn mysqlInfo $ SQL.runSqlConn x
{% endhighlight %}

Das `runDB` sorgt dafür, dass unsere *persistent*-Aktionen in der richtigen Monade laufen - letztendlich wird pro Request eine neue Datenbankverbindung geöffnet
und dann wieder beendet. Man könnte hier übrigens noch eine Optimierung durchführen und einige Verbindungen bereits beim Start des Servers öffnen und offen
halten (*ConnectionPool*, ist mit *persistent* relativ einfach möglich), sodass dann bei einem Request zur Antwortzeit nicht noch die Verbindungszeit zur Datenbank hinzukommt.

{% highlight haskell %}
launchServer port =
    do runResourceT $ SQL.withMySQLConn mysqlInfo $
                      SQL.runSqlConn $ SQL.runMigrationUnsafe migrateAll
{% endhighlight %}

Hier führen wir die *persistent*-Datenbank-Migrationen aus. Persistent legt als automatisch nicht existierende Tabellen und Felder an. Gibt es eine Migration, die
*persistent* nicht selbst durchführen kann, so beendet sich der Server mit einer Fehlermeldung.

Jetzt werden die Routen definiert. *scotty* orientiert sich dabei sehr an [sinatra (Ruby)](http://www.sinatrarb.com/): eine Routen-Definition sieht
zum Beispiel wie folgt aus:
{% highlight haskell %}
    [get/post/put/delete] "/ein/pfad/:parameter" $ do
        v <- param "parameter"
        text v
{% endhighlight %}

Zunächst wählen wir die passende Funktion zu unseren HTTP-Request-Type, dann geben wir den Pfad an. Für Parameter schreiben wir `:parameterName`. Diese können wir dann auf zwei Arten auslesen:
{% highlight haskell %}
    get "/ein/beispiel/:p" $ \p -> do
        ...
{% endhighlight %}

oder:
{% highlight haskell %}
    get "/ein/beispiel/:p" $ do
        p <- param "p"
{% endhighlight %}

Die `param` Funktion sucht übrigens bei *POST*-Requests auch in FormData nach einem entsprechend benannten Parameter. Um etwas an den Browser zurück zu geben können wir eine der folgenden Funktionen wählen:

 * `text` einfach Text
 * `html` Text, den der Browser als HTML interpretieren soll
 * `file` Eine Datei auf dem Server laden und an den Browser schicken
 * `json` Ein beliebiges Haskell-Record senden, welches eine ToJSON-Instanz hat

Mehr dazu findet man in der [scotty Dokumentation](http://hackage.haskell.org/packages/archive/scotty/0.4.6/doc/html/Web-Scotty.html)

{% highlight haskell %}
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
             response <- runDB $ do comments <- SQL.selectList
                                                [
                                                 NewsCommentNews
                                                   SQL.==.
                                                 ((fromJust $ fromPathPiece newsId) :: NewsItemId)
                                                ]
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

In der *scotty*-Monade definieren wir zu sogenannten Routes eine Action. Zuerst fügen wir ein paar Routes
hinzu um die statischen *HTML*/*JavaScript*-Dateien zu laden. Dann kommt die *REST-API*: Zunächst definieren wir zwei GET-Routes `/news` und `/comments/:id` um
aus der Datenbank News-Einträge und deren Kommentare abzufragen. Mit `selectList` aus *persistent* können wir sehr einfach entsprechende Anfrage durchführen.
Die Funktion nimmt als ersten Parameter `Filter` und als zweiten weitere Optionen wie zum Beispiel sortieren oder Limits. Bei den Kommentaren beispielsweise
suchen wir nach allen Kommentaren, die zu der News mit der ID `newsId` gehören. Mit `fromPathPiece` wandeln wir die Eingabe in eine Datenbank-ID um - das
`fromJust` ist an dieser Stelle auch nicht gefährlich, da jedes Request in seinem eigenen Thread lebt, und falls dieser per Exception beendet wird bekommt
unser *JavaScript* später einen HTTP-Fehlercode. Der Server läuft einfach weiter.

`json` serialisiert dann das Ergebnis unserer Datenbank-Abfrage (was Dank unseren oben definierten Instanzen ohne Probleme möglich ist) und erzeugt
dann eine Antwort.

Nun implementieren wir noch das Hinzufügen von News und Kommentaren. Hierzu sind zwei neue POST-Routes notwendig: `/news` und `/comments`. Die `parseComment`/`parseNews`
Funktion nimmt den POST-Body und parst diesen als *JSON* in unsere Datentypen. Mit `insert` aus *persistent* speichern wir dann den Kommentar bzw. den Newsbeitrag.
Ein *Foreign-Key-Constraint* sorgt dafür, dass wir nur Kommentare zu existierenden News speichern können. Wenn das JSON-Parsen oder das Speichern fehlschlägt,
dann wird der Thread wieder beendet und unser JavaScript erhält einen Fehlercode. Für unsere *REST*-Schnittstelle gilt also: Wenn der Server ein Request
beantwortet, gab es keine Fehler. Ansonsten ist etwas mit der Eingabe falsch. Das ist zugegebenermaßen nicht optimal, da man zum Beispiel keine näheren
Informationen zum Fehler bekommt, aber genauere Fehlerbehandlung würde an dieser Stelle den Rahmen sprengen.

Das war's eigentlich schon - unser Server ist „fertig“! Natürlich fehlen hier noch Sachen wie zum Beispiel Authentifizierung (damit nicht jeder News verfassen
kann), eine Suchfunktion, etc., aber auch das geht über den Umfang dieses Beitrags hinaus. Nun benötigen wir noch eine Main.hs:

{% highlight haskell %}
-- Datei main.hs
module Main
( main )
where

import ServerApp

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

Rufen wir im Browser nun `http://localhost:8085` auf, bekommen wir *File not found*.

Zum Schluss noch ein kleiner Test unseres Servers mit `curl`:
{% highlight bash %}
$ curl http://localhost:8085/news
[]
{% endhighlight %}
Keine Blogbeiträge vorhanden, da noch keiner in die Datenbank eingetragen wurde.
Wir können mit CURL einen Beitrag hinzufügen:
{% highlight bash %}
$ curl --data '{"title": "Test", "author": "Alex", "tags": ["a", "b"], "content": "Test Beitrag"}' http://localhost:8085/news
true
{% endhighlight %}
Nun können wir diesen in unserer Liste von Beiträgen sehen:
{% highlight bash %}
$ curl http://localhost:8085/news
[{"title":"Test","author":"Alex","tags":["a","b"],"id":5,"content":"Test Beitrag"}]
{% endhighlight %}

Das war's für heute, den JavaScript/HTML-Client implementieren wir in Teil 2!
