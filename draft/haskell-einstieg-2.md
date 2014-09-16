---
layout: post
description: Haskell für Einsteiger, Teil 2
title: "Haskell für Einsteiger, Teil 2"
author: stefan-wehr
tags: ["Haskell"]
---

Mit dem heutigen Blogartikel möchte ich die Artikelserie
"Haskell für Einsteiger" fortsetzen.
Die Serie richtet sich an Leser mit Programmiererfahrung, die
Lust auf Haskell haben, bisher aber den Einstieg in die Sprache nicht richtig
geschafft haben. Im [ersten Teil](/2014/07/25/haskell-einstieg.html) ging
es um eine abgespeckte Variante des Unix-Tools `tail`,
heute soll es um ein kleines, aber nützliches Programm gehen,
welches bei der Analyse von Textdateien mit verschiedenen Encodings
hilfreich sein kann.

<!-- more start -->

Um loszulegen benötigen Sie
lediglich eine Installation der
[Haskell Platform](https://www.haskell.org/platform), sowohl die neue
Version 2014.2.0.0 als auch die vorige Version 2013.2.0.0
wird unterstützt. Außerdem brauchen Sie ein Checkout
des
[git Repositories](https://github.com/funktionale-programmierung/haskell-for-beginners.git)
zu dieser Artikelserie.

Ich werde mir Mühe geben, viele Dinge detailliert zu erklären. Allerdings
würde es den Rahmen dieses Blogs sprengen, auf jedes Detail
einzugehen. Hierzu sei das Studium des einen oder
anderen Haskell [Tutorials](http://learnyouahaskell.com/chapters) oder
[Buchs](http://www.realworldhaskell.org/) empfohlen. Natürlich können Sie
Rückfragen auch als Kommentar zu diesem Artikel stellen.

Im heutigen Artikel implementieren wir ein Tool names `view-ascii`. Dieses
Programm gibt eine Textdatei so aus, dass ASCII-Steuerzeichen und das
Encoding von Umlauten sofort ersichtlich ist. Beim Debuggen von
Encodingproblem hat mir dieses Tool schon öfters geholfen.

Um zu schauen, ob Ihre Haskell-Installation funktioniert, sollten Sie
spätestens jetzt ein Checkout des
[git Repositories](https://github.com/funktionale-programmierung/haskell-for-beginners.git)
machen. Dann führen Sie im Verzeichnis `haskell-for-beginners` folgende
Befehle aus:

~~~
$ cabal build                        # baut das Programm
~~~

Wenn alles gut geht, finden Sie jetzt das kompilierte Programm unter
`dist/build/view-ascii/view-ascii`. Im Repository finden Sie auch
Textdateien in unterschiedlichen Encodings. Der Inhalt der
Textdatei sieht immer so aus:

~~~
$ cat test/utf8.txt
Hallo Umlaute: äöüß!
auch in Großbuchstaben: ÄÖÜ
~~~

Hier ein Aufruf von `view-ascii` mit einer Datei, die diesen Inhalt
in gemischten Encodings speichert:

~~~
$ dist/build/view-ascii/view-ascii test/mixed.txt
Hallo Umlaut: <utf-8: a with diaresis>!
auch in Gro<iso-8859-1: sharp s>buchstaben: <iso-8859-1: O with diaresis>
~~~

So, jetzt aber zur Implementierung. Wir importieren zuerst das Modul
`Data.ByteString` zum Arbeiten mit rohen Bytes.
Die beiden Module `Data.Text` und `Data.Text.Encoding` brauchen wir,
um Strings in Bytes zu kodieren.

{% highlight haskell %}
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
{% endhighlight %}

Wir benötigen noch weitere Standardmodule zum Lesen von Dateien, zum
Umgang mit Ausnahmen dabei, sowie zum Zugriff auf Kommandozeilenargument
und zum vorzeitigen Beenden des Programms.

{% highlight haskell %}
import System.IO
import Control.Exception
import System.Environment
import System.Exit
{% endhighlight %}

Jetzt geht's richtig los. Wir definieren uns zuerst eine Tabelle, in
der wir angeben, welche Bytesequenzen wir in der Ausgabe speziell
darstellen möchten. Im deutschsprachen Raum sind dies die Umlaute und
das Scharf-s in UTF-8 und ISO-8859-1 Encoding. Sie
können die Tabelle aber auch gerne noch um weitere Zeichen und/oder
Encodings erweitern. Die Tabelle ist als Liste von Paaren repräsentiert
und hat den Typ `[(BS.ByteString, String)]`. Die erste
Komponente eines solchen Paars ist dabei die zu erkennende Bytesequenz,
die zweite der Ersetzungstext.

{% highlight haskell %}
_SPECIAL_CODES_ :: [(BS.ByteString, String)]
_SPECIAL_CODES_ =
    [(p1 0xE4      , "<iso-8859-1: a with diaresis>")
    ,(p1 0xF6      , "<iso-8859-1: o with diaresis>")
    ,(p1 0xFC      , "<iso-8859-1: u with diaresis>")
    ,(p1 0xC4      , "<iso-8859-1: A with diaresis>")
    ,(p1 0xD6      , "<iso-8859-1: O with diaresis>")
    ,(p1 0xDC      , "<iso-8859-1: U with diaresis>")
    ,(p1 0xDF      , "<iso-8859-1: sharp s>")
    ,(p2 0xC3 0xA4 , "<utf-8: a with diaresis>")
    ,(p2 0xC3 0xB6 , "<utf-8: o with diaresis>")
    ,(p2 0xC3 0xBC , "<utf-8: u with diaresis>")
    ,(p2 0xC3 0x84 , "<utf-8: A with diaresis>")
    ,(p2 0xC3 0x96 , "<utf-8: O with diaresis>")
    ,(p2 0xC3 0x9C , "<utf-8: U with diaresis>")
    ,(p2 0xC3 0x9f , "<utf-8: sharp s>")]
    where
      p1 = BS.singleton
      p2 w1 w2 = BS.pack [w1, w2]
{% endhighlight %}

Als nächstes benutzen wir die soeben definierte Tabelle, um die Funktion
`lookupSpecial` zu definieren. Diese Funktion prüft, ob die übergebenen
Bytes mit einer solchen speziellen Sequenz beginnen.

{% highlight haskell %}
lookupSpecial :: BS.ByteString -> Maybe (String, BS.ByteString)
lookupSpecial bs =
    lookup _SPECIAL_CODES_
    where
      lookup [] = Nothing
      lookup ((code, msg):rest) =
          let (prefix, suffix) = BS.splitAt (BS.length code) bs
          in if prefix == code
             then Just (msg, suffix)
             else lookup rest
{% endhighlight %}

Der Rückgabetyp von `lookupSpecial`
ist `Maybe (String, BS.ByteString)`. Der `Maybe`-Typ
kennt zwei Varianten:

* `Just x`: es ist ein Wert vorhanden und dieser Wert ist `x`
* `Nothing`: es ist kein Wert vorhanden

In Haskell setzt man `Nothing` häufig dort ein, wie man in Sprachen wie
Java oder C# den Wert `null` verwendet. Allerdings ist `Nothing`
typsicher, d.h. der Haskell-Compiler zwingt den Programmierer an allen
Stellen sauber zwischen `Just x` und `Nothing` zu unterscheiden. In Java
oder C# hingegen ist dies nicht der Fall, dort kommt es dann zur Laufzeit
zur berühmt-berüchtigten `NullPointerException`.

Nun schreiben wir die Funktion `transform`, welche eine Sequenz von Bytes
so transformiert, dass unsere Spezialzeichen auch wirklich speziell
dargestellt werden. In der `transform`-Funktionen übernehmen wir als
Optimierung zuerst alle druckbaren ASCII-Zeichen unverändert in das
Ergebnis. Dann prüfen wir, ob die restliche Bytesequenz mit einem
speziellen Zeichen beginnt und berechnen dafür das Ergebnis. Wenn
noch Bytes vorhanden sind, rufen wir abschließend `transform` rekursiv auf.

{% highlight haskell %}
transform :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
transform chunk acc =
    let (prefix, suffix) = BS.span (\w -> w >= 0x20 && w <= 0x7E) chunk
        newAcc = prefix : acc
    in case lookupSpecial suffix of
         Nothing ->
             case BS.uncons suffix of
               Nothing ->  -- suffix ist leer
                   BS.concat (reverse newAcc) -- Rückgabe des Ergebnis'
               Just (b, rest) ->
                   let newAcc' =
                           (if b == 10
                            then BS.singleton b -- \n
                            else encode ("<" ++ show b ++ ">")) : newAcc
                   in transform rest newAcc' -- endrekursiv
         Just (special, rest) ->
             transform rest (encode special : newAcc)  -- endrekursiv
    where
      encode :: String -> BS.ByteString
      encode s = T.encodeUtf8 (T.pack s)
{% endhighlight %}

Der zweite Parameter `acc` von `transform` ist ein sogenannter
*Akkumulator*, welcher zum Aufsammeln des Ergebnisses dient. In der
funktionalen Programmierung benutzt man einen Akkumulator oft, um eine
Funktion *endrekursiv* schreiben zu können. Endrekursiv bedeutet, dass der
rekursive Aufruf die letzte "Aktion" der Funktion ist. Bei unserer
`transform`-Funktion sind beide rekursiven Aufrufe auch endrekursiv, wie
durch die Kommentare angedeutet. Der Vorteil von endrekursiven Funktionen
ist, dass sie nur konstanten Platz auf dem Stack brauchen, womit die
Rekursionstiefe unbeschränkt ist.

Sie wundern sich vielleicht, warum wir bei der Rückgabe des Ergebnis' auf
dem Akkumulator noch ein `reverse` ausführen. Die Benutzung von `reverse`
ist nötig, da wir im Verlauf der Rekursion die Teilergebnisse immer vorne an den Akkumulator
angehängt haben (mittels des Operators `:`). Wir haben
diesen Weg gewählt, da `:` konstante Laufzeit hat. Wir hätten alternativ
auch die Teilergebnisse mittels `++` hinten an den Akkumulator anhängen
können. Dies ist allerdings ineffizient, weil das Konkatenieren von zwei
Listen `l1 ++ l2` eine Laufzeit linear zur Länge der Liste `l1` hat.

So, nun aber zurück zum eigentlichen Programm. Mittels `transform` können
wir nun eine Funktion implementieren, die eine komplette Datei
transformiert und das Ergebnis auf dem Bildschirm ausgibt. Dazu lesen
wir die Datei in 4kB großen Blöcken ein, transformieren den Block und
machen solange weiter, bis wir die Datei vollständig abgearbeitet haben.

{% highlight haskell %}
handleFile :: FilePath -> IO ()
handleFile file =
    if file == "-"
    then action stdin
    else withFile file ReadMode action
    where
      action h = loop h `catch` (\ex -> hPutStrLn stderr (file ++ ": " ++
                                                          show (ex::IOException)))
      loop h =
          do bs <- BS.hGet h 4096
             if BS.null bs
             then return () -- EOF
             else do BS.putStr (transform bs [])
                     loop h
{% endhighlight %}

Wir benutzen `catch`, um Ausnahmen, die beim Lesen der Datei auftreten, zu
fangen und als Fehler auf dem Bildschirm auszugeben. Damit können wir
später mehrere Dateien verarbeiten und ein Fehler bei einer Datei führt
nicht sofort zum Abbruch des gesamten Programms.

Jetzt bleibt uns nur noch das Parsen der Kommandozeilenargumente und die
eigentliche `main`-Funktion. Dies ist heute einfach, die `view-ascii`
lediglich die zu verarbeitenden Dateien übergeben bekommt.

{% highlight haskell %}
parseArgs :: [String] -> IO [FilePath]
parseArgs args =
    if "-h" `elem` args || "--help" `elem` args
    then usage
    else if null args
         then return ["-"]
         else return args
    where
      usage = abort "USAGE: view-ascii [FILE ...]"
      abort msg =
          do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

main =
    do args <- getArgs
       files <- parseArgs args
       mapM_ handleFile files
{% endhighlight %}

Damit haben wir die Implementierung von `view-ascii` abgeschlossen.
Ich hoffe, Sie haben etwas gelernt und freue mich auf Ihr Feedback!
