---
layout: post
description: Haskell für Einsteiger, Teil 3
title: "Haskell für Einsteiger, Teil 3"
author: stefan-wehr
tags: ["Haskell"]
---

Mit dem heutigen Blogartikel möchte ich die Serie
"Haskell für Einsteiger" fortsetzen.
Die Serie richtet sich an Leser mit Programmiererfahrung, die
Lust auf Haskell haben, bisher aber den Einstieg in die Sprache nicht richtig
geschafft haben. Im [ersten Teil](/2014/07/25/haskell-einstieg.html) ging
es um eine abgespeckte Variante des Unix-Tools `tail` und im
[zweiten Teil](/2014/09/18/haskell-einstieg-2.html) haben wir ein
Programm zur Analyse von Textdateien mit verschiedenen Encodings
geschrieben. Heute werden wir
einen Pretty-Printer für [JSON](http://json.org) schreiben und
dabei Datentypen sowie eine Bibliothek zur JSON-Verarbeitung und zum
Formatieren von Texten kennenlernen.

<!-- more start -->

Um loszulegen benötigen Sie
lediglich eine Installation der
[Haskell Platform](https://www.haskell.org/platform), sowohl die neue
Version 2014.2.0.0 als auch die vorige Version 2013.2.0.0
wird unterstützt. Außerdem brauchen Sie ein Checkout
des
[git-Repositories](https://github.com/funktionale-programmierung/haskell-for-beginners.git)
zu dieser Artikelserie.

In diesem Posting versuche ich, die Funktionsweise des Codes möglichst
verständlich zu erläutern. Allerdings
würde es den Rahmen dieses Blogs sprengen, auf jedes Detail
einzugehen. Hierzu sei das Studium des einen oder
anderen [Haskell-Tutorials](http://learnyouahaskell.com/chapters) oder
[-Buchs](http://www.realworldhaskell.org/) empfohlen. Natürlich können Sie
Rückfragen auch als Kommentar zu diesem Artikel stellen.

Im heutigen Artikel implementieren wir ein Tool names `jsonpp`. Dieses
Programm liest JSON-Daten ein und gibt sie schön formatiert auf dem
Bildschirm aus. Hier ein Beispiel für eine Eingabe von `jsonpp`:

~~~
// Datei sample.json
[{"name": {"first": "Stefan", "last": "Wehr"}, "age": 36}, {"name": "Max", "age": 23}]
~~~

Die Ausgabe sieht für `sample.json` wie folgt aus:

~~~
$ jsonpp sample.json
[{"age": 36.0,
  "name":
     {"first": "Stefan",
      "last": "Wehr"}},
 {"age": 23.0,
  "name": "Max"}]
~~~

Damit kann man in einer typischen Unix-Pipeline JSON-Daten viel besser
mittels `grep`, `sed` und Co bearbeiten. Als zusätzliches Feature möchten
wir noch einbauen, dass man die JSON-Daten mittels eines Pfadausdruck
filtern kann. Wenn wir z.B. als Pfadausdruck einfach `name` wählen, werden
nur die Daten unterhalb von `name` ausgegeben:

~~~
$ jsonpp --filter name test/sample.json
[{"first": "Stefan",
  "last": "Wehr"},
 "Max"]
~~~

Wir können auch kompliziertere Pfadausdrücke angeben:

~~~
$ jsonpp --filter name.first test/sample.json
["Stefan"]
~~~

So, jetzt wissen wir, was `jsonpp` machen soll und können mit der
Implementierung beginnen. Wir starten mit 2
[Pragmas](https://www.haskell.org/ghc/docs/7.8.3/html/users_guide/pragmas.html).

{% highlight haskell %}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{% endhighlight %}

Das erste sorgt dafür, dass der [GHC](https://www.haskell.org/ghc/) vor
dem Kompilieren den `htfpp`-Präprozessor über den Code laufen
lässt. Der Präprozessor gehört zum
[HTF](https://hackage.haskell.org/package/HTF) Paket und sorgt dafür, dass Testfälle
automatisch aufgesammelt werden. Wir haben HTF auch schon in einem
[vorigen Artikel](2014/02/06/testing-haskell.html) kennengelernt.
Mehr zum Testen aber später.

Das zweite Pragma schaltet die Spracherweiterung `OverloadedStrings`
ein. Um zu verstehen, warum diese Spracherweiterung sinnvoll ist, muss man
wissen, dass der `String`-Typ in Haskell ein Synonym für `[Char]` ist, d.h.
ein String in Haskell ist einfach als verkettete Liste von Characters
implementiert. Dies hat den Vorteil, dass alle Funktionen auf Listen auch
direkt auf Strings funktionieren (und es gibt in der
[Standardbibliothek](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html)
sehr viele solcher Funktionen). Der Nachteil ist aber, dass daduch die
Speicherrepräsentation von Strings nicht sonderlich effizient ist. Daher
gibt es eine Reihen von Bibliotheken, die alternative String-Typen zur
Verfügung stellen. Die bekannteste Bibliothek ist
[Data.Text](http://hackage.haskell.org/package/text-1.2.0.0/docs/Data-Text.html),
welche den Typ `Text` bereitstellt und Strings als Array von UTF-16 kodierten Zeichen repräsentiert. Die
Spracherweiterung `OverloadedStrings` sorgt nun dafür, dass wir normale
Stringliterale auch an Stellen verwenden können, in denen ein solcher alternativer
String-Typ erwartet wird.

Als nächstes folgen eine Reihen von Imports:

{% highlight haskell %}
-- Bibliothek für JSON
import qualified Data.Aeson as J

-- Alternative Strings
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Rohe Byte-Arrays
import qualified Data.ByteString as BS

-- Hash-Maps
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- Arrays beliebiger Datentypen
import qualified Data.Vector as V

-- Bibliothek zur Textformatierung
import Text.PrettyPrint

-- Unterstützung für Tests
import Test.Framework

-- Standardmodule
import Data.Maybe
import System.IO
import System.Environment
import System.Exit
{% endhighlight %}

Einige der Imports sind qualifiziert (Schlüsselwort `qualified`), d.h. Funktionen und Typen aus diesen Modulen
können nur durch Voranstellen des Modulnamens gefolgt von einem Punkt verwendet werden. Da die Modulnamen
relativ lang sind, geben wir mittels dem `as` Teil noch ein Kürzel für den Modulnamen an. So definiert
`Data.Vector` z.B. eine Funktion `fromList`. Um diese in unserem Programm verwenden zu können, müssen
wir jetzt `V.fromList` schreiben.

Wenn Sie selbst ein Haskell Programm schreiben, aber nicht wissen welche
Funktionen in welchen Modulen zu finden sind, gibt es mindestens drei
Möglichkeiten, dies herauszufinden:

* Sie benutzen [hoogle](http://www.haskell.org/hoogle/) oder
  [hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html),
  zwei Suchmaschine für Haskell-API-Dokumentation.
* Sie studieren die
  [Übersicht](http://www.haskell.org/ghc/docs/latest/html/libraries/) der
  gängigen Haskell Module.
* Sie schauen sich auf [hackage](http://hackage.haskell.org/) um.

Jetzt geht's richtig los. Wir widmen uns zunächst dem Filtern von
JSON-Daten. Wir haben oben gesehen, dass wir Pfadausdrücke der Form
`name.first` angeben können, um bestimmte Teile der JSON-Daten zu
selektieren. Ein Pfadausdruck ist also eine Liste von Propertyname:

{% highlight haskell %}
type JsonPath = [T.Text]
{% endhighlight %}

Die eigentliche Funktion `filterJson` zum Filtern nimmt einen solchen Pfadausdruck
und eine Repräsentation der JSON-Daten.
Um JSON-Daten zu Repräsentieren,
zu Parsen und zu Serialisieren, benutzen wir die Bibliothek
[aeson](http://hackage.haskell.org/package/aeson-0.7.0.6). Dort ist der
zentral Datentyp
[Value](http://hackage.haskell.org/package/aeson-0.7.0.6/docs/Data-Aeson.html#t:Value)
definiert. Wir sehen den `Value`-Typ sowohl im Argument- als auch im
Ergebnistyp von `filterJson`. Im Ergebnis ist `Value` noch in ein
[`Maybe`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html#t:Maybe)
eingepackt. Der `Maybe`-Typ kennt zwei Alternativen: `Just x`, in diesem
Fall hat das Filtern das Ergebnis `x` produziert. Die andere Alternative
ist `Nothing`, dann hat das Filtern zu keinem Ergebnis geführt.

Der `Value`-Datentyp erlaubt eine Fallunterscheidung über die
Art der JSON-Daten. Die Funktion `filterJson` macht von dieser
Fallunterscheidung gebrauch, nämlich dann wenn der vorliegende
Pfadausdruck mit einem Propertynamen `p` beginnt.

* Liegt ein JSON-Objekt vor (also etwas von der Form `{"key1": value1, "key2": value2}`), dann landen
wir im Fall `J.Object m`, wobei `m` eine Hash-Map mit den Properties
ist. In diesem Fall schlagen wir `p` in der Hash-Map nach. Falls `p` drin
ist, machen wir mit dem restlichen Pfadausdruck und dem unter `p`
gespeicherten Wert weiter. Anderenfalls liefern wir `Nothing` als Ergebnis zurück.

* Liegt eine JSON-Array vor (also sowas wie
`[1, "foobar", {"name": "Stefan"}]`), dann filtern wir mittels des kompletten
Pfadausdrucks die Elemente des Arrays. Das [`mapMaybe`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html#v:mapMaybe) sorgt hier dafür,
dass nur erfolgreich gefilterte JSON-Werte im Ergebnis landen.

* In allen anderen Fällen liefert `filterJson` für einen nicht-leeren
Pfadausdruck `Nothing` zurück, als kein Ergebnis.

* Für einen leeren Pfadausdruck ist das Ergebnis die unveränderte Eingabe
(eingepackt in ein `Just`).

{% highlight haskell %}
filterJson :: JsonPath -> J.Value -> Maybe J.Value
filterJson path json =
    case path of
      [] -> Just json
      (p:ps) ->
          case json of
            J.Object m ->
                case HashMap.lookup p m of
                  Nothing -> Nothing
                  Just v -> filterJson ps v
            J.Array arr ->
                let newArr = V.fromList (mapMaybe (filterJson path) (V.toList arr))
                in Just (J.Array newArr)
            _ -> Nothing
{% endhighlight %}

So, jetzt können wir direkt mit dem Pretty-Printing weitermachen. Auch
hier verwenden wir eine Fallunterscheidung über die Form der JSON-Daten,
dieses Mal lernen wir aber nicht nur die Fälle `Object` und `Array`
sondern auch die übrigen Fälle `String`, `Number`, `Bool` und `Null`
kennen. Der Rückgabewert von `prettyJson` ist [`Doc`](http://hackage.haskell.org/packages/archive/pretty/latest/doc/html/Text-PrettyPrint-HughesPJ.html#t:Doc). Dieser Typ
für Ausgabedokumente stammt aus der Library
[pretty](https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint.html),
welche effiziente Operationen zum Layouten von textuellen Ausgaben
bereitstellt. Schauen wir uns nun `prettyJson` an.

{% highlight haskell %}
prettyJson :: J.Value -> Doc
prettyJson json =
    case json of
      J.Object m ->
          let elems = map prettyKv (HashMap.toList m)
          in braces (prettyList elems)
      J.Array arr ->
          let elems = map prettyJson (V.toList arr)
          in brackets (prettyList elems)
      J.String t -> prettyString t
      J.Number n -> text (show n)
      J.Bool b -> text (if b then "true" else "false")
      J.Null -> text "null"
    where
      prettyList l =
          nest 1 (vcat (punctuate comma l))
      prettyKv (k, v) =
          let combine x y =
                  if isStructured v
                  then x $$ (nest 1 y)
                  else x <+> y
          in (prettyString k <> text ":") `combine` prettyJson v
      prettyString t =
          text (show t)
      isStructured json =
          case json of
            J.Object _ -> True
            J.Array _ -> True
            _ -> False
{% endhighlight %}

* Bei einem JSON-Objekt formatieren wir zunächst die einzelnen
  Schlüssel-Wert-Paare mittels `prettyKv`. In dieser lokal definierten
  Funktion benutzen wir verschiedene Operatoren aus der pretty-Bibliothek:

  * [`$$`](http://hackage.haskell.org/package/pretty-1.1.1.2/docs/Text-PrettyPrint.html#v:-36--36-)
    hängt zwei `Doc`s durch ein Newline getrennt aneinander.
  * [`<+>`](http://hackage.haskell.org/package/pretty-1.1.1.2/docs/Text-PrettyPrint.html#v:-60--43--62-)
    hängt zwei `Doc`s durch ein Space getrennt aneinander.
  * [`<>`](http://hackage.haskell.org/package/pretty-1.1.1.2/docs/Text-PrettyPrint.html#v:-60--62-)
    hängt zwei `Doc`s ohne Trennzeichen einander.

  Interessant ist auch
  [`nest`](https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint.html#v:nest),
  was aus einem Dokument durch
  Vergrößern der Einrückungstiefe ein neues Dokument macht.

  Die Listen der formatierten Schlüssel-Wert-Paare (Variable `elem`)
  hat nun den Typ `[Doc]`. Um daraus ein `Doc` zu machen, fügen
  wir zuerst mittels
  [`puncuate`](https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint.html#v:punctuate)
  Kommata zwischen die einzelnen `Doc`s und benutzten dann
  [`vcat`](https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint.html#v:vcat)
  um die `Doc`s durch Newlines getrennt einanderzufügen. Zum Schluss
  packen wir das ganze mittels
  [`braces`](https://hackage.haskell.org/package/pretty-1.1.1.0/docs/Text-PrettyPrint.html#v:braces)
  noch in geschweifte Klammern `{ }` ein.

* Die Formatierung eines JSON-Arrays erfolgt nach denselben Prinzipien.

* Ein JSON-String wird mittels
  [show](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#v:show)
  formatiert, was in diesem Fall auch die Anführungszeichen und das
  Escaping umfasst.

* Die restlichen Fälle sind einfach.

Die Hauptarbeit ist getan, wir brauchen aber noch eine Funktion, die eine
einzelne Datei verarbeitet:

{% highlight haskell %}
processFile :: JsonPath -> FilePath -> IO ()
processFile filter file =
    if file == "-" then action stdin else withFile file ReadMode action
    where
      action handle =
          do bytes <- BS.hGetContents handle
             case J.decodeStrict bytes of
               Just v ->
                   case filterJson filter v of
                     Nothing -> return ()
                     Just outV -> putStrLn (show (prettyJson outV))
               Nothing ->
                   hPutStrLn stderr ("Error parsing JSON from " ++ file)
{% endhighlight %}

Falls der Dateiname `-` ist, verwenden wir direkt das
[`Handle`](http://hackage.haskell.org/package/base-4.7.0.1/docs/System-IO.html#t:Handle)
für `stdin`, ansonsten kümmert sich
[`withFile`](http://hackage.haskell.org/package/base-4.7.0.1/docs/System-IO.html#v:withFile)
um das Öffnen der Datei zu einem `Handle` (und natürlich auch um das
Schließen). In der lokalen Methode `action` lesen wir erst den Inhalt der
Datei mittels
[`hGetContents`](http://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString.html#v:hGetContents),
parsen dann die JSON-Daten mit
[`decodeStrict`](http://hackage.haskell.org/package/aeson-0.7.0.6/docs/Data-Aeson.html#v:decodeStrict),
wenden dann `filterJson` an und drucken schließlich den mittels
`prettyJson` erzeugten String.

Jetzt bleibt uns noch die eigentliche `main`-Funktion:

{% highlight haskell %}
main :: IO ()
main =
    do args <- getArgs
       if ("--help" `elem` args || "-h" `elem` args) then usage else return ()
       case args of
         "--test":rest -> runTests rest
         "--filter":filter:files -> doWork (parseFilter filter) files
         "--filter":[] -> usage
         files -> doWork [] files
    where
      parseFilter :: String -> JsonPath
      parseFilter str =
          T.splitOn "." (T.pack str)
      usage :: IO a
      usage =
          do hPutStrLn stderr ("USAGE: jsonpp [--test] [--filter EXPR] [FILE..]")
             exitWith (ExitFailure 1)
      doWork :: JsonPath -> [FilePath] -> IO ()
      doWork filter files =
          if null files
          then processFile filter "-"
          else mapM_ (processFile filter) files
      runTests :: [String] -> IO ()
      runTests args = htfMainWithArgs args htf_thisModulesTests
{% endhighlight %}

In der `main`-Funktionen sehen wir auch, dass wir bei Angabe der
Kommandozeilenoption `--test` unsere noch nicht vorhandenen Tests laufen
lassen. In der Variable `htf_thisModulesTests` sammelt der Präprozessor
des [HTF](https://hackage.haskell.org/package/HTF) Pakets alle im Modul
definierten Tests. Die Namen solcher Tests müssen mit `test_` beginnen,
damit sie automatisch gefunden werden. Hier sind die Definitionen der
Testfälle:

{% highlight haskell %}
--
-- Tests
--
test_filterJson =
    do assertEqual (Just $ unsafeParseJson "[36, 23]") (filterJson ["age"] sampleJson)
       assertEqual (Just $ unsafeParseJson "[\"Stefan\"]")
                   (filterJson ["name", "first"] sampleJson)
       assertEqual Nothing (filterJson ["name"] (J.Number 42))
       assertEqual (Just sampleJson) (filterJson [] sampleJson)

-- Falls das Parsen fehlschlägt bricht das Programm einfach ab. In den
-- Tests ist das OK.
unsafeParseJson :: T.Text -> J.Value
unsafeParseJson t =
    case J.decodeStrict (T.encodeUtf8 t) of
      Just v -> v
      Nothing -> error ("Could not parse JSON: " ++ T.unpack t)

sampleJson :: J.Value
sampleJson =
    unsafeParseJson
      (T.concat
        ["[{\"name\": {\"first\": \"Stefan\", \"last\": \"Wehr\"}, \"age\": 36},",
         " {\"name\": \"Max\", \"age\": 23}]"])

test_prettyJson =
    do assertEqual expected (show (prettyJson sampleJson))
    where
      expected =
          ("[{\"age\": 36.0,\n" ++
           "  \"name\":\n" ++
           "   {\"first\": \"Stefan\",\n" ++
           "    \"last\": \"Wehr\"}},\n" ++
           " {\"age\": 23.0,\n" ++
           "  \"name\": \"Max\"}]")
{% endhighlight %}

Wenn wir nun `jsonpp` mit der Option `--test` aufrufen, bekommen wir
folgende Ausgabe:

~~~
$ jsonpp --test
[TEST] Main:filterJson (src/JsonPretty.hs:111)
+++ OK (2ms)

[TEST] Main:prettyJson (src/JsonPretty.hs:130)
+++ OK (0ms)

* Tests:     2
* Passed:    2
* Pending:   0
* Failures:  0
* Errors:    0
* Timed out: 0
* Filtered:  0

Total execution time: 6ms
~~~

So, das war's für heute. Ich hoffe, das Lesen hat Ihnen Spaß gemacht. Ich
freue mich über Feedback jeglicher Art!
