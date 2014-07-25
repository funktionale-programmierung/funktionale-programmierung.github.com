---
layout: post
description: Haskell für Einsteiger, Teil 1
title: "Haskell für Einsteiger, Teil 1"
author: stefan-wehr
tags: ["Haskell"]
---

Mit dem heutigen Blogartikel möchte ich eine kleine Artikelserie mit dem
Titel "Haskell für Einsteiger"
beginnen. Die Serie richtet sich an Leser mit Programmiererfahrung, die
Lust auf Haskell haben, bisher aber den Einstieg in die Sprache nicht richtig
geschafft haben.

Ich werde versuchen, Ihnen durch praxisnahe Beispiel und direkt
lauffähigem Code den Einstieg zu erleichtern. Um loszulegen benötigen Sie
lediglich eine Installation der
[Haskell Platform](https://www.haskell.org/platform) sowie ein Checkout
des
[git Repositories](https://github.com/funktionale-programmierung/haskell-for-beginners.git)
zu dieser Artikelserie. Wir werden hin und wieder auch ausgewählte Paket über
[hackage](https://hackage.haskell.org/), dem Haskell-Paket-Repository, installieren.

Ich werde mir Mühe geben, viele Dinge detailliert zu erklären. Allerdings
würde es den Rahmen dieses Blogs sprengen, auf jedes Detail
einzugehen. Hierzu sei das Studium des einen oder
anderen Haskell [Tutorials](http://learnyouahaskell.com/chapters) oder
[Buchs](http://www.realworldhaskell.org/) empfohlen. Natürlich können Sie
Rückfragen auch als Kommentar zu diesem Artikel stellen.

<!-- more start -->

Im heutigen Artikel implementieren wir eine abgespeckte Version des
Unix-Tools `tail`. Dieses Kommandozeilenprogramm zeigt die letzten Zeilen
einer Datei an; per Default werden die letzten 10 Zeilen angezeigt, über
die Kommandozeilenoption `-n N` kann die Zeilenanzahl aber auch auf die
Zahl `N` gesetzt werden.

Um zu schauen, ob Ihre Haskell Installation funktioniert, sollten Sie
spätestens jetzt ein Checkout des
[git Repositories](https://github.com/funktionale-programmierung/haskell-for-beginners.git)
machen. Dann führen Sie im Verzeichnis `haskell-for-beginners` folgende
Befehle aus:

~~~
$ cabal install --only-dependencies  # installiert zusätzliche Pakete
$ cabal build                        # baut das Programm
~~~

Wenn alles gut geht, finden Sie jetzt das kompilierte Programm unter
`dist/build/tail/tail`. Wenn Sie nun dieses Programm mit der Option
`--help` aufrufen, sollten Sie folgende Ausfgabe sehen:

~~~
$ dist/build/tail/tail --help
USAGE: tail [-n N] [FILE ...]
~~~

So, jetzt schauen wir uns aber den Code unserer tail-Implementierung
an. Sie finden den Code in `src/Tail.hs`. Wir starten zunächst mit den
Imports für eine Datenstruktur für Sequenzen. Das Besondere an dieser
Datenstrukturen ist, dass man ein neues Element links oder rechts an eine
Sequenz in Zeit O(1) anhängen kann. (Wir könnten auch normale Listen
verwenden. Allerdings hat hier das Anhängen an das rechte
Ende einer Liste lineare Laufzeit.)

<div class="highlight"><pre><code class="haskell"><span
class="kr">import</span> <span class="k">qualified</span> <span
class="nn"><a href="http://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-Sequence.html">Data.Sequence</a></span> <span class="k">as</span> <span class="n">Seq</span> <span class="c1"></span>
<span class="kr">import</span> <span class="k">qualified</span> <span
class="nn"><a hef="https://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Foldable.html">Data.Foldable</a></span> <span class="k">as</span> <span class="n">F</span> <span class="c1">-- nötig für toList auf Sequenzen</span>
</code></pre></div>

Das Modul `Safe` benötigen wir, um beim Parsen der Kommandozeilenargument
das Argument der Option `-n` in eine Zahl zu konvertieren. Haskell hat zwar
standardmäßig eine `read`-Funktion für diesen Zweck, aber hierbei führt
das Fehlschlagen der Konvertierung direkt zu einem Programmabsturz. Das
`Safe`-Modul stellt eine bessere Variante der `read`-Funktion zur
Verfügung.

<div class="highlight"><pre><code class="haskell"><span
class="kr">import</span> <span class="nn"><a href="https://hackage.haskell.org/package/safe-0.3.7/docs/Safe.html">Safe</a></span> <span class="c1"></span>
</code></pre></div>

Als nächstes folgen Module, die uns Zugriff auf die
Kommandozeilenargumente, auf Funktionen zum Beenden des Programms sowie
auf Funktionalität zur Ein-/Ausgabe bieten.

<div class="highlight"><pre><code class="haskell"><span
class="kr">import</span> <span class="nn"><a href="https://hackage.haskell.org/package/base-4.2.0.0/docs/System-Environment.html">System.Environment</a></span> <span class="c1"></span>
<span class="kr">import</span> <span class="nn"><a href="https://hackage.haskell.org/package/base-4.2.0.0/docs/System-Exit.html">System.Exit</a></span>        <span class="c1"></span>
<span class="kr">import</span> <span class="nn"><a href="https://hackage.haskell.org/package/base-4.3.1.0/docs/System-IO.html">System.IO</a></span>          <span class="c1"></span>
</code></pre></div>

Das `ByteString`-Modul schließlich stellt Operation für den Umgang mit
Byte-Arrays zur Verfügung.

<div class="highlight"><pre><code class="haskell"><span
class="kr">import</span> <span class="k">qualified</span> <span
class="nn"><a href="https://hackage.haskell.org/package/bytestring-0.10.4.0">Data.ByteString</a></span> <span class="k">as</span> <span class="n">BS</span>
</code></pre></div>

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

Jetzt können wir mit der eigentlichen Implementierung loslegen. Wir
starten mit der Funktion `printTail`, welche die letzten `lines` Zeile
einer Datei `file` ausgibt.

{% highlight haskell %}
printTail :: Int -> FilePath -> IO ()
printTail lines file =
    if file == "-"
    then doWork stdin
    else withFile file ReadMode doWork
    where
      doWork :: Handle -> IO ()
      doWork h =
          do list <- collectLines lines h
             mapM_ printLine list
      printLine bs =
          do BS.hPut stdout bs
             BS.hPut stdout newline
      newline = BS.pack [10]
{% endhighlight %}

Die `printTail`-Funktion öffnet erst die zu lesende Datei, dabei wird `-` als
Standard-Input interpretiert (wie üblich). Für echte Dateien benutzten wir die Funktion
`withFile`. Diese Funktion öffnet eine Datei, führt mit der geöffneten
Datei die übergebenen Aktion aus (hier: `doWork`) und stellt dann sicher,
dass die Datei nach Abschluss der Aktion oder im Fehlerfall geschlossen
wird.

Die Funktion `doWork` nimmt dann eine `Handle`, welches in Haskell eine
geöffnete Datei repräsentiert. Mittels der noch zu implementierenden
Funktion `collectLines` werden die letzten `lines` Zeilen aus der Datei
extrahiert und anschließend mit der Hilfsfunktion `printLine`
ausgegeben. Der Aufruf `mapM_ printLine list` ruft dabei die
`printLine`-Funktion für jedes Element der Liste `list` auf.

Noch ein Wort zum Typ von `printTail`: in der Typsignatur zu Beginn der
Funktion legen wir diesen auf `Int -> FilePath -> IO ()` zurück, die
Funktion nimmt also zwei Argumente vom Typ `Int` und `FilePath` und hat
als Rückgabetyp `IO ()`. Der Rückgabetyp bedeutet, dass die Funktion
Seiteneffekte hat (der `IO` Teil des Typs) und kein Ergebnis liefert (der
`()` Teil des Typs, gesprochen "Unit").

In der `collectLines`-Funktion sammeln wir nun die richtigen Zeilen aus
der Datei auf. Dazu benutzen wir die Datenstruktur aus `Data.Sequence`. Zu
Anfang ist die Sequenz leer (`Seq.empty`). Dann hängen wir jede Zeile der
Datei rechts an die Sequenz mittels `Seq.|>` an, und sorgen anschließend
dafür, dass überschüssige Zeilen am linken Ende entfernt werden. Dazu
benutzen wir `Seq.viewl`, um einen "View" auf das linke Ende zu
bekommen. Wenn das linke Ende leer ist (`Seq.EmptyL`) ist das ein Bug,
anderenfalls matchen wir mittels `_ Seq.:< rest` auf das erste Element `_`
und den `rest`.

{% highlight haskell %}
collectLines :: Int -> Handle -> IO [BS.ByteString]
collectLines lines handle = loop Seq.empty
    where
      loop acc =
          do isEof <- hIsEOF handle
             if isEof
             then return (F.toList acc)
             else do l <- BS.hGetLine handle
                     loop (trim (acc Seq.|> l))
      trim seq =
          if Seq.length seq <= lines
          then seq
          else case Seq.viewl seq of
                 Seq.EmptyL -> error ("Bug in tail: argument to -n should not be negative")
                 _ Seq.:< rest -> rest
{% endhighlight %}

Da wir keine Annahme über das Encoding der zu lesenden Daten machen
wollen, lesen wir rohe Byte-Arrays (Type `BS.ByteString`) aus der Datei und
verzichten auf eine Konvertierung nach `String`.

Die hier vorgestellte Implementierung von `collectLines` ist sicher nicht
die effizienteste, da wir immer die komplette Datei lesen. Die
C-Implementierung von `tail` unter BSD ist hier z.B. wesentlich schlauer,
denn hier wird mit
[Memory-Mapped IO](http://en.wikipedia.org/wiki/Memory-mapped_file) vom
Ende der Datei her immer mehr Zeilen in den Speicher geladen, bis die
gewünschte Anzahl Zeilen erreicht ist. So etwas ist natürlich auch mit
Haskell möglich, es gibt z.B. auf
[Hackage](https://hackage.haskell.org/package/mmap) einen Wrapper für den
`mmap`-Systemcall. Falls ein Leser es wünscht, liefere ich gerne eine
effizientere Implementierung von `collectLines` mittels `mmap` nach, in
diesem Falle bitte einen entsprechenden Kommentar schreiben.

Als nächstes widmen wir uns dem Parsen der Kommandozeilenargumente. Dazu
definieren wir zunächste einen Datentyp für die Optionen, die wir
erwarten:

{% highlight haskell %}
data TailOpts
    = TailOpts
      { to_files :: [FilePath]
      , to_lines :: Int
      }
{% endhighlight %}

Das Parsen der Kommandzeilenargument funktioniert über Pattern-Matching
auf der Liste der Argumente:

{% highlight haskell %}
parseArgs :: [String] -> IO TailOpts
parseArgs args =
    if "-h" `elem` args || "--help" `elem` args
    then usage
    else
        case args of
          ("-n" : nStr : rest) ->
              case readMay nStr of
                Just n | n >= 0 ->
                    return (TailOpts { to_files = rest, to_lines = n })
                _ ->
                    abort "Argument to -n must be a non-negative int."
          [] -> return (TailOpts { to_files = ["-"], to_lines = defaultLines })
          _ -> return (TailOpts { to_files = args, to_lines = defaultLines })
    where
      usage = abort "USAGE: tail [-n N] [FILE ...]"
      defaultLines = 10
      abort msg =
          do hPutStrLn stderr msg
             exitWith (ExitFailure 1)
{% endhighlight %}

In der `main`-Funktion bauen wir schließlich alles zusammen:

{% highlight haskell %}
main =
    do args <- getArgs
       opts <- parseArgs args
       mapM_ (printTail (to_lines opts)) (to_files opts)
{% endhighlight %}

Damit haben wir unsere abgespeckte Version des Unix-Tools `tail`
abgeschlossen. Ich freue mich auf Ihr Feedback!
