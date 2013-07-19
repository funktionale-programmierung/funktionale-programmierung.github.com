---
layout: post
description: Beschreibung
title: "Imperatives Programmieren in Haskell"
author: joachim-breitner
tags: ["Haskell", "imperativ", "takeR"]
---

Die wohl wichtigste Datenstruktur in Haskell ist die Liste. Da sie als
einfach verkette Liste implementiert ist, arbeitet man meist nur am
vorderen Ende, und so ist es auch kein Wunder dass es zwar eine Funktion
`take :: Int -> [a] -> [a]` gibt, die die ersten *n* Elemente einer
Liste zurück gibt, aber kein entsprechendes Gegenstück, dass die letzten
*n* Elemente zurück gibt.

In diesem Artikel betrachten wir drei verschiedene Implementierungen
einer solchen Funktion: Eine naive, eine effiziente imperative und eine
effiziente funktionale. Dabei sehen wir insbesondere wie man imperative
Algorithmen in Haskell umsetzen kann.

<!-- more start -->

Damit dieser Artikel auch vollständig ausführbarer Code ist, zu Beginn
ein paar Deklarationen:

{% highlight haskell %}
{-# LANGUAGE ScopedTypeVariables #-}
module TakeR where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Criterion
import Criterion.Main
{% endhighlight %}

Die naive Implementierung
-------------------------

Eine Stärke von funktionaler Programmierung liegt darin, dass man eine
Aufgabe einfach durch das Zusammenstecken bereits vorhandener Bausteine
lösen kann. So lässt sich unsere gesuchte Funktion in einer Zeile
implementieren:

{% highlight haskell %}
takeRNaive :: Int -> [a] -> [a]
takeRNaive n = reverse . take n . reverse
{% endhighlight %}

Dieser Code ist zweifelsfrei korrekt, allerdings zeigt er recht
schlechtes Performance-Verhalten: Die übergebene Liste wird komplett
kopiert und im Speicher gehalten, bis alle Elemente ausgegeben sind.
Wenn der Programmierer sich auf das Speicherverhalten von Haskells
Bedarfsauswertung (*Lazy Evaluation*) verlässt ist das nicht in Ordnung,
und wir müssen nach besseren Alternativen suchen.

Im Folgenden werden wir unsere Implementierungen mit einem einfachen
Benchmark vergleichen: Wir messen, wie lange die Berechnung von
`takeR 100000 [1..100000]` und `takeR 100 [1..100000]` dauert. Die naive
Implementierung braucht dafür 9,9 ms bzw. 4,2 ms.

Die imperative Implementierung
------------------------------

Auch langjährige funktionale Programmierer denken manchmal in Schleifen
und Arrays statt in Rekursion und Listen, und so kam ich erst auf einen
imperativen Algorithmus. Da *n* Elemente zurückgegeben werden sollen,
arbeiten wir mit einem Array der Länge *n*. Diesen befüllen wir mit den
Elementen der Liste. Wenn wir hinten ankommen, machen wir vorne weiter
und überschreiben damit genau die Elemente, die wir sicher nicht mehr
brauchen. Sobald wir am Ende der Liste ankommen stehen genau die letzten
*n* Elemente im Array, und wir müssen sie nur noch in der richtigen
Reihenfolge auslesen.

Für solchen imperativen Code bietet sich die *ST-Monade* an; darin
lassen sich – wie auch in der IO-Monade – veränderliche Variablen
(`STRef`) und veränderliche Arrays (`MArray`) anlegen und bearbeiten.
Ein eleganter Trick des Typsystems garantiert, dass dabei die „Reinheit“
von Haskell erhalten bleibt: Die Funktion `runST` beschränkt die
Seiteneffekte auf diesen einen Aufruf, verschiedene Aufrufe von `runST`
können sich nicht gegenseitig beeinflussen.

{% highlight haskell %}
takeRArray :: forall a. Int -> [a] -> [a]
takeRArray n l | n <= 0 = []
takeRArray n l = runST stAction
  where
    stAction :: forall s. ST s [a]
    stAction = do
        buffer <- newArray_ (0, n-1)
        i <- go (buffer :: STArray s Int a) 0 l
        let s = min i n
        sequence $ [ readArray buffer (j `mod` n) | j <- [i-s..i-1] ]
    go buffer i [] = return i
    go buffer i (x:xs) = writeArray buffer (i `mod` n) x >> go buffer (i+1) xs
{% endhighlight %}

Die Funktion `stAction` legt also ein veränderliches Array der
angefragten Länge an. Die rekursive Funktion `go` implementiert die oben
beschriebene Schleife und befüllt das Array, wobei der Modulo-Operator
dafür sorgt, dass wir nach dem Ende des Arrays wieder an den Anfang
springen. Zuletzt bauen wir aus dem Array wieder die Ergebnisliste.

Der Code ist viel komplizierter als der für `takeRNavie`, und es gibt
etliche Stellen, an denen sich Off-By-One-Fehler oder ähnliche
Unachtsamkeiten hätten einschleichen können. Zum Glück erlaubt es uns
der QuickCheck-Ansatz (welchen wir für die Sprache Racket schon in einem
[früheren Artikel](http://funktionale-programmierung.de/2013/07/10/randomisierte-tests-mit-quickcheck.html)
besprochen haben)
mit minimalem Aufwand, die beiden Funktionen
auf hinreichend viel zufällig erzeugten Testdaten zu vergleichen, und

    quickCheck (\n l -> n <= 100000 ==> takeRNaive n l == takeRArray n (l::[Int]))

im Interpreter GHCi ausgeführt meldet uns ein beruhigendes
`+++ OK, passed 100 tests.`.

Die Funktion ist also wohl korrekt. Doch ist sie auch schnell? Die
Messung ergibt unerfreuliche 11 ms für die lange Ausgabeliste und sehr
erfreuliche 1,7 ms für die kurze. Die Verbesserung im zweiten Fall liegt
daran dass mit `takeRArray` die Elemente vom Anfang der Liste, von denen
man schon weiß dass sie nicht benötigt werden, freigegeben werden
können.

Aber warum haben sind wir uns im ersten Fall sogar verschlechtert? Will
man solchen Performance-Fragen auf den Grund gehen bietet es sich an,
den Computer mittels `-ddump-simpl` nach dem optimierten Code in der
Zwischensprache *Core* zu fragen. Dort kann man, mit etwas Übung,
erkennen dass der Aufruf von `mod` in `go` in *jeder* Iteration der
Schleife prüft, ob `n` vielleicht -1 oder 0 ist, um diese Sonderfälle
getrennt zu behandeln. Scheinbar ist der Compiler nicht clever genug zu
erkennen, dass hier immer `n > 0` gilt, oder zumindest den Check aus der
Schleife herauszubewegen. Wir können ihm auf die Sprünge helfen, in dem
wir diese beiden Fälle explizit behandeln:

{% highlight haskell %}
takeRArray' :: forall a. Int -> [a] -> [a]
takeRArray' n l | n <= 0 = []
takeRArray' (-1) l = []
takeRArray' 0 l = []
takeRArray' n l = runST stAction
  where
    stAction :: forall s. ST s [a]
    stAction = do
        buffer <- newArray_ (0, n-1)
        i <- go (buffer :: STArray s Int a) 0 l
        let s = min i n
        sequence $ [ readArray buffer (j `mod` n) | j <- [i-s..i-1] ]
    go buffer i [] = return i
    go buffer i (x:xs) = writeArray buffer (i `mod` n) x >> go buffer (i+1) xs
{% endhighlight %}

Damit schlagen wir jetzt auch im ersten Benchmark den naiven Code
(9,3 ms) und verbessern uns im zweiten Benchmark weiter (1,6 ms).

Gegen die Implementierung von `takeRArray` kann man jetzt einwenden dass
sie nicht nur Haskell-untypisch programmiert ist, sondern auch
untypisches Speicherverhalten zeigt: Da in Haskell alle Werte
unveränderlich sind, können Datenstrukturen – oder auch Teile davon –
zwischen verschiedenen Referenzen geteilt werden. So erstellt die
Funktion `tail :: [a] -> [a]` nicht etwa eine Kopie der Liste, sondern
gibt sozusagen einfach den `next`-Zeiger der ersten Listen-Zelle zurück.
Eigentlich sollte das auch für `takeR` möglich sein. Um solches
Verhalten mit `takeRArray` zu erzielen dürfen wir in unserem Array nicht
mehr nur die Einträge der Originalliste speichern, sondern müssen uns
ganze Listen merken:

{% highlight haskell %}
takeRArray2 :: forall a. Int -> [a] -> [a]
takeRArray2 n l | n <= 0 = []
takeRArray2 (-1) l = []
takeRArray2 0 l = []
takeRArray2 n l = runST stAction
  where
    stAction :: forall s. ST s [a]
    stAction = do
        buffer <- newArray_ (0, n)
        i <- go (buffer :: STArray s Int [a]) 0 l
        let s = min i n
        if s <= 0 then return [] else readArray buffer ((i-s) `mod` n)
    go buffer i [] = return i
    go buffer i l@(x:xs) = writeArray buffer (i `mod` n) l >> go buffer (i+1) xs
{% endhighlight %}

Der Speicheraufwand ist dadurch nicht relevant gestiegen, da ja die
Liste in einer Zelle die Restliste der Liste in der vorherigen Zelle ist
und somit nur einmal im Speicher liegt. Diese Verbesserung beschleunigt
den ersten Benchmark enorm: Wir sind bei 1,4 ms, ¼ der Originalzeit! Im
anderen Benchmark verschwindet der Vorteil in den Rundungsfehlern.

Fazit soweit: Man kann auch in Haskell imperativ programmieren. Dabei
muss man so schöne Eigenschaften wie das Typsystem, die kompakte Syntax
oder das Testen mit QuickCheck nicht aufgeben, und kann deutliche
Peformanceverbesserungen erzielen.

Allerdings ist der Code nicht mehr kompakt, weder einfach zu verstehen
noch schön, so dass sich die Frage stellt: Gibt es keine idiomatische
*und* effiziente Implementierung.

Die idiomatische Implementierung
--------------------------------

Und ja, es gibt sie. Die Erkenntnis von gerade eben, dass das Ergebnis
ein Teil der Liste sein sollte, legt nahe, dass eine solche
Implementierung die
Eingabeliste an zwei Stellen gleichzeitig betrachtet: Zum einen schaut
der Code, ob die Liste schon fertig ist, zum anderen hält er
gleichzeitig die Stelle der Liste fest, die er zurückgeben muss, wenn
die Liste tatsächlich fertig ist. So kommt man auf folgenden Code:

{% highlight haskell %}
takeRIdiomatic n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys
{% endhighlight %}

Hier sind wir wieder in der schönen Haskell-Welt: Kurzer Code, keine
fehlerträchtigen Array-Index-Berechnungen (und abgesehen von der
Berechnung, die `drop` machen muss, gar keine Arithmetik), keine
gesonderte Fehlerbehandlung nötig, gutes Speicherverhalten. Und er
schlägt sogar alle oben gezeigten Varianten: 0,9 ms für die lange und
0,4 ms für die kurze Ausgabe. Mit dieser Funktionsdefinition können wir
nun zufrieden sein.

Doch wie funktioniert das eigentlich? Um das zu verdeutlichen
visualisieren wir hier einen Aufruf von `takeRIdiomatic 4 [1..7]` mit
ASCII-Art:

Anfangs ist das Argument noch nicht ausgewertet (was wir als als
`[1..7]` schreiben):

    [1..7]
    └ takeRIdiomatic 4

Nun werten wir die Funktion aus. Dabei wird `go` mit zwei Argumenten
aufgerufen. (Aus Gründen der Ästhetik ist das erste Argument der Zeiger
auf der rechten Seite.)

    [1..7]
    ├──── drop 4
    └ go ─┘

Da `go` einen Pattern-Match auf den ersten Parameter durchführt müssen
wir `drop` auswerten. Drop gibt solange nichts zurück bis es die
angegebene Anzahl an Elementen gesehen hat, daher müssen wir die Liste
vier Elemente weit auswerten:

    1:[2..7]       1:2:[3..7]      1:2:3:[4..7]     1:2:3:4:[5..7]     1:2:3:4:[5..7]
    │ └── drop 3   │    └ drop 2   │      └ drop 1  │        └ drop 0  │        │
    └ go ─┘        └ go ──┘        └ go ────┘       └ go ──────┘       └── go ──┘

Nun kann `go` ans Werk gehen: Es pattern-matcht auf beide Argumente (was
die Liste weiter auswertet) und „schiebt die Zeiger weiter“. Dabei
können nun die ersten Elemente der Liste vom Garbage Collector
freigegeben werden.

    1:2:3:4:5:[6..7]  1:2:3:4:5:6:[7..7]  1:2:3:4:5:6:7:[]
      └─ go ──┘           └─ go ──┘             └─ go ──┘

Nach diesen Schritten hat `go` das Ende der Liste erreicht und gibt
einfach das zweite Argument zurück. Dieses enthält tatsächlich die
letzten vier Elemente der Liste und die konkreten Datenstrukturen im
Speicher werden von beiden Listen geteilt:

    1:2:3:4:5:6:[]
        ↑

Fazit
-----

Man kann in Haskell gut und erfolgreich imperativ programmieren, aber es
lohnt sich darüber nachzudenken, ob es nicht eine idiomatischere Lösung
gibt.

Benchmarks
----------

Um die Benchmarks nachzuvollziehen kompiliert man den Code in diesem Artikel mit
`ghc -O2 --make TakeR.lhs -main-is TakeR` und der folgenden
`main`-Funktion:

{% highlight haskell %}
main = defaultMain [
    bgroup (show m ++ " taken") [
        bench "naive"     (nf (flip takeRNaive     [1..n]) m), 
        bench "array"     (nf (flip takeRArray     [1..n]) m), 
        bench "arrayopt"  (nf (flip takeRArray'    [1..n]) m), 
        bench "array2"    (nf (flip takeRArray2    [1..n]) m), 
        bench "ideomatic" (nf (flip takeRIdiomatic [1..n]) m)
    ]
    | let n = 100000 :: Int
    , m <- [100000, 100]
    ]
{% endhighlight %}



<!-- Local Variables: -->
<!-- mode: text -->
<!-- End: -->

