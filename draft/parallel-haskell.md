---
layout: post
description: Parallele Programmierung mit Haskell
title: "Parallele Programmierung mit Haskell"
author: stefan-wehr
tags: ["parallel", "Haskell"]
---

Computerprozessoren werden heutzutage leider nicht mehr mit jeder neue Generation
schneller und schneller. Stattdessen konzentrieren sich die Chiphersteller
darauf mehr Prozessorkerne in unsere Rechner einzubauen.  Für die
Softwareentwicklung bedeutet dies dass Software nicht mehr automatisch mit
jeder Prozessorgeneration schneller läuft sondern dass die Entwickler
dafür etwas tun müssen.

Das Zauberwort heißt daher _parallele Programmierung_. Damit ist gemeint
dass verschiedene Teil eines Programms gleichzeitg ablaufen können, somit
mehrere Prozessorkerne auslasten und im Endeffekt schneller an's Ziel
kommen. Die automatische Parallelisierung von Software ist allerdings
immer noch ein Traum; stattdessen muss Parallelität von Hand in die Software
eingebaut werden. Funktionale Programmierung eignet sich hervorragend zur
parallelen Programmierung, insbesondere weil funktionale Programmierer
diszipliniert und sparsam mit Seiteneffekten umgehen und damit ein großes
Hindernis für Parallelität von Grund auf vermeiden.

Dieser Artikel demonstriert, wie man in der funktionalen Sprache
[Haskell](http://haskell.org) sehr einfach und elegant parallele Programme
schreiben kann. Insbesondere sind parallele Haskell Programme
*deterministisch*, d.h. sie liefern garantiert dasselbe
Ergebnis egal ob sie auf einem, zwei oder 32 Prozessorkernen laufen.
In traditionellen Sprache wird im Gegensatz dazu Parallelität häufig mit
Threads umgesetzt. Damit werden die Programme häufig schwer zu debuggen,
da sie z.B. auf einem Kern wunderbar funktionieren, auf zwei Kernen aber
das falsche Ergenbis berechnet wird und es auf vier Kernen hin und wieder
zu einem Deadlock kommt. Haskell Kentnisse sind zum Verständnis des
Artikels nicht nötig.

<!-- more start -->

Bevor wir richtig loslegen, noch eine kurze Abgrenzung von Parallelität
und Nebenläufigkeit. Wie oben geschrieben hat Parallelität zum Ziel ein
Programm schneller zu machen. Nebenläufigkeit hingegen ist eine inherente
Eigenschaft eines Programm und dient dazu, gleichzeitig mehrere
Interaktionen mit der Außenwelt führen zu können. Ein nebenläufigkes Programm
ist damit zwangsläufig nicht-deterministisch, wohingegen parallele Programme
durchaus deterministisch sein können und es in Haskell auch sind.

So, jetzt aber zu unserem konkreten Beispiel. Ich möchte in diesem Artikel
zeigen wie man in Haskell eine paralleles Programm zum Lösen von Sudokurätseln
entwickeln kann. Das Programm soll dabei auf beliebig viele Prozessorkerne
skalieren und die vorhandenen Kerne möglichst effizient ausnützen. Das gezeigte
Beispiel basiert auf einem [Tutorial](http://community.haskell.org/~simonmar/slides/CUFP.pdf)
von Simon Marlow. Ich habe den Code mit dem [Glasgow Haskell Compiler](http://haskell.org/ghc)
in Version 6.2.1 getestet.

In einem ersten Schritt schreiben wir ein sequentielles Programm. Wir
importieren zunächst das Module `Sudoko`, welches eine Funktion `solve`
zum Lösen eines Sudokus bereitstellt. Außerdem importieren wir ein Drivermodule
sowie ein Standardmodule.

{% highlight haskell %}
-- Datei SudokuSeq.hs
import Sudoku
import Driver

import Data.Maybe
{% endhighlight %}

Ein Sudokurätsel der Dimension 9x9 wird als eine 81 Zeichen langer String
repräsentiert, wobei ein `.` für ein initial leeres Feld steht und eine Zahl
die entsprechende Vorbelegung darstellt. Mehrere Sudokurätsel werden zeilenweise
in einer Datei abgelegt, etwa so:

    .......2143.......6........2.15..........637...........68...4.....23........7....
    .......241..8.............3...4..5..7.....1......3.......51.6....2....5..3...7...
    .......24....1...........8.3.7...1..1..8..5.....2......2.4...6.5...7.3...........

Das Drivermodule stellt eine Funktion `driver` zur Verfügung.
Diese Funktion übernimmt für uns das Parsen der Kommandozeilenargumente,
das Einlesen der Eingabedatei sowie das Extrahieren der einzelnen Rätsel
aus dieser Datei. Wir müssen `driver` lediglich mit einer Funktion zum
Lösen aller Sudokurätsel aufrufen.

Das restliche sequentielle Programme sieht dann wie folgt aus:

{% highlight haskell %}
main = driver computeSolutions
    where
      computeSolutions puzzles =
        filter isJust (map solve puzzles)
{% endhighlight %}

Das Argument `puzzles` von `computeSolutions` ist eine Liste von Strings, also
eine Liste von Sudokurätseln.
Der Ausdruck `map solve puzzles` wendet die `solve` Funktion auf jedes dieser Rätsel an.
Der Typ von `solve` ist `String -> Maybe Grid`, so dass wir dann mit `filter isJust`
aus der Ergebnisliste die echten Lösungen herausfiltern können. Da wir hier nicht
an den Lösungen an sich interessiert sind sondern nur die Zeit verbessern wollen die
es zum Berechnen einer Lösung braucht, geben wir ganz am Ende lediglich die
Anzahl der Lösungen aus. Jetzt können wir das Programm mit folgendem Befehl kompilieren:

    ghc --make -O2 -threaded -rtsopts SudokuSeq.hs

Der komplette Code zu diesem Artikeln kann auch [heruntergeladen](/files/FIXME) werden.
In dem .zip Archiv befindet sich auch eine Datei `sudoku17.1000.txt` welche 1000
Rätseln mit jeweils 17 vorbelegten Feldern enthält. Wenn wir nun das kompilierte
`SudokuSeq` mit dieser Datei aufrufen erhalten wir als Ausgabe tatsächlich `1000`.
Da wir an der Laufzeit interessiert sind müssen wir dem Aufruf noch ein paar
extra Argument spendieren:

    ./SudokuSeq +RTS -s -RTS sudoku17.1000.txt

Jetzt erhalten wir vom Laufzeitsystem des Haskell Programms noch folgende Zusatzinformation:

       2,362,896,368 bytes allocated in the heap
          38,873,144 bytes copied during GC
             241,816 bytes maximum residency (15 sample(s))
              79,912 bytes maximum slop
                   2 MB total memory in use (0 MB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
      Gen  0      4573 colls,     0 par    0.06s    0.07s     0.0000s    0.0001s
      Gen  1        15 colls,     0 par    0.00s    0.00s     0.0002s    0.0002s

      TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)

      SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

      INIT    time    0.00s  (  0.00s elapsed)
      MUT     time    1.46s  (  1.46s elapsed)
      GC      time    0.06s  (  0.07s elapsed)
      EXIT    time    0.00s  (  0.00s elapsed)
      Total   time    1.52s  (  1.53s elapsed)

      Alloc rate    1,622,000,569 bytes per MUT second

      Productivity  96.0% of total user, 95.4% of total elapsed

Das meist davon können wir ignorieren, interessant ist vorerst eigentlich nur die Zeile

      Total   time    1.52s  (  1.53s elapsed)

Hier wird die gesamte CPU Zeit des Programms mit 1,52 Sekunden angegeben. Die komplette Laufzeit
("wall time") betrug 1,53 Sekunden. Nicht wirklich überraschend, schließlich handelt es sich um ein
sequentielles Programm.

Wir möchten nun dieses Programm parallelisieren. In Haskell stehen dazu mehrere Möglichkeiten
zur Verfügung. Wir beschäftigen uns in diesem Artikel mit einer Möglichkeit welche Parallelität
explizit einführt und Datenabhängigkeiten über spezielle _Boxen_ repräsentiert. In einem
späteren Artikel werde ich auch noch auf eine weitere, implizitere Möglichkeit eingehen.

Wenn Sie die folgenden Beispiel ausprobieren möchten, müssten Sie zunächst das
Paket `monad-par` installieren. Am einfachsten geschieht dies über den Befehl

    cabal install monad-par

Unsere erste Version des parallelen Sudoku Solvers sieht so aus:

{% highlight haskell %}
-- Datei SudokuPar1.hs
import Sudoku
import Driver

import Data.Maybe
import Control.Monad.Par

main = driver computeSolutions
    where
      computeSolutions puzzles =
          let (as, bs) = splitAt (length puzzles `div` 2) puzzles
          in runPar (do b1 <- new
                        b2 <- new
                        fork (put b1 (map solve as))
                        fork (put b2 (map solve bs))
                        res1 <- get b1
                        res2 <- get b2
                        return (filter isJust (res1 ++ res2)))
{% endhighlight %}

Um die Funktionalität des Pakets `monad-par` nutzen zu können ist der Import
von `Control.Monad.Par` hinzugekommen. Außerdem ist die `computeSolutions` deutlich
komplizierter geworden. Mittels `splitAt` teilen wir zunächst die Eingabe in zwei
etwa gleich große Teile `as` und `bs` auf. Auf diesen beiden Teil soll
nun die Lösung parallel berechnet werden. Dazu bedienen wir uns folgender
Konstrukte aus dem `Control.Monad.Par` Modul:

 * `new` Legt eine neue "Box" an in der später das Ergebnis einer parallelen Berechnung
   gespeichert wird. Da wir zwei Berechnungen parallel ausführen möchten legen
   wir zwei solcher Boxen `b1` und `b2` an.

 * `put` Speichert das Ergebnis einer Berechnung in einer Box. Damit parallele Berechnungen
   deterministisch bleiben ist es verboten ein Ergebnis in eine volle Box zu schreiben.

 * `get` Holt das Ergebnis einer Berechnung aus einer Box. Falls die Box leer ist wartet
   `get` solange bis ein Ergebnis vorliegt.

 * `fork` Stößt eine Berechnung an die dann parallel abläuft. In unserem Beispiel
   benutzen wir `fork` um zwei parallele Berechnungen zu starten, die ihre Ergebnisse
   in die Boxen `b1` und `b2` schreiben. Während die Berechnungen noch laufen
   warten wir mittels `get` bis die Ergebnisse in `b1` und `b2` vorliegen.

 * `runPar` Um aus der Welt der parallelen Berechnungen das Ergebnis in die Welt der "normalen"
   Haskell Berechnungen zurückzureichen müssen wir die parallele Berechnung auf oberster
   Ebene in ein `runPar` einpacken. In unserem Fall ist das Ergebnis von `runPar`
   das Argument des `return` Aufrufs in der letzten Zeile, also
   `(filter isJust (res1 ++ res2))`.

Schauen wir uns un mal die sequentielle Performance unserer ersten parallelen Version an.
Dies ist immer eine gute Prüfung um sich zu vergewissern dass man nicht völligen
Blödsinn programmiert hat:

    ./SudokuPar1 +RTS -s -RTS sudoku17.1000.txt
      Total   time    1.52s  (  1.54s elapsed)

Wir erinner uns, die sequentielle Version benötigt für denselben Benchmark insgesamt 1,53
Sekunden, es scheint also alles zu passen. Um nun mehrere Prozessorkerne zu verwenden
müssen wir dem Haskell Laufzeitsystem (RTS) die Option `-N<K>` mitgeben, wobei
`<K>` die Anzahl der Kerne ist:

    ./SudokuPar1 +RTS -s -N2 -RTS sudoku17.1000.txt
      Total   time    1.57s  (  0.97s elapsed)

Aha, jetzt brauchen wir nur noch 0,97 Sekunden anstatt 1,53 Sekunden im sequentiellen Fall.
Der Speedup berechnet sich wie üblich als Quotient dieser beiden Zeiten, also
`1,53 / 0,97 = 1,55`. Das ist leider nicht ganz das was wir erwartet haben, denn
da das Problem fast vollständig parallelisierbar ist sollte der Speedup bei zwei Kernen irgendwo
knapp unterhalb von 2 liegen.

Um dem Problem auf den Grund zu gehen können wir während des Programmlaufs ein
Eventlog erstellen, das wir danach analysieren können. Dazu müssen wir das
Programm mit der `-eventlog` Flag neu kompilieren

    ghc --make -O2 -threaded -rtsopts -eventlog -o SudokuPar1_e SudokuPar1.hs

und dann mit der RTS Option `-ls` nochmals ausführen:

    ./SudokuPar1_e +RTS -s -N2 -ls -RTS sudoku17.1000.txt

Dann benutzen wir [threadscope](FIXME) um das Eventlog `SudokuPar1_e.eventlog`
zu analysieren.


<div id="center">
<img src="/files/parallel-haskell/threadscope.png">
</img>
</div>

<br/>

Offensichtlich laufen die zwei Kerne nach einer kurzen Startphase schön parallel,
aber noch etwas mehr als der Hälfte der Zeit geht einem Kern die Arbeit aus
und es ist nur noch ein Kern aktiv. Das Problem ist offensichtlich, dass
das Lösen zweier Sudokurätsel unterschiedlich lange dauern kann und dass unsere
Strategie, die Liste der Rätsel einfach in zwei Hälften zu teilen diesen
Aspekt nicht berücksichtigt.

Schauen wir uns auch noch an was unsere erste parallele Version macht wenn wir ihr vier Kerne
zur Verfügung stellen:

    ./SudokuPar1 +RTS -s -N4 -RTS sudoku17.1000.txt
      Total   time    1.80s  (  1.03s elapsed)

Oh, die Performance ist sogar ein klein wenig schlechter geworden, obwohl meine Maschine
sogar acht Kerne hat. Die etwas schlechtere Performance liegt am größeren Overhead
der durch die Verteilung auf mehr Kerne entsteht.
Es sollte aber auch einleuchtend sein dass wir mit unserer
fixen Aufteilung der Sudokurätsel in zwei Teile niemals mehr als zwei Kerne
beschäftigen können.

Wir benötigen daher eine flexiblere Strategie zur Aufteilung der Sudokurätsel
in parallele Berechnungen. Wir bedienen uns dabei eines Ansatzes der sich
*dynamische Partitionierung* nennt. Die Idee ist dass wir die Sudokurätsel
in viele kleine Einheiten aufteilen, und dass sich das Laufzeitsystem von Haskell
darum kümmert diese kleinen Einheiten in sinnvolle Arbeitspakete für einen Prozessorkern
zusammenzupacken. Mit diesem Ansatz lösen wir beide Probleme die mit unserer
ersten Version aufgetreten sind:

 * Es ist egal wie groß die einzelnen Teilprobleme sind. Wenn ein Prozessorkern
   nichts mehr zu tun hat schiebt ihm das Laufzeitsystem neue Arbeit zu.

 * Wir müssen nicht im voraus die Anzahl der zur Verfügung stehenden Prozessorkerne
   planen, denn das Laufzeitsystem kümmert sich um diesen Aspekt.

{% highlight haskell %}
-- Datei SudokuPar2.hs
import Sudoku
import Driver

import Data.Maybe
import Control.Monad.Par

main = driver computeSolutions
    where
      computeSolutions puzzles =
          filter isJust (runPar (parMap solve puzzles))
{% endhighlight %}

*Hinweise für Haskell Anfänger:
Das syntaktische Konstrukt `str <- readFile f` führt die Aktion `readFile f` aus und bindet
das Ergebnis an die Variable `str`. Im Fall `[f] <- getArgs` liefert `getArgs` eine Liste
zurück, wobei wir mit `[f]` das Erste Element der Liste an die Variable `f` binden.
Die beiden Aktion `getArgs` und `readFile` interagieren beide mit der externen Welt, daher
ist hier die Benutzung von `<-` erforderlich. Im Gegensatz dazu binden wir
in dem `let` Ausdruck lediglich die Ergebnisse von seiteneffektfreien Berechnungen
an die Variablen `puzzles` und `solutions`.*


    ./SudokuPar2 +RTS -s -RTS sudoku17.1000.txt
      Total   time    1.51s  (  1.53s elapsed)

    ./SudokuPar2 +RTS -s -N2 -RTS sudoku17.1000.txt
      Total   time    1.54s  (  0.78s elapsed)

    ./SudokuPar2 +RTS -s -N4 -RTS sudoku17.1000.txt
      Total   time    1.56s  (  0.41s elapsed)

    ./SudokuPar2 +RTS -s -N8 -RTS sudoku17.1000.txt
      Total   time    2.60s  (  0.35s elapsed)

    ./SudokuSeq +RTS -s -RTS sudoku17.16000.txt
      Total   time   22.16s  ( 22.30s elapsed)

    ./SudokuPar2 +RTS -s -N4 -RTS sudoku17.16000.txt
      Total   time   22.79s  (  5.91s elapsed)

    ./SudokuPar2 +RTS -s -N8 -RTS sudoku17.16000.txt
      Total   time   36.21s  (  4.87s elapsed)
