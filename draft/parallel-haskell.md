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
zum Lösen eines Sudokus bereitstellt. Außerdem importieren wir zwei
Standardmodule.

{% highlight haskell %}
-- Datei SudokuSeq.hs
import Sudoku

import System.Environment
import Data.Maybe
{% endhighlight %}

Ein Sudokurätsel der Dimension 9x9 wird als eine 81 Zeichen langer String
repräsentiert, wobei ein `.` für ein initial leeres Feld steht und eine Zahl
die entsprechende Vorbelegung darstellt. Mehrere Sudokurätsel werden zeilenweise
in einer Datei abgelegt, etwa so:

    .......2143.......6........2.15..........637...........68...4.....23........7....
    .......241..8.............3...4..5..7.....1......3.......51.6....2....5..3...7...
    .......24....1...........8.3.7...1..1..8..5.....2......2.4...6.5...7.3...........

Das restliche sequentielle Programme sieht jetzt wie folgt aus:

{% highlight haskell %}
main =
    do [f] <- getArgs                -- Kommandozeilenargument abgreifen
       str <- readFile f             -- Datei einlesen
       let puzzles = lines str       -- Sudokurätsel aus `str` extrahieren
           solutions = computeSolutions puzzles
       print (length solutions)      -- Anzahl der Lösungen ausgeben
    where
      computeSolutions puzzles =
        filter isJust (map solve puzzles)
{% endhighlight %}

*Hinweise für Haskell Anfänger:
Das syntaktische Konstrukt `str <- readFile f` führt die Aktion `readFile f` aus und bindet
das Ergebnis an die Variable `str`. Im Fall `[f] <- getArgs` liefert `getArgs` eine Liste
zurück, wobei wir mit `[f]` das Erste Element der Liste an die Variable `f` binden.
Die beiden Aktion `getArgs` und `readFile` interagieren beide mit der externen Welt, daher
ist hier die Benutzung von `<-` erforderlich. Im Gegensatz dazu binden wir
in dem `let` Ausdruck lediglich die Ergebnisse von seiteneffektfreien Berechnungen
an die Variablen `puzzles` und `solutions`.*

Der Ausdruck `map solve puzzles` wendet die `solve` Funktion auf jedes Sudokurätsel an.
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

       2,362,898,072 bytes allocated in the heap
          38,911,992 bytes copied during GC
             241,944 bytes maximum residency (15 sample(s))
              79,784 bytes maximum slop
                   2 MB total memory in use (0 MB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
      Gen  0      4573 colls,     0 par    0.06s    0.07s     0.0000s    0.0002s
      Gen  1        15 colls,     0 par    0.00s    0.00s     0.0002s    0.0003s

      TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)

      SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

      INIT    time    0.00s  (  0.00s elapsed)
      MUT     time    1.37s  (  1.38s elapsed)
      GC      time    0.06s  (  0.07s elapsed)
      EXIT    time    0.00s  (  0.00s elapsed)
      Total   time    1.44s  (  1.45s elapsed)

      Alloc rate    1,718,772,579 bytes per MUT second

      Productivity  95.6% of total user, 94.8% of total elapsed

Das meist davon können wir ignorieren, interessant ist vorerst eigentlich nur die Zeile

      Total   time    1.44s  (  1.45s elapsed)

Hier wird die gesamte CPU Zeit des Programms mit 1,44 Sekunden angegeben. Die komplette Laufzeit
("wall time") betrug 1,45 Sekunden. Nicht wirklich überraschend, schließlich handelt es sich um ein
sequentielles Programm.

Wir möchten nun dieses Programm parallelisieren. In Haskell stehen dazu mehrere Möglichkeiten
zur Verfügung. Wir beschäftigen uns in diesem Artikel mit einer Möglichkeit welche Parallelität
explizit einführt und Datenabhängigkeiten über spezielle _Boxen_ repräsentiert. In einem
späteren Artikel werde ich auch noch auf eine weitere, implizitere Möglichkeit eingehen.

Wenn Sie die folgenden Beispiel ausprobieren möchten, müssten Sie zunächst das
Paket `monad-par` installieren. Am einfachsten geschieht dies über den Befehler

    cabal install monad-par

Unsere erste Version des parallelen Sudoku Solvers sieht so aus:

{% highlight haskell %}
{% endhighlight %}

Um die Funktionalität des Pakets nutzen zu können ist der Import von `Control.Monad.Par`
hinzugekommen. Außerdem ist die `computeSolutions` deutlich komplizierter geworden.

Wie so oft in Haskell werden parallele Berechnung mittels einer _Monade_ ausgedrückt.
Sie müssen an dieser Stelle aber nicht verstehen was eine Monade ist. Es sei nur so viel
gesagt dass ma
