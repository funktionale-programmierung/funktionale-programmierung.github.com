---
layout: post
description: Wie man schwere algorithmische Probleme automatisch lösen lässt
title: "Einführung in die logische Programmierung mit Microsoft z3"
author: markus-schlegel
tags: ["z3", "Logische Programmierung", "Erste Schritte", "Einführung", "Projekt"]
---

Der Schritt von der imperativen Programmierung zur funktionalen
Programmierung ist auch ein Schritt hin zu einem deklarativeren
Programmierstil. Wir beschreiben die Lösung eines Problems, anstatt
den Computer Schritt für Schritt anzuweisen, welche konkreten Hebel er
ziehen und welche Knöpfe er drücken muss. Der erste Vorteil dieses
deklarativen Stils ist, dass er die Kommunikation zwischen den
beteiligten Menschen vereinfacht. Der zweite Vorteil ist, dass ein
beschreibender Programmierstil es besser erlaubt, auf
Anforderungsänderungen einzugehen.

Die funktionale Programmierung ist zwar deklarativer als die
imperative Programmierung, aber ein Konzept von Schritten finden wir
auch in der funktionalen Programmierung noch. Die Problemlösungen, die
wir in der funktionalen Programmierung beschreiben, bauen schließlich
meistens auf kleineren Lösungen auf. Wir sagen beispielsweise, dass
eine Liste von Zahlen komponentenweise verdoppelt werden kann, indem
wir den Kopf der Liste verdoppeln und den Rest der Liste
komponentenweise verdoppeln. Die beschriebene Lösung baut also auf
einer Teillösung auf. Sehr viele Probleme lassen sich so beschreiben.
Es gibt aber auch Probleme, die sich nicht so einfach in Teilprobleme
aufschneiden lassen. Man denke an klassische Rätselaufgaben wie
Sudoku. Bei Sudoku hängt beinahe alles mit allem zusammen. In jeder
Entscheidung müssen wir beachten, dass die Entscheidung einen Einfluss
auf fast alle anderen Felder des Spiels hat. In diesem Artikel werden
wir die logische Programmierung kennenlernen, um mit solchen schwer
greifbaren Problemen umzugehen.


<!-- more start -->

Das Spiel Sudoku ist schnell erklärt. Es gibt ein Spielfeld aus 3x3
Blöcken mit jeweils 3x3 Feldern. Die Felder bilden also 9 Reihen und 9
Spalten. Ziel des Spiels ist es, jedem Feld eine Zahl von 1 bis 9
zuzuweisen und zwar so, dass jede Zahl in jeder Reihe, jeder Spalte
und jedem Block nur einmal auftritt. Meistens sind einige Felder
bereits vorausgefüllt.

Die Beschreibung des Spielfelds hat uns zwei kurze Sätze gekostet. Die
Charakterisierung der Lösung konnten wir auch in einem Satz angeben.
Sudoku sollte also eigentlich mit einem deklarativen Programmierstil
leicht zu lösen sein. Aber selbst mit funktionaler Programmierung
müssen wir hier lange rätseln, um einen effizienten Algorithmus zu
finden. Das liegt, wie in der Einleitung bereits angedeutet, daran,
dass sich das Problem schwer in Teilprobleme aufteilen lässt. Hat der
deklarative Ansatz also versagt?


## Logische Programmierung

Vielleicht ist die funktionale Programmierung einfach noch nicht
deklarativ genug? Wenn wir von der funktionalen Programmierung noch
einen Schritt weiter in Richtung deklarative Programmierung gehen,
dann landen wir bei der logischen Programmierung. In der logischen
Programmierung bleibt uns gar nichts anderes übrig als deklarativ
vorzugehen. Es gibt einfach keine anderen Werkzeuge als die
Beschreibung. Wir werden deshalb im folgenden Sudoku mithilfe der
logischen Programmierung lösen, um zu sehen, wie praktikabel dieser
Ansatz ist. Zunächst müssen wir dazu verstehen, welche Theorie hinter
der logischen Programmierung steht.


## Die Aussagenlogik und das SAT-Problem

z3 ist ein sogenannter SAT-Solver. SAT steht für englisch
_Satisfiability_ und beschreibt eines der schwierigsten Probleme in
der theoretischen Informatik, das [Erfüllbarkeitsproblem der
Aussagenlogik](https://de.wikipedia.org/wiki/Erfüllbarkeitsproblem_der_Aussagenlogik).
Sudoku und viele andere Rätsel sind Abwandlungen dieses SAT-Problems.

Die Aussagenlogik besteht aus Variablen, Und-Verknüpfungen,
Oder-Verknüpfungen, Negation und den beiden Wahrheitswerten `true` und
`false`. Mit diesen Bausteinen können wir Ausdrücke formulieren. Ein
Ausdruck ohne Variablen kann direkt einem der Wahrheitswerte
zugeordnet werden. Wir können den Ausdruck sozusagen direkt
ausrechnen:

```
   (true oder false) und (false oder (nicht true))
=>       true        und (false oder false)
=>       true        und        false
=>                   false
```

True und true ergibt true, true und false (oder umgekehrt) ergibt
false, false und false ergibt false. True oder true ergibt true, true
oder false (oder umgekehrt) ergibt true, aber false oder false ergibt
false. Nicht true ergibt false, nicht false ergibt true. Wir rechnen
diese logischen Ausdrücke analog zu arithmetischen Ausdrücken von
innen nach außen aus.

Wenn Variablen ins Spiel kommen, können wir den Ausdrücken nicht mehr
einfach Wahrheitswerte zuweisen. Die Wahrheitswerte der Ausdrücke
hängen von den Wahrheitswerten der Variablen ab.

```
(x oder y) und (x und (nicht z))
```

Wir können uns aber eine ganz zentrale Frage stellen: Gibt es
überhaupt eine Lösung, bei der jeder Variablen ein fester Wahrheitswert
zugeordnet wird und der Gesamtausdruck zu true ausgewertet werden
kann? Und falls ja: welche Wahrheitswerte nehmen dann die Variablen an?

Das Beispiel oben hat mindestens eine gültige Zuordnung. Wenn wir für
x true, für y false und für z false einsetzen, wertet der Ausdruck zu
true aus. Wir nennen diese Zuordnung ein _Modell_ des Ausdrucks. Ein
Modell ist eine Variablenbelegung, die zeigt, dass die Probleminstanz
erfüllbar ist. Deshalb heißt das Problem _Erfüllbarkeitsproblem der
Aussagenlogik_.

In den kleinen Beispielen lassen sich Modelle noch recht einfach
finden. Im Zweifelsfall zählen wir einfach alle Kombinationen von
Wahrheitswertbelegungen für die Variablen auf und rechnen den Ausdruck
jeweils aus. Für drei Variablen gibt es nur 2 hoch 3, also 8 solcher
Kombinationen. Die Zahl der Kombinationen wächst aber exponentiell mit
der Zahl der Variablen. Bei 10 Variablen gibt es schon mehr als 1000
Kombinationen, die wir durchprobieren müssten. Die Menge der
Kombinationen entspricht hier dem Suchraum und der wird schon für
moderate Probleminstanzen enorm groß.


## Sudoku als aussagenlogisches Problem

Auch bei Sudoku könnten wir alle Zahlenkombinationen durchprobieren.
Es gibt insgesamt 9x9=81 Felder. Wenn wir annehmen, dass 21 Felder
schon vorausgefüllt sind, bleiben noch 60 Felder, die wir mit den
Zahlen von 1 bis 9 ausfüllen müssen. Das sind dann 10 hoch 60
Möglichkeiten. Viel Spaß!

Wir wählen einen anderen Weg. Wir formulieren ein gegebenes
Sudokuspielfeld als aussagenlogisches Programm und lassen dann z3
darauf los. z3 ist ein allgemeiner Problemlöser für SAT-Probleme. Die
Software wird von Microsoft Research entwickelt und enthält das Wissen
aus jahrzehntelanger Forschung zu dieser Klasse von schwierigen
Problemen. Entsprechend schnell kann z3 solche Probleme lösen.

### z3 und SMT-LIB 2

z3 nimmt SAT-Probleme im speziellen Format [SMT-LIB
2](http://smtlib.cs.uiowa.edu) entgegen. SMT-LIB 2 ist eine logische
Programmiersprache für SAT-Probleme. Ein SMT-LIB-2-Programm teilt sich
in drei Teile auf: Datentypdeklarationen, Variablendeklarationen und
Assertions. Betrachten wir zunächst eine abgespeckte Variante des
Sudokuspiels. Wir haben nur eine einzelne Reihe aus neun Felder, denen
wir jeweils unterschiedliche Ziffern zuweisen möchten. Wir können in
SMT-LIB zunächst die neun Ziffern mit einer Datentypdeklaration beschreiben:

```scheme
(declare-datatypes () ((Ziffer Eins Zwei Drei Vier Fuenf Sechs Sieben Acht Neun)))
```

Dieser Datentyp beschreibt eine geschlossene Aufzählung, ist also ein
Enum-Typ. Wir können für die neun Felder jetzt Variablen mit diesem
Typen anlegen:

```scheme
(declare-const x1 Ziffer)
(declare-const x2 Ziffer)
(declare-const x3 Ziffer)
(declare-const x4 Ziffer)
(declare-const x5 Ziffer)
(declare-const x6 Ziffer)
(declare-const x7 Ziffer)
(declare-const x8 Ziffer)
(declare-const x9 Ziffer)
```

Die Variablen können also die Werte `Eins`, `Zwei`, `Drei` und so
weiter annehmen. Jetzt möchten wir noch ausdrücken, dass die neun
Variablen alle unterschiedliche Werte erhalten sollen. Das können wir
mithilfe einer Assertion bewerkstelligen. Wir wollen zuerst bestimmen,
dass die Variablen `x1` und `x2` unterschiedliche Werte bekommen:

```scheme
(assert (not (= x1 x2)))
```

Diese Ungleichheit gilt auch für `x1` und `x3`. Wir können diese
Bedingung mit `and` an die erste Bedingung anhängen:

```scheme
(assert (and (not (= x1 x2))
             (not (= x1 x3))))
```

Wenn wir das bis zum bitteren Ende so weitermachen, werden wir zum
Hirsch. SMT-LIB 2 bietet deshalb eine Funktion `distinct`, um
ausdrücken zu können, dass mehrere Werte unterschiedlich sein
sollen:

```scheme
(assert (distinct x11 x12 x13 x14 x15 x16 x17 x18 x19))
```

z3 kann im Web direkt ausprobiert werden. Wir können unser Programm in
den [Online-Editor](https://rise4fun.com/Z3/) übertragen und von z3
ausführen lassen. Die Ausgabe lautet:

```scheme
sat
(model 
  (define-fun x13 () Ziffer
    Fuenf)
  (define-fun x16 () Ziffer
    Vier)
  (define-fun x17 () Ziffer
    Sechs)
  (define-fun x18 () Ziffer
    Sieben)
  (define-fun x14 () Ziffer
    Drei)
  (define-fun x12 () Ziffer
    Eins)
  (define-fun x19 () Ziffer
    Zwei)
  (define-fun x15 () Ziffer
    Neun)
  (define-fun x11 () Ziffer
    Acht)
  )
```

Wir sehen, dass z3 ein Modell gefunden hat. Das Modell sieht auch so
aus, wie wir uns das vorgestellt haben. Der Online-Editor ist sehr
hilfreich für iteratives Modellieren. Wir können zwischendurch immer
mal wieder schauen, ob die produzierten Ergebnisse auch unseren
Erwartungen entsprechen. Falls das nicht der Fall ist, haben wir einen
Fehler in der Spezifikation gemacht.

Um ein komplettes 9x9-Sudokufeld zu beschreiben, benötigen wir 81
Variablen. Wir nennen die Variablen für die Ziffern `xij` für `i` als
Zeilennummer und `j` als Spaltennummer. Für jede Zeile und Spalte
benötigen wir dann eine Assertion mit `distinct` wie oben. Wir
brauchen auch noch neun weitere `distinct`-Assertions für die neun
Blöcke. Zu guter Letzt werden einige Felder vorausgefüllt sein. Diese
Vorbelegungen müssen wir auch noch in Assertions packen. Wenn
beispielsweise das Feld in der dritten Zeile und sechsten Spalte eine
9 enthält, müssen wir folgende Assertion hinzufügen:

```scheme
(assert (= 9 x36))
```

Das komplette Programm ist sehr lang, weshalb wir es hier nicht
listen. z3 braucht für die Berechnung der Lösung des Programms nur
wenige Sekunden.
