---
layout: post
title: "Eine Einführung in Denotational Design Teil II: Zeitreihen"
author: markus-schlegel
tags: ["Denotational Design", "Modelling", "Formale Methoden", "Praxis"]
---

Dieser Artikel ist der zweite einer Serie von Artikeln über
Denotational Design.  Im [vorherigen Teil dieser Reihe][dd1] hatten
wir die theoretische Grundlage von Denotational Design kennen gelernt:
Die denotationelle Semantik. In diesem Artikel wollen wir dieses
Werkzeug nutzen, um eine konkrete Fachlichkeit zu spezifizieren.

## Denotational Design

Die denotationelle Semantik entstand wie gesagt zur Erforschung von
Programmiersprachen. Dieselbe Methode lässt sich aber ganz einfach auf
beliebige zusammengesetzte Programmelemente übertragen. Wir wollen
dies im Folgenden anhand von Zeitreihen illustrieren. Das Beispiel
kommt aus einem großen Industrieprojekt direkt aus unserer täglichen
Arbeit.

Grundlage unserer Überlegungen ist eine simple Architektur: Im Zentrum
steht unser System. Dieses speist sich mit Inhalten aus einem
Datenservice, welcher eine Fassade vor einer Menge von Sensoren
bildet. Die Dateninhalte sind im Wesentlichen Zeitreihen. Wir werden
später sehen, dass das nicht ganz präzise ist, aber für den Moment
können wir so darüber nachdenken. Unser System erlaubt dem Nutzer die
Visualisierung von Zeitreihen und die Beschreibung von
Transformationen von Zeitreihen. Aus der Domäne kommen nun einige
Anforderungen, welche Operationen auf und mit den Zeitreihen möglich
sein sollen:

* Einlesen vom Datenservice
* Einstellige Operationen wie abs/log/exp/...
* Mehrstellige Operationen wie Addition/Subtraktion/Multiplikation
* Interpolation bzw. Resampling
* Integration und Differentiation
* Glättung
* …

In den meisten Softwareprojekten sieht die "Anforderungsspezifikation"
genau so aus wie diese Liste. Dann wird _implementiert_. Das Wort
_implementieren_ erfordert aber eigentlich ein Objekt im Satz. _Was_
wollen wir überhaupt implementieren? Das ist bisher noch gar nicht
klar.

### Was ist eine Zeitreihe?

Beim Denotational Design starten wir mit der Frage: Was _bedeuten_ die
elementaren Bestandteile meiner Domäne, der Fachlichkeit, für die ich
Software schreiben möchte? In diesem Impetus deckt sich Denotational
Design noch mit der herkömmlichen funktionalen Programmierung, bei der
wir auch bestrebt sind, Objekte der Wirklichkeit als Daten und
Zusammenhänge als Funktionen zu modellieren. Für Zeitreihen könnten
wir einen solchen Versuch wagen und mit dieser Modellierung starten:

```haskell
type TimeSeries :: * -> *
lookup :: UTCTime -> TimeSeries a -> a
```

`TimeSeries` ist also ein simpler Typkonstruktur und `lookup` ist eine
Funktion, die diesen Typkonstruktur verwendet. Nach kurzem Nachdenken
kommen wir aber drauf, dass diese Modellierung nicht ganz richtig sein
kann. Zeitreihe, das klingt doch so, als gäbe es da nur an bestimmten
Zeitpunkten zugeordnete Werte. Die Signatur von `lookup` legt aber
nahe, dass wir für jeden beliebigen Zeitpunkt (`UTCTime`) einen Wert
rausbekommen könnten. Mit einer kleinen Korrektur landen wir bei:

```haskell
lookup :: UTCTime -> TimeSeries a -> Maybe a
```

Jetzt bekommen wir manchmal einen Wert und manchmal eben `Nothing`.

Woran haben wir diese Änderung in der Signatur von `lookup` eigentlich
festgemacht? Offensichtlich hatten wir eine Idee davon im Kopf, was
`TimeSeries` ist und was `lookup` tut. Diese Idee geht über die reinen
Signaturen hinaus und sie leitet auch die nachfolgende
Implementierung. Denn wenn wir nur der Signatur folgen würden, dann wäre
dies eine korrekte Implementierung:

```haskell
-- Eine vermutliche falsche Implementierung:
lookup _ _ = Nothing
```

Korrekt wäre diese Implementierung insofern, als das Programm
erfolgreich kompilierbar wäre. Die Implementierung ist aber alles
andere als korrekt in Bezug auf unsere Idee, wie `lookup`
_funktionieren_ soll. Wir müssten den Code korrigieren auf Basis
unserer Idee. Beim herkömmlichen Programmieren geben wir uns damit
zufrieden: Wir behalten die Idee im Hinterkopf und programmieren daran
entlang. Denotational Design zeigt eine alternative Vorgehensweise
auf: Wenn es diese abstrakte Idee gibt, dann können wir doch mal
versuchen, diese auch formal aufzuschreiben. Die denotationelle
Semantik gibt uns das Werkzeug, um diese Formulierung zu
bewerkstelligen: Wir müssen eine mathematische Struktur finden und
jede Operation auf eine entsprechende Operation in dieser Struktur
abbilden.

Um die Denotational-Design-Methode weiter zu motivieren, gehen wir
aber zunächst klassisch vor: Wir beginnen mit einer konkreten
Repräsentation unseres `TimeSeries a` Typen.

```haskell
type TimeSeries a = [(Time, a)]
```

Neben der Operation `lookup` ist eine Anforderung, dass wir Zeitreihen
addieren (oder subtrahieren oder multiplizieren) möchten. Wir
deklarieren also einen entsprechenden Zeitreihenkombinator:

```haskell
addTS :: TimeSeries Float -> TimeSeries Float -> TimeSeries Float
```

Spätestens bei der Implementierung dieser Funktion fällt aber auf,
dass noch gar nicht ganz klar ist, was eine solche Addition überhaupt
bedeuten soll. In dem Fall, dass sich die beiden Eingangszeitreihen in
ihren Zeitstempeln decken, sollen wohl die Punkte addiert werden:

```haskell
addTS [(t1, 4), (t2, 7)] [(t1, 9), (t2, 14)]
-- => [(t1, 13), (t2, 21)]
```

Was soll aber passieren, wenn es Tupel gibt, die keinen Gegenspieler
haben? Eine Möglichkeit wäre, diesen Fall als Programmierfehler
anzusehen und mit `error` abzustürzen.

```haskell
addTS [(t1, 4), (t2, 7)] [(t1, 9), (t3, 42)]
-- => Error!
```

Wir könnten auch die Punkte ohne Gegenspieler ignorieren.

```haskell
addTS [(t1, 4), (t2, 7)] [(t1, 9), (t3, 42)]
-- => [(t1, 13)]
```

Oder wir könnten die Punkte ohne Gegenspieler as-is mit ins Ergebnis
aufnehmen.

```haskell
addTS [(t1, 4), (t2, 7)] [(t1, 9), (t3, 42)]
-- => [(t1, 13), (t2, 7), (t3, 42)]
```

Alternativ könnten wir zur Einsicht gelangen, dass die Signatur von
`addTS` falsch war und eine Addition lediglich ein `Maybe (TimeSeries Float)` ausgeben sollte.

```haskell
addTS :: TimeSeries Float -> TimeSeries Float -> Maybe (TimeSeries Float)
```

Oder wir möchten übereinstimmende Tupel mit `Just` auszeichnen und
nicht-übereinstimmende mit `Nothing`.

```haskell
addTS :: TimeSeries Float -> TimeSeries Float -> TimeSeries (Maybe Float)
```

Vielleicht fallen uns noch ein paar weitere Möglichkeiten
ein. Unumstößlich und ausschließlich richtig ist keine der
Optionen. Alle haben ihre Berechtigung. Eine funktional vollständige
Programmierschnittstelle müsste also entweder alle oben genannten
Optionen mitliefern oder wir müssten noch nach einer passenden
Abstraktion suchen. Wir möchten den letzteren Weg beschreiten. Anstatt
die Abstraktion an der Oberfläche der oben angedeuteten Signaturen und
Implementierungen zu orientieren, wollen wir einen Schritt zurückgehen
und über die Bedeutung von Zeitreihen nachdenken.

Ein erster Wurf könnte so aussehen:

```
type TimeSeries a = [(Time, a)]
mu :: TimeSeries a -> Set of tuples (Time, a) where all left
components are distinct
mu [] = ∅
mu (t, v):xs = let s be mu(xs)
                in if ∃ v such that (t, v) ∈ s
                     then s
                     else s ∪ {(t, v)}

mu (lookup t ts) = if ∃ v, (mu(t), v) ∈ mu(ts)
                     then mu(Just) v
                     else mu(Nothing)
```

Hier wird's schon wilder als bei den Numeralen aus dem vorherigen
Artikel. Am Anfang steht jetzt nicht nur eine Typdefinition, sondern
zusätzlich ein Typkonstruktor. Diesen Typkonstruktor können wir mit
einer Liste von `(Time, a)`-Tupeln füttern und erhalten einen Wert
unseres `TimeSeries a` Typs.  Die Bedeutungsfunktion, die in den
folgenden Zeilen definiert wird, bildet die Werte auf eine
mathematische Struktur ab, die sehr ähnlich aussieht wie der Typ des
Haskell-Typ-Konstruktors. Ähnlich ist jedoch nicht gleich: Listen
haben eine Reihenfolge, Mengen nicht. Die Einschränkung, die wir auf
der rechten Seite machen -- alle Zeitpunkte müssen unterschiedlich
sein -- machen wir auf der linken Seite nicht. Das hat Auswirkungen
auf die Definition von `mu`. Dort müssen wir beispielsweise bestimmen,
welche Werte bei überlappenden Zeitstempeln gewinnen: In obiger
Definition sticht das letzte Tupel.

Darunter spezifizieren wir die Bedeutung von `lookup t ts` und zwar
mithilfe der Bedeutungen von `t` und `ts`. Die Bedeutung von `ts`,
also `mu(ts)` haben wir oben gesehen. Die Bedeutung eines Zeitstempels
`t :: UTCTime` müssten wir streng genommen noch angeben; immerhin ist
`UTCTime` ein Haskell-Typ und kein Objekt unserer reinen
mathematischen Welt. In diesem Artikel sparen wir uns diesen
Schritt. Es sei nur so viel gesagt: Auch diese Übersetzung ist nicht
ganz trivial. Der Haskell-Typ `UTCTime` hat keine unendliche
Präzision. Für unsere mathematische Modellwelt setzen wir im Folgenden
aber oft implizit unendliche Präzision voraus. Die Bedeutungen
`mu(Just)` und `mu(Nothing)` sind wiederum recht einfach, diese können
wir mit Singleton-Menge und leerer Menge abbilden.

Die auffälligste "Operation" in den obigen Definitionen der
Bedeutungen ist `∃`. Der Existenzquantor erledigt auf der
Spezifikationsseite eine große Aufgabe, die auf der
Implementierungsseite sehr viel komplizierter ist. Darin besteht einer
der maßgeblichen Vorteile, die ein solches mathematisches Modell als
Spezifikation bietet: Die Korrektheit einer Implementierung ist leicht
zu beurteilen, wenn man einen konkreten Wert als Beweis der Existenz
in der Hand hält. Der Weg, um zu diesem Wert zu gelangen -- also die
Implementierung -- ist sehr viel aufwendiger. Die Gewährleistung der
Übereinstimmung von Spezifikation und Implementierung ist aber ein
Problem, das sich mit Technik lösen lässt^[haskell-weak].

In einem späteren Artikel werden wir uns ums automatische Beweisen
unserer Implementierungen kümmern. In diesem Artikel kümmern wir uns
um den Modellierungsaspekt. Unser erster Wurf oben ist nämlich noch
nicht so einfach wie er sein könnte. Zunächst können wir ein paar
einfache Umbenennungen machen. Eine Menge von Zweier-Tupeln `(x, y)`
ist nämlich nichts anderes als eine _Relation_. Relationen, bei denen
die linke Komponente eindeutig ist, nennt man
_rechtseindeutig_. Rechtseindeutige Relationen sind partielle
Funktionen und partielle Funktionen können wir mit `Maybe` kodieren
(wir erlauben uns für einen kurzen Moment auch in unserer
Spezifikationswelt mit Maybe zu programmieren).

```haskell
type TimeSeries a = [(Time, a)]
mu :: TimeSeries a -> (Time -> Maybe a)
mu = ...

mu (lookup t ts) = mu(ts)(t)
```

Diese Spezifikation ist schon deutlich einfacher. `lookup` ist damit
lediglich eine Variante von Funktionsapplikation. Einen kleinen
Vereinfachungsschritt können wir uns aber noch gönnen. `Time -> Maybe
a` ist ganz gut, aber `Time -> a` wäre noch einfacher und
allgemeiner. Vielleicht sollten wir also von unserer Idee von
Zeitreihen ablassen und stattdessen über Zeitfunktionen sprechen.

```haskell
data TimeFunction a where
  TimeSeries :: [(Time, a)] -> TimeFunction (Maybe a)
  
mu :: TimeFunction a -> (Time -> a)
mu (TimeSeries ...) = ...

lookup :: TimeFunction a -> UTCTime -> a

mu (lookup t ts) = mu(ts)(t)
```

`lookup` ergibt auch für Zeitfunktionen Sinn und die Definition ist
wiederum bloß Funktionsapplikation. Zeitfunktionen können wir auch
addieren und jetzt ist die punktweise Addition die offensichtlich
richtige Definition. Wir benutzen dazu direkt die naheliegende
Abstraktion.

```haskell
liftTF :: (a -> b -> c) -> TimeFunction a -> TimeFunction b -> TimeFunction c
mu(liftTF f x y) = \t -> f (mu x t) (mu y t)

addTF :: TimeFunction Float -> TimeFunction Float -> TimeFunction Float
addTF = liftTF (+)
```

`liftTF`



[dd1]: <https://funktionale-programmierung.de/2024/02/27/denotational-design-01.html>
[haskell-weak]: Im allgemeinen ist Haskell leider zu schwach, um ordentliche Spezifikationen ausdrücken zu können. Wir werden in einem späteren Artikel sehen, wie Programmiersprachen wie Lean oder Agda uns erlauben, sowohl Spezifikation als auch Implementierung in ein und derselben Sprache aufzuschreiben und die Deckung zu gewährleisten.
