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
Anforderungen, welche Transformationen auf den Zeitreihen möglich sein
sollen:

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
lookup _ _ = Nothing
```

Korrekt wäre diese Implementierung insofern, dass das Programm
erfolgreich kompilierbar wäre. Die Implementierung ist aber alles
andere als korrekt im Bezug auf unsere Idee, wie sich `lookup`
_funktionieren_ soll. Wir müssten den Code korrigieren auf Basis
unserer Idee. Beim herkömmlichen Programmieren geben wir uns damit
zufrieden: Wir behalten die Idee im Hinterkopf und programmieren daran
entlang. Denotational Design zeigt eine alternative Vorgehensweise
auf: Wenn es diese abstrakte Idee gibt, dann können wir doch mal
versuchen, diese auch formal aufzuschreiben. Die Denotationale
Semantik gibt uns das Werkzeug, um diese Formulierung zu
bewerkstelligen: Wir müssen eine mathematische Struktur finden und
jede Operation auf eine entsprechende Operation in dieser Struktur
abbilden.

Ein erster Wurf könnte so aussehen:

```
newtype TimeSeries a = TS [(Time, a)]
mu :: TimeSeries a -> Set of tuples (Time, a) where all left
components are distinct
mu (TS xs) = foldl (\(t, v) s -> if \exists v, (t, v) \elem s
                                   then s
                                   else s \union (t, v))
                   empty
                   xs

mu(lookup t ts) = if \exists v, (mu(t), v) \elem mu(ts)
                    then mu(Just) v
                    else mu(Nothing)
```

Hier wird's schon wilder als bei den Numeralen. Am Anfang steht jetzt
nicht nur eine Typdefinition, sondern zusätzlich ein
Typkonstruktor. Diesen Typkonstruktor können wir mit einer Liste von
`(Time, a)`-Tupeln füttern und erhalten einen Wert unseres `TimeSeries a` Typs.
Die Bedeutungsfunktion, die darunter definiert wird, bildet
die Werte auf eine mathemtische Struktur ab, die sehr ähnlich aussieht
wie der Typ des Haskell-Typ-Konstruktors. Ähnlich ist nicht gleich:
Listen haben eine Reihenfolge, Mengen nicht. Die Einschränkung, die
wir auf der rechten Seite machen -- alle Zeitpunkte müssen
unterschiedlich sein -- machen wir auf der linken Seite nicht. Das hat
Auswirkungen auf die Definition von `mu`.
