---
layout: post
title: "Eine Einführung in Denotational Design Teil II: Zeitreihen"
author: markus-schlegel
tags: ["Denotational Design", "Modelling", "Formale Methoden", "Praxis"]
---

Dieser Artikel ist der zweite einer Serie von Artikeln über
Denotational Design.  Im [vorherigen Teil dieser Reihe](https://funktionale-programmierung.de/2024/02/27/denotational-design-01.html) hatten
wir die theoretische Grundlage von Denotational Design kennen gelernt:
Die denotationelle Semantik. In diesem Artikel wollen wir dieses
Werkzeug nutzen, um eine konkrete Fachlichkeit zu spezifizieren.

<!-- More start -->

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
können wir so darüber nachdenken. Unser System erlaubt den Nutzer:innen die
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
wir einen solchen Versuch wagen und mit folgender Modellierung
starten. Unser Zeitreihen-Modul besteht aus einem Typ für Zeitreihen,
einer Menge von Konstruktoren, einer Menge von Kombinatoren und einer
Menge von Accessoren.

```haskell
type TimeSeries :: * -> *

-- Ein Konstruktor
fromDataService :: [(UTCTime, a)] -> TimeSeries a

-- Ein Kombinator
addTS :: TimeSeries Float -> TimeSeries Float

-- Ein Accessor
lookup :: UTCTime -> TimeSeries a -> a
```

`TimeSeries` ist also ein simpler Typkonstruktur und `lookup` ist eine
(von mehreren) Funktion, die diesen Typkonstruktur verwendet. Nach
kurzem Nachdenken kommen wir aber drauf, dass die Signatur für
`lookup` nicht ganz richtig sein kann. "Zeitreihe" klingt doch so,
als gäbe es da nur an bestimmten Zeitpunkten zugeordnete Werte. Die
Signatur von `lookup` legt aber nahe, dass wir für jeden beliebigen
Zeitpunkt (`UTCTime`) einen Wert rausbekommen könnten. Mit einer
kleinen Korrektur landen wir bei:

```haskell
lookup :: UTCTime -> TimeSeries a -> Maybe a
```

Jetzt bekommen wir manchmal einen Wert und manchmal eben `Nothing`.
Woran haben wir diese Änderung in der Signatur von `lookup` eigentlich
festgemacht? Offensichtlich hatten wir eine Idee davon im Kopf, was
`TimeSeries` ist und was `lookup` tut. Diese Idee geht über die reinen
Signaturen hinaus und sie leitet auch die nachfolgende
Implementierung. Denn wenn wir nur der Signatur folgen würden, dann
wäre dies eine korrekte Implementierung:

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

### Erster Versuch

Die erste Idee für eine solche mathematische Struktur ist selten die
richtige, doch irgendwo müssen wir beginnen. Wir beginnen für unsere
Zeitreihen deshalb mit der naheliegenden Betrachtung: Zeitreihen, das
sind Listen[^list] von Zeit-Wert-Tupeln. Das klingt schon stark nach
Implementierung und tatsächlich könnten wir dieses Modell von
Zeitreihen ebenfalls mit Listen von Tupeln implementieren. Es kann
also durchaus sein, dass Modell und Implementierung nah
beieinanderliegen. Wir sollten uns jedoch in jedem Fall nicht davor
scheuen, diese beiden Welten im Laufe der Evolution unserer Software
auseinanderlaufen zu lassen, denn die beiden Welten haben
unterschiedlichen Anforderungen zu genügen.

```haskell
type TimeSeries a -- Modell: Liste von (Time, a)-Tupeln.
```

Jetzt müssen wir die oben genannten Funktionen beschreiben, indem wir
ihre Bedeutung in der Sprache von Zeitreihen als Listen von
Zeit-Wert-Tupeln ausdrücken. Der Konstruktor `fromDataService` scheint
trivial.[^overload] 

```haskell
𝛍 (fromDataService xs) = 𝛍 xs

-- Beispiel:
𝛍 (fromDataService []) = []
𝛍 (fromDataService [(t1, v)]) = [(𝛍 t1, 𝛍 v)]
```

Nun haben wir aber schon einen kleinen Salat: Die Listen `[(t1, x),
(t2, y)]` und `[(t2, y), (t1, x)]` sind natürlich nicht gleich, obwohl
sie _eigentlich_ dieselben Informationen ausdrücken. Das _Eigentlich_
ist der Knackpunkt. Immer, wenn wir uns ein _Eigentlich_ denken, ist
das ein Hinweis, dass unser Modell noch nicht ganz treffend ist. Was
wir uns bei den Listen von Zeit-Wert-Tupeln eigentlich gedacht haben,
sind wahrscheinlich Mengen von Zeit-Wert-Tupeln, wobei es für jeden
Zeitpunkt maximal ein Tupel gibt:

```haskell
type TimeSeries a -- Modell: Mengen von (Time, a)-Tupeln, wobei die Zeitpunkte eindeutig sind.
```

Mengen von Tupeln sind schlicht _Relationen_ und Relationen, wo die
linke Komponente eindeutig ist (sog. Rechtseindeutigkeit) nennt man
_partielle Funktionen_. Partielle Funktionen können wir in
Haskell mit `Maybe` im Ergebnistyp ausdrücken. Diesen Trick nutzen wir
auch in unserer mathematischen Modellierung.[^maybe]

```haskell
type TimeSeries a -- Modell: Time -> Maybe a
```

Dieses Modell drückt jetzt schon eher das aus, was wir mit Zeitreihe
meinen. Der Konstruktor `fromDataService` wird etwas komplexer:

```haskell
𝛍 (fromDataService []) = λ _ . Nothing
𝛍 (fromDataService (t1, v):xs) = λ t2 . if t1 == t2
                                          then Just v
                                          else 𝛍(fromDataService xs) t2
```

Die beiden Ausdrücke `fromDataService [(t1, x), (t2, y)]` und
`fromDataService [(t2, y), (t1, x)]` führen jetzt zu demselben
Ergebnis -- unser Modell ist sozusagen schlanker geworden.

Die Accessor-Funktion `lookup` ist einfach Funktionsapplikation.

```haskell
𝛍(lookup t ts) = 𝛍 ts t
```

Jetzt fehlt noch `addTS`. Bevor wir die Bedeutung aufschreiben,
schauen wir uns einige Beispiele an. In dem Fall, dass sich die beiden
Eingangszeitreihen in ihren Zeitstempeln decken, sollen wohl die
Punkte addiert werden:

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

Oder wir könnten die Punkte ohne Gegenspieler unverändert mit ins
Ergebnis aufnehmen.

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
Abstraktion suchen. Wir möchten den letzteren Weg beschreiten, denn
mindestens einen kleinen Vereinfachungsschritt bei der Modellierung
können wir uns noch gönnen. `Time -> Maybe a` ist ganz gut, aber `Time
-> a` wäre noch einfacher und allgemeiner.

### Zweiter Versuch: ~~Zeitreihen~~ Zeitfunktionen

Vielleicht sollten wir also von unserer Idee von Zeitreihen ablassen
und stattdessen über Zeitfunktionen sprechen.

```haskell
type TimeFunction a -- Modell: Time -> a

fromDataService :: [(Time, a)] -> TimeFunction (Maybe a)
𝛍 (fromDataService []) = λ _ . Nothing
𝛍 (fromDataService (t1, v):xs) = λ t2 . if t1 == t2
                                          then Just v
                                          else 𝛍(fromDataService xs) t2

lookup :: TimeFunction a -> UTCTime -> a
𝛍 (lookup t ts) = 𝛍 ts t
```

`fromDataService` und `lookup` ergeben auch für Zeitfunktionen Sinn
und die Definitionen sind dieselben wie zuvor. Zeitfunktionen können
wir auch addieren und jetzt ist die punktweise Addition die
offensichtlich richtige Definition. Wir benutzen dazu direkt die
naheliegende Abstraktion. Damit können wir auch Subtraktion und
Multiplikation ausdrücken.

```haskell
liftTF :: (a -> b -> c) -> TimeFunction a -> TimeFunction b -> TimeFunction c
𝛍 (liftTF f x y) = \t -> f (𝛍 x t) (𝛍 y t)

addTF :: TimeFunction Float -> TimeFunction Float -> TimeFunction Float
addTF = liftTF (+)

subTF :: TimeFunction Float -> TimeFunction Float -> TimeFunction Float
subTF = liftTF (-)

mulTF :: TimeFunction Float -> TimeFunction Float -> TimeFunction Float
mulTF = liftTF (*)
```

`liftTF` ist das zentrale Element unseres Modells, also unserer
Programmierschnittstelle, die wir anderen zur Verfügung stellen
können. Damit kann ein Benutzer dieser Schnittstelle selbst
entscheiden, welche Funktion er in die Domäne der Zeitfunktionen heben
möchte.

Bislang haben wir unser ursprüngliches Problem -- Addition von
Zeitreihen -- noch nicht gelöst. Die Verantwortung für die korrekte
Auswahl der Addition können wir dank `liftTF` jetzt einfach in die
Hand der Nutzer:innen legen. Beispielsweise:

```haskell
-- TimeSeries a ist jetzt TimeFunction (Maybe a)
addTS1 :: TimeFunction (Maybe Float) -> TimeFunction (Maybe Float) -> TimeFunction (Maybe Float)
addTS1 = liftTF (liftA2 (+))

addTS2 :: TimeFunction (Maybe Float) -> TimeFunction (Maybe Float) -> TimeFunction (Maybe Float)
addTS2 = liftTF (\x y -> case (x, y) of
                           (Just x', Just y') -> Just (x' + y')
                           (Just x', Nothing) -> Just x'
                           (Nothing, Just y') -> Just y'
                           (Nothing, Nothing) -> Nothing)
```

Wir könnten auch einen neuen Zeitfunktionskombinator `interpolate`
einführen und dann `addTF` nutzen.

```haskell
interpolate :: a -> TimeFunction (Maybe a) -> TimeFunction a
𝛍 (interpolate dflt tf) = λ t . let tl be the largest timestamp such that tl <= t and 𝛍 tf tl == Just v
                                  if such a tl exists
                                    then v
                                    else dflt
```

Die Bedeutungsfunktion von `interpolate` nutzt Quantoren und scheut
nicht vor der Unendlichkeit zurück. Spätestens hier wird klar, dass
die Bedeutungsebene nicht mehr mit der Implementierung in eins
fällt. Im einem der nächsten Artikel werden wir sehen, wie wir
gewährleisten können, dass Implementierungen ihre Bedeutung
(Spezifikation) einhalten.

## Zusammenfassung

Wir haben angefangen mit einer eher vagen Idee von unserer Domäne:
Zeitreihen. Wir sind mit der ersten Modellierung allerdings auf
Probleme gestoßen. Eine einfache Operation wie die Addition ließ sich
nur ungelenk abbilden. Wir haben uns deshalb überlegt, was ein
geeignetes mathematisches Modell für Zeitreihen sein könnte. Wir haben
dieses Modell nach und nach vereinfacht und sind dabei zu der Einsicht
gelangt, dass Zeitreihen nur ein Sonderfall eines allgemeineren
Konzepts sind: Zeitfunktionen. Mit Zeitfunktionen konnten wir unser
Modell so mächtig gestalten, dass Detailfragen einfach die Nutzer:innen
selbst beantworten können.


[^haskell-weak]: Im allgemeinen ist Haskell leider zu schwach, um ordentliche Spezifikationen ausdrücken zu können. Wir werden in einem späteren Artikel sehen, wie Programmiersprachen wie Lean oder Agda uns erlauben, sowohl Spezifikation als auch Implementierung in ein und derselben Sprache aufzuschreiben und die Deckung zu gewährleisten.
[^overload]: Wir gehen im Fortgang des Artikels stets davon aus, dass `𝛍` überladen ist -- hier bspw. für Listen, Tuple, `UTCTime` und `a`.
[^list]: Listen sind sozusagen keine nativen mathematischen Objekte. Wir basteln uns mathematische Listen mit dem Null-Tupel `()` und dem Zweier-Tupel `(Wert, Restliste)` als `Cons`.
[^maybe]: Auch `Maybe` findet sich nicht unmittelbar in der Standardbibliothek der Mengentheorie (auf welcher wir hier aufbauen). Wir können `Nothing` mit dem Null-Tupel `()` ausdrücken und `Just x` ist das Einer-Tupel `(x)`.

<!-- more end -->
