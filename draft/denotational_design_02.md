---
layout: post
title: "Eine Einführung in Denotational Design Teil II: Zeitreihen"
author: markus-schlegel
tags: ["Denotational Design", "Modelling", "Formale Methoden", "Praxis"]
---

Dieser Artikel ist der zweite einer Serie von Artikeln über
Denotational Design.  Im vorherigen Teil dieser Reihe[^dd1] hatten
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
wir einen solchen Versuch wagen und mit folgender Modellierung
starten. Unser Zeitreihen-Modul besteht aus einem Typ für Zeitreihen,
einer Menge von Konstruktoren, einer Menge von Kombinatoren und einer
Menge von Accessoren.

```haskell
type TimeSeries :: * -> *

-- Ein Konstruktor
fromDataService :: [(Time, a)] -> TimeSeries a

-- Ein Kombinator
addTS :: TimeSeries Float -> TimeSeries Float

-- Ein Accessor
lookup :: UTCTime -> TimeSeries a -> a
```

`TimeSeries` ist also ein simpler Typkonstruktur und `lookup` ist eine (von mehreren)
Funktion, die diesen Typkonstruktur verwendet. Nach kurzem Nachdenken
kommen wir aber drauf, dass die Signatur für `lookup` nicht ganz richtig sein
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

### Erster Versuch

Die erste Idee für eine solche mathematische Struktur ist selten die
richtige, doch irgendwo müssen wir beginnen. Wir beginnen für unsere
Zeitreihen deshalb mit der naheliegenden Betrachtung: Zeitreihen, das
sind Listen[^list] von Zeit-Wert-Tupeln. Das klingt schon stark nach
Implementierung und tatsächlich könnten wir dieses Modell von
Zeitreihen ebenfalls mit Listen von Tupeln implementieren. Es kann
also durchaus sein, dass Modell und Implementierung nah
beieinanderliegen. Wir sollten uns in jedem Fall nicht davor scheuen,
diese beiden Welten im Laufe der Evolution unserer Software
auseinanderlaufen zu lassen, denn die beiden Welten haben
unterschiedlichen Anforderungen zu genügen.

```haskell
type TimeSeries a -- Modell: Liste von (Zeit, a)-Tupeln.
```

Jetzt müssen wir die oben genannten Funktionen beschreiben, indem wir
ihre Bedeutung in der Sprache von Zeitreihen als Listen von
Zeit-Wert-Tupeln ausdrücken. Der Konstruktor `fromDataService` scheint
trivial.

```haskell
𝛍(fromDataService xs) = xs
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
type TimeSeries a -- Modell: Mengen von (Zeit, a)-Tupeln, wobei die Zeitpunkte eindeutig sind.
```

Mengen von Tupeln sind schlicht _Relationen_ und Relationen, wo die
linke Komponente eindeutig ist (sog. Rechtseindeutigkeit) nennt man
_partielle Funktionen_. Partielle Funktionen können wir in
Haskell mit `Maybe` im Ergebnistyp ausdrücken. Diesen Trick nutzen wir
auch in unserer mathematischen Modellierung.[^maybe]

```haskell
type TimeSeries a -- Modell: Time -> Maybe a
```

Dieses Modell drückt jetzt eher das aus, was wir mit Zeitreihe meinen. Der Konstruktor `fromDataService` wird etwas komplexer:

```haskell
𝛍 (fromDataService []) = λ _ . Nothing
𝛍 (fromDataService (t1, v):xs) = λ t2 . if t1 == t2
                                          then Just v
                                          else 𝛍(fromDataService xs) t2
```

Die beiden Ausdrücke `fromDataService [(t1, x), (t2, y)]` und
`fromDataService [(t2, y), (t1, x)]` führen jetzt zu demselben
Ergebnis -- unser Modell ist sozusagen schlanker geworden.

Die Accessor-Funktion `lookup` ist jetzt einfach Funktionsapplikation.

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
-> a` wäre noch einfacher und allgemeiner. Vielleicht sollten wir also
von unserer Idee von Zeitreihen ablassen und stattdessen über
Zeitfunktionen sprechen.

### Zweiter Versuch: ~Zeitreihen~ Zeitfunktionen

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
möchte. Bislang haben wir unser ursprüngliches Problem -- Addition von
Zeitreihen -- noch nicht gelöst. Die Verantwortung für die korrekte
Auswahl der Addition legen wir jetzt einfach in die Hand des
Nutzers. Beispielsweise:

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

------------------------------------------------------------------------------

Ein erster Wurf könnte so aussehen:

```
type TimeSeries a
𝛍 :: TimeSeries a -> Set of tuples (Time, a) where all left components are distinct
𝛍 [] = ∅
𝛍 (t, v):xs = let s be 𝛍(xs)
               in if ∃ v such that (t, v) ∈ s
                    then s
                    else s ∪ {(t, v)}

𝛍 (lookup t ts) = if ∃ v, (𝛍(t), v) ∈ 𝛍(ts)
                    then 𝛍(Just) v
                    else 𝛍(Nothing)
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
auf die Definition von `𝛍`. Dort müssen wir beispielsweise bestimmen,
welche Werte bei überlappenden Zeitstempeln gewinnen: In obiger
Definition sticht das letzte Tupel.

Darunter spezifizieren wir die Bedeutung von `lookup t ts` und zwar
mithilfe der Bedeutungen von `t` und `ts`. Wir haben uns erlaubt, `𝛍`
als überladene Funktion anzunehmen. Die Bedeutung von `ts`, also
`𝛍(ts)` haben wir oben gesehen. Die Bedeutung eines Zeitstempels `t :: UTCTime`
müssten wir streng genommen noch angeben; immerhin ist
`UTCTime` ein Haskell-Typ und kein Objekt unserer reinen
mathematischen Welt. In diesem Artikel sparen wir uns diesen
Schritt. Es sei nur so viel gesagt: Auch diese Übersetzung ist nicht
ganz trivial. Der Haskell-Typ `UTCTime` hat keine unendliche
Präzision. Für unsere mathematische Modellwelt setzen wir im Folgenden
aber oft implizit unendliche Präzision voraus. Die Bedeutungen
`𝛍(Just)` und `𝛍(Nothing)` sind wiederum recht einfach -- diese können
wir mit der einelementigen Menge und der leeren Menge abbilden.

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
Problem, das sich mit Technik lösen lässt[^haskell-weak].

In einem späteren Artikel werden wir uns ums automatische Beweisen
unserer Implementierungen kümmern. In diesem Artikel kümmern wir uns
um den Modellierungsaspekt. Unser erster Wurf oben ist nämlich noch
nicht so einfach wie er sein könnte. Zunächst können wir ein paar
triviale Umbenennungen machen. Eine Menge von Zweier-Tupeln `(x, y)`
ist nämlich nichts anderes als eine _Relation_. Relationen, bei denen
die linke Komponente eindeutig ist, nennt man
_rechtseindeutig_. Rechtseindeutige Relationen sind partielle
Funktionen und partielle Funktionen können wir mit `Maybe` kodieren
(wir erlauben uns für einen kurzen Moment auch in unserer
Spezifikationswelt mit Maybe zu hantieren).

```haskell
type TimeSeries a = [(Time, a)]
𝛍 :: TimeSeries a -> (Time -> Maybe a)
𝛍 = ...

𝛍 (lookup t ts) = 𝛍(ts)(t)
```

Diese Spezifikation ist schon deutlich einfacher. `lookup` ist damit
lediglich eine Variante von Funktionsapplikation. Einen kleinen
Vereinfachungsschritt können wir uns aber noch gönnen. `Time -> Maybe a`
ist ganz gut, aber `Time -> a` wäre noch einfacher und
allgemeiner. Vielleicht sollten wir also von unserer Idee von
Zeitreihen ablassen und stattdessen über Zeitfunktionen sprechen.

```haskell
data TimeFunction a where
  TimeSeries :: [(Time, a)] -> TimeFunction (Maybe a)
  
𝛍 :: TimeFunction a -> (Time -> a)
𝛍 (TimeSeries ...) = ...

lookup :: TimeFunction a -> UTCTime -> a

𝛍 (lookup t ts) = 𝛍 ts t
```

`lookup` ergibt auch für Zeitfunktionen Sinn und die Definition ist
wiederum bloß Funktionsapplikation. Zeitfunktionen können wir auch
addieren und jetzt ist die punktweise Addition die offensichtlich
richtige Definition. Wir benutzen dazu direkt die naheliegende
Abstraktion. Damit können wir auch Subtraktion und Multiplikation ausdrücken.

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
möchte. Bislang haben wir unser ursprüngliches Problem -- Addition von
Zeitreihen -- noch nicht gelöst. Die Verantwortung für die korrekte
Auswahl der Addition legen wir jetzt einfach in die Hand des
Nutzers. Beispielsweise:

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

## Zusammenfassung

Wir haben angefangen mit einer eher vagen Idee von unserer Domäne:
Zeitreihen. Wir sind mit der ersten Modellierung allerdings auf
Probleme gestoßen. Eine einfache Operation wie die Addition ließ sich
nur ungelenk abbilden. Wir haben uns deshalb überlegt, was ein
geeignetes mathematisches Modell für Zeitreihen sein könnte. Wir haben
dieses Modell nach und nach vereinfacht und sind dabei zu der Einsicht
gelangt, dass Zeitreihen nur ein Sonderfall eines allgemeineren
Konzepts sind: Zeitfunktionen. Mit Zeitfunktionen konnten wir unser
Modell so mächtig gestalten, dass Detailfragen einfach der Nutzer
selbst beantworten kann.


[^dd1]: <https://funktionale-programmierung.de/2024/02/27/denotational-design-01.html>
[^haskell-weak]: Im allgemeinen ist Haskell leider zu schwach, um ordentliche Spezifikationen ausdrücken zu können. Wir werden in einem späteren Artikel sehen, wie Programmiersprachen wie Lean oder Agda uns erlauben, sowohl Spezifikation als auch Implementierung in ein und derselben Sprache aufzuschreiben und die Deckung zu gewährleisten.
