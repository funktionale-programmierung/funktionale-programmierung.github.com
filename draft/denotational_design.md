---
layout: post
title: "Eine Einführung in Denotational Design anhand der Frage 'Was ist eine Zeitreihe?'"
author: markus-schlegel
tags: ["Praxis", "Denotational Design", "Modelling"]
---

Beim Programmieren wird viel Text produziert und dennoch [ist
Programmieren nicht gleich
Textproduktion.](https://www.sciencedirect.com/science/article/abs/pii/0165607485900328)
Die Produktion von Text ist viel mehr die Form, in der etwas ganz
anderes stattfindet, nämlich die Formulierung abstrakter Ideen in
einer Sprache, die der Computer versteht. Der Computer kann sehr
schnell Berechnungen ausführen, versteht selbst aber nicht, was er
tut. Die Interpretation der Ein- und Ausgaben, das machen
Menschen. Die Programmiercommunity fokussiert sich gern auf den
Gesichtspunkt der Performance. Dieser ist durchaus wichtig, denn ohne
Performance taugt die beste Software nichts. Etwas in Vergessenheit
geraten ist dabei aber, dass es sich bei jeder Software lediglich um
die Abbildung einer abstrakten Idee handelt. In dieser Artikelreihe
wollen wir uns diesen abstrakten Ideen -- wir sagen dazu oft: Modelle
-- und deren Verhältnis zum Computer widmen.

Das abstrakte Modell kommt spätestens immer dann zum Einsatz, wenn
Bugs in der Software auftreten. Ein Bug, also ein Defekt in einem
vorliegenden Programm, kann nur dann identifiziert werden, wenn man
ein Idealbild vorm inneren Auge hat, mit dem man das defekte Programm
vergleicht. Dass es diese abstrakte Idee -- das Modell -- hinter der
Software also geben muss, sollte unstrittig sein. Die Disziplin des
Denotational Design rückt das Modell wieder in den Fokus und zeigt,
wie dieses präzise beschrieben werden kann und wie diese Beschreibung
die nachfolgende Implementierung leiten kann.

## Denotationale Semantik

In der Programmiersprachenforschung werden Programmiersprachen oft
entlang der Semantik von Programmbausteinen studiert. Eine mögliches
semantisches Werkzeug ist die Denotationale Semantik. Hierbei weist
man jedem Datentyp eine mathematische Bedeutung zu, also einen Wert in
der abstrakten Sprache der Mathematik: Mengen, Funktionen, Zahlen,
Tupel etc. Operationen in der Programmiersprache werden definiert als
(mathematische) Funktionen von den Bedeutungen der Argumente zu den
Bedeutungen der Resultate.

Klingt abstrakt und ist es auch. Dahinter steckt aber wieder nur die
Idee, dass jedes Programm eigentlich bloß eine konkrete Repräsentation
einer abstrakten Idee ist. Die abstrakte Idee existiert in unseren
Köpfen außerhalb des Computers. Die zugehörige konkrete Repräsentation
ist maßgeschneidert auf den Computer.

Das [Original-Paper zur denotationalen
Semantik](https://dl.acm.org/doi/pdf/10.1145/360303.360308) erklärt
diesen Gedanken anhand des Unterschieds zwischen Numeralen und
natürlichen Zahlen. Die Sechs ist eine natürliche Zahl. Diese kann ich
auf unterschiedliche Arten repräsentieren: "110" in binär, "VI" als
römisches Numeral etc. Alle diese unterschiedlichen Repräsentationen
(Numerale) _meinen_ dieselbe Zahl. Mit einem einfachen
Gleichungssystem kann eine Bedeutungsfunktion `mu`, die Numerale auf
natürliche Zahlen abbildet, definiert werden. Für binäre Numerale:

```
mu(0) = 0
mu(1) = 1
mu(x0) = 2 * mu(x)
mu(x1) = 2 * mu(x) + 1
```

Die Bedeutungsfunktion `mu` beschreibt jetzt, was wir mit binären
Numeralen "eigentlich meinen." An diesen Gleichungen sind einige
Punkte bemerkenswert:

1. Die Bedeutungsfunktion `mu` bildet Numerale auf natürliche Zahlen
   ab. Die `0`, die in den Klammern steht und die `0`, die auf der
   rechten Seite steht, sind also unterschiedliche Objekte.
2. Das `x` ist kein Element der Numeralsprache, sondern eine Variable,
   die für beliebige Numeral(-teile) steht. Die dritte Gleichung und
   die vierte Gleichung beschreiben also eigentlich eine ganze Klasse
   von Gleichungen.
3. Diese Definition von `mu` hat einige Operationen als
   Voraussetzung. Auf der Seite der Numerale setzen wir voraus, dass
   es möglich ist, einzelne Ziffern aneinanderzukleben (`x0`). Auf der
   Seite der Zahlen gehen wir davon aus, dass wir multiplizieren und
   addieren können.
   
Der letzte Punkt ist beachtenswert, weil wir Multiplikation und
Addition natürlich auch auf binären Numeralen definieren könnten. Das
wäre zunächst etwas ganz anderes als die Operationen auf den
natürlichen Zahlen. Wenn wir aber "Multiplikation und Addition auf
binären Numeralen" sagen, dann _meinen_ wir wahrscheinlich
Operationen, die auch den jeweiligen Operationen auf den natürlichen
Zahlen entsprechen. Wir könnten das als Bedingung an korrekte
Implementierungen formulieren:

```
mu(x + y) = mu(x) + mu(y)
mu(x * y) = mu(x) * mu(y)
```

Hier sagen wir, dass sich die Addition/Multiplikation auf Numeralen
beobachtbar genau so verhalten sollen wie die Addition/Multiplikation
auf natürlichen Zahlen. Das drücken wir über das Verhältnis der
jeweiligen Ein- und Ausgaben der Operationen aus.

Anhand der einfacheren Inkrementieroperation `inc` können wir das
Ganze durchspielen. Die Gleichung, die die Korrektheit regelt, lautet:

```
mu(inc(x)) = mu(x) + 1
```

Achtung: `inc` ist eine ganz normale Operation auf Numeralen, bildet
also Numeral auf Numeral ab. `mu` ist wie oben beschrieben eine
besondere Funktion, die jedes Numeral auf eine natürliche Zahl
abbildet. Wir können diesen Zusammenhang anhand eines Diagramms
illustrieren.

// Kommutierendes Diagramm hier einfügen

Dieses Diagramm können wir auf zwei Arten lesen: 1. `+1` soll der
Gegenspieler unserer Implementierung `inc` sein. D.h. für alle Paare
von Numeralen, die `inc` als Eingabe und Ausgabe miteinander in
Beziehung setzt, soll eine entsprechende Beziehung auch über `+1`
gelten, wenn man die Ein- und Ausgaben durch die Bedeutungsfunktion
schickt. 2. Für alle Numerale als Eingabe für `inc` soll gelten: Es
kommt am Ende die selbe Zahl raus, egal, ob man zunächst das
Eingabenumeral in eine Zahl übersetzt und dann dort `+1` rechnet, oder
ob man zunächst `inc` anwendet und dann mithilfe der
Bedeutungsfunktion diese Ausgabe in eine Zahl übersetzt. In jedem Fall
ist dieses sog. kommutierende Diagramm ein Desiderat: Es _soll_
gelten, damit wir sagen können, dass `inc` korrekt[^1] ist.

Eine mögliche korrekte Implementierung für `inc` wäre jetzt:

```
inc(0) = 1
inc(1) = 10
inc(x0) = x1
inc(x1) = inc(x)0
```

Dass diese Implementierung korrekt ist, das ist bisher nur eine
Behauptung. Diese Behauptung müssen wir noch beweisen, z.B. durch
Induktion. Es gibt zwei Induktionsanfänge:

```
mu(inc(0)) = mu(1) = 1 = mu(0) + 1
mu(inc(1)) = mu(10) = mu(1) * 2 = 2 = mu(1) + 1
```

Das passt. Weiter geht's mit dem Induktionsschritt:

```
// gegeben: mu(inc(x)) = mu(x) + 1

mu(inc(x0)) = mu(x1)
            = 2 * mu(x) + 1
            = mu(x0) + 1

mu(inc(x1)) = mu(inc(x)0)
            = 2 * mu(inc(x))
            = 2 * (mu(x) + 1)
            = 2 * mu(x) + 2
            = 2 * mu(x) + 1 + 1
            = mu(x1) + 1
```

Nun _wissen_ wir, dass sich `inc` verhält wie `+ 1`. Dieses Wissen hat
eine ganz andere Qualität als die Sicherheit, die wir aus Tests
kennen. Mit ausreichend vielen Tests (und im besten Fall sogar mit
einigen Property Tests) können wir uns relativ sicher sein, dass
unsere Implementierung korrekt ist. Hier wissen wir's.

Das ist ein subtiler Punkt, mit dem die ganze Angelegenheit aber steht
und fällt. Was wissen wir denn genau? Dass sich `inc` auf den binären
Numeralen so verhält wie `+ 1` auf den natürlichen Zahlen. Unter der
Voraussetzung -- aber _nur_ unter der Voraussetzung -- dass `+ 1`
wirklich das ist, was wir implementieren wollten, ist unsere Software
also korrekt. Das lässt noch offen, ob wir wirklich `+ 1` meinten,
oder nicht doch `+ 2` oder was ganz anderes. Diesen letzten Schritt
können wir nicht beweisen. Deshalb ist es wichtig, dass die Semantiken
so simpel sind, dass sie auf einen Blick offensichtlich richtig
sind.

Als Gedankenexperiment könnten wir ja einfach mal definieren, dass
unsere Bedeutungsfunktion `mu` die Identitätsfunktion ist, `mu(x) =
x`. Damit würde jede Datenstruktur und jede Funktion für sich selbst
stehen und alles wäre trivialerweise korrekt implementiert. So richtig
vorwärtsgekommen sind wir dann aber nicht, denn der letzte Schritt
heißt: Sei dir sicher, dass die Bedeutung auch das ist, was du
ausdrücken willst. Das ist hier schwer möglich, denn Implementierungen
sind meistens sehr komplex -- berechtigterweise, denn eine
Implementierung muss sich eben noch um andere Aspekte außer
Korrektheit scheren: Performance, Portierbarkeit, überhaupt
Lauffähigkeit auf realer Hardware etc.

## Denotational Design

Die Denotationale Semantik entstand wie gesagt zur Erforschung von
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


## Konsequenzen

1. Wirklich fearless refactoring
 -- es ist nicht so schlimm, mal scheißcode hinzurotzen. wenn das
 interface stimmt und die implementierung das richtige tut.
 
2. Outsourcing

3. decouled by default

[^1] Es lohnt sich, noch einmal scharf darüber nachzudenken, was
Korrektheit hier bedeutet. Es wäre vielleicht präziser zu sagen: die
Berechnungsvorschrift ist so korrekt wie möglich. Bei der
_Durchführung_ der Berechnung können immer noch Fehler auftreten: Der
Speicher geht aus, kosmische Strahlung flippt ein paar Bits, jemand
wirft den Computer in einen See etc.
