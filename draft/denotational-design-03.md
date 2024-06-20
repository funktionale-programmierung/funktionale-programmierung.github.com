---
layout: post
title: "Einführung in Denotational Design, Teil III: Einführung in Agda"
author: markus-schlegel
tags: ["Denotational Design", "Modelling", "Formale Methoden", "Agda", "Semantik"]
---

Dieser Artikel ist der dritte einer Reihe über _Denotational
Design_. In diesem Artikel lernen wir die [Programmiersprache
Agda](https://agda.readthedocs.io/en/v2.6.4.3-r1/overview.html)
kennen. Wir legen uns das Binärzahlenbeispiel aus dem [ersten
Artikel](https://funktionale-programmierung.de/2024/02/27/denotational-design-01.html)
vor und formalisieren es in Agda. Im nächsten Artikel wenden wir die
hier dargestellten Inhalte auf unser Praxisbeispiel mit den Zeitreihen
an.

<!-- more start -->

Im [ersten Artikel](https://funktionale-programmierung.de/2024/02/27/denotational-design-01.html)
dieser Reihe hatten wir die natürlichen Zahlen untersucht. Genauer:
Wir hatten gesehen, wie wir binäre Numerale -- reine Symbole,
zusammengesetzt aus Nullen und Einsen -- in die natürlichen Zahlen
übersetzen können und wie wir sicherstellen können, dass diese
Übersetzung verträglich ist mit unserer intuitiven Vorstellung, was
denn die natürlichen Zahlen eigentlich bedeuten. Unsere Übersetzung
hatten wir durch folgendes Gleichungssystem beschrieben:

```
𝛍(0) = 0
𝛍(1) = 1
𝛍(x0) = 2 * 𝛍(x)
𝛍(x1) = 2 * 𝛍(x) + 1
```

Hier sind die Zeichen `0` und `1` auf der linken Seite ledigliche
Symbole ohne Bedeutung. Die Zeichen `0`, `1` und `2` auf der rechten
Seite der Gleichheitszeichen bedeuten die natürlichen Zahlen. `x0`
bzw. `x1` auf der linken Seite bedeutet, dass für ein beliebiges
Numeral `x` rechts eine Null bzw. Eins angehängt wird. Was wir in
unserem ersten Artikel dogmatisch vorausgesetzt haben -- die Existenz
und Funktionsweise der natürlichen Zahlen -- wollen wir in diesem
Artikel zunächst weiter formalisieren.

## Eine Formalisierung der natürlichen Zahlen

Warum wollen wir überhaupt die natürlichen Zahlen weiter
formalisieren? Immerhin hat doch jeder ein ausreichendes intuitives
Verständnis vom Zählen, Addieren, Multiplizieren und so weiter, oder?
Tatsächlich könnten wir uns für die reine Übersetzung von Numeralen in
natürliche Zahlen auf die natürlichen Zahlen in der
Agda-Standard-Library beziehen. Die Addition und Multiplikation sind
dort bereits definiert und mehr brauchen wir für die Übersetzung gar
nicht. Später wollen wir aber den Beweis führen, dass sich Operationen
auf den Numeralen mit denen auf den natürlichen Zahlen decken und
spätestens dann müssen wir ein Verständnis entwickeln, wie die
natürlichen Zahlen in Agda repräsentiert sind. Es lohnt sich also,
dass wir diese natürlichen Zahlen selbst definieren.

Unser Vorhaben mag aussichtslos scheinen. Wir wollen die _eine_
Repräsentation der natürlichen Zahlen (binäre Numerale) auf eine
andere Repräsentation überführen. Mehr können wir formal auch gar
nicht erreichen. Um über die natürlichen Zahlen nachzudenken,
benötigen wir immer eine konkrete Repräsentation dieser. Die reinen,
abstrakten natürlichen Zahlen sind ohne konkrete Repräsentation gar
nicht greifbar. Man könnte sagen, dass der Begriff der natürlichen
Zahlen das Gemeinsame aller verschiedenen Repräsentationen ist.

Wir sind also dazu verdammt, mit konkreten Repräsentation der
natürlichen Zahlen zu arbeiten. Es ist damit aber nicht gleich alles
egal. Immerhin gibt es ungeschickte und geschicktere
Repräsentationen. Die römischen Zahlen sind eine sehr komplizierte
Repräsentation der natürlichen Zahlen (ohne die Null), die
Dezimalzahlen oder Binärzahlen sind da schon deutlich nützlicher für
theoretische Untersuchungen. Es gibt aber noch eine weitere
Repräsentation, die alle anderen schlägt in Sachen Einfachheit. Jedes
dreijährige Kind kennt diese Repräsentation: das Unärsystem.

Im Unärsystem wird eine Zahl einfach durch eine entsprechende Menge
von Strichen beschrieben. Eins ist I, Zwei ist II, Drei ist III, Vier
ist IIII und so weiter. Diese Bierdeckelnotation wollen wir zur
Grundlage unserer Formalisierung nehmen. Wir verwenden die
Programmiersprache Agda, um dieses Vorhaben umzusetzen. Agda ist eine
Programmiersprache mit sog. Dependent Types. Das Typsystem von Agda
lässt uns fast alle denkbaren Konstruktionen ausdrücken -- auch
Aussagen über die Korrektheit unseres Codes.

In Agda führen wir einen neuen Datentyp ähnlich wie in Haskell mit dem
Schlüsselwort `data` ein. Wir definieren zwei Konstruktoren: Anstatt
bei der Eins beginnen wir bei der Null und anstatt einer unendlichen
Menge von Symbolen definieren wir einen Hochzähl-Kombinator.

```agda
data Nat : Set where
  zero : Nat
  suc : Nat -> Nat
```

Et voila, wir haben die natürlichen Zahlen definiert. Den ersten paar
Zahlen können wir noch sprechendere Namen geben:

```agda
one : Nat
one = suc zero

two : Nat
two = suc two

three : Nat
three = suc two
```

Die Bedeutung dieser ansich bedeutungslosen Konstruktionen kommt erst
dann richtig zum Tragen, wenn wir ein paar Operationen auf den
natürlichen Zahlen definieren. Wir wollen beispielsweise zwei Zahlen
addieren.

```agda
+ : Nat -> Nat -> Nat
+ x y = ?
```

Wir können bei der Implementierung von `+` nach Konstruktionsanleitung
vorgehen. Wir schauen uns dazu an, auf welche zwei Arten das `x`
zustandegekommen sein könnte. `Nat` ist ein Summentyp aus zwei
Konstruktoren, also sollte unsere Definition auch auf diese beiden
Fälle eingehen.

```agda
+ : Nat -> Nat -> Nat
+ zero y = ?
+ (suc x') y = ?
```

Der erste Fall `zero + y` ergibt `y`. Im zweiten Fall sollten wir
einen rekursiven Aufruf wagen.

```agda
+ : Nat -> Nat -> Nat
+ zero y = y
+ (suc x') y = suc (+ x' y)
```

Die Multiplikation ist nicht viel komplexer:

```agda
* : Nat -> Nat -> Nat
* zero m = zero
* suc n m = (+ m (* n m))
```

Diese drei Codeblöcke -- die Datendefinition, die Addition und die
Multiplikation -- können wir nicht weiter verifizieren. Hier müssen
wir uns absolut sicher sein. Glücklicherweise ist die unäre Kodierung
so trivial, dass uns diese Sicherheit nicht schwer fällt. Hätten wir
eine binäre Kodierung als Grundlage der natürlichen Zahlen verwendet,
sähe das schon anders aus.

## Die Binärzahlen

Die oben beschriebene Definition der binären Numerale lässt sich fast wortwörtlich in Agda übersetzen.

```agda
data Bin : Set where
  0b : Bin
  1b : Bin
  at-0 : Bin -> Bin
  at-1 : Bin -> Bin
```

Die Datendefinition der unären Zahlen hatte nur zwei Fälle; hier haben
wir vier. Allein dadurch sieht man schon, dass diese Repräsentation
der natürlichen Zahlen deutlich aufwendiger ist. Auch bei der
Definition einiger erster konkreter Zahlen müssen wir uns ein wenig
mehr anstrengen:

```agda
10b : Bin
10b = at-0 1b

11b : Bin
11b = at-1 1b

100b : Bin
100b = at-0 (at-0 1b)
```

Im ersten Blogartikel hatten wir noch eine simple Operation auf den
binären Numeralen definiert: Das Hochzählen.

```agda
inc : Bin -> Bin
inc 0b = 1b
inc 1b = 10b
inc (at-0 x) = at-1 x
inc (at-1 x) = at-0 (inc x)
```

Das ist zwar verständlicher Code, aber bei weitem nicht so trivial wie
das Hochzählen in der unären Repräsentation. Dort wäre `+1` bloß ein
Synonym für den `suc`-Konstruktor.

Im ersten Blogartikel hatten wir dann bewiesen, dass `inc` auf den
binären Numeralen genau diesem `+1` auf den natürlichen Zahlen
entspricht. Dazu benötigten wir eine Übersetzung der beiden
Welten. Diese nannten wir die Denotation oder Bedeutungsfunktion `𝛍`.

```
𝛍(0) = 0
𝛍(1) = 1
𝛍(x0) = 2 * 𝛍(x)
𝛍(x1) = 2 * 𝛍(x) + 1
```

Auch diese Definition können wir in Agda beinahe Eins-zu-Eins übernehmen.[^1]

```agda
𝛍 : Bin -> Nat
𝛍 0b = zero
𝛍 1b = one
𝛍 (at-0 x) = (* two (𝛍 x))
𝛍 (at-1 x) = (+ (* two (𝛍 x)) one)
```

Diese Bedeutungsfunktion ist wieder einer dieser Codeblöcke über deren
Korrektheit wir uns schlicht sicher sein müssen. Kein Computer kann
uns sagen, dass das wirklich das ist, was wir mit binären Numeralen
meinen. Erst beim nächsten Schritt können wir uns maschinelle
Untestützung erwarten.

## inc==+1

Bevor wir gleich die semantische Übereinstimmung von `inc` und `+ one`
beweisen, basteln wir uns noch ein paar Werkzeuge, um überhaupt
Aussagen über unseren Code treffen zu können. Die einfachste Art von
Aussage, die wir treffen können, ist, dass zwei Codeschnipsel
"äquivalent" sind. Da wir daran interessiert sind, dass Agda uns
statisch sagen kann, ob unser Code gleich ist, kann diese Äquivalenz
aber nicht die einfache Laufzeit-Gleichheit `==` wie in Haskell
sein. Nein, wir brauchen eine Gleichheit auf dem Typlevel.

Zur Erinnerung: Aussagen folgender Spielart möchten wir prüfen.

```
(=== (+ one two) three)
forall x y . (=== (+ x y) (+ y x))
forall x . (=== (𝛍 (inc x)) (+ x one))
```

Derartige Aussagen wollen wir in Typen überführen. Damit ist klar,
dass `===` ein zweistelliger Typkonstruktor sein muss. Ein erster
(scheiternder) Versuch:

```agda
data === (x : Nat) (y : Nat) : Set where
  construct : (=== x y)
```

Hier sagen wir, dass `===` ein zweistelliger Typkonstruktor ist, der
zwei Werte `x` und `y` vom Typen `Nat` nimmt und einen Typen (`Set`)
zurückgibt. Hier sehen wir zum ersten Mal den Einsatz von Dependent
Types. Das `x` könnte nämlich so was sein wie `zero` oder `suc zero`,
also Werte und das ist schließlich auch das, was wir wollen, wenn wir
einen Typen wie `one + two === three` konstruieren:

``` agda
yes : (=== (+ one two) three)
yes = construct
```

Dieser Code geht durch den Typchecker -- aber das tut auch dieser Code:

``` agda
weird : (=== (+ one one) three)
weird = construct
```

Das ist zwar folgerichtig; es ist aber nicht das, was wir erreichen
wollten. Wir möchten nur wahre Aussagen konstruieren können. Oder
anders ausgedrückt: Die obige Definition von `===` ist noch nicht das,
was wir mit Gleichheit meinen -- wir wollen nicht nur, dass gleiche
Werte als gleich angesehen werden, sondern auch, dass ungleiche Werte
nicht als gleich angesehen werden. Wir möchten also nur Werte
konstruieren können, die Zeuge davon sind, dass `x` und `y` auch
wirklich gleich sind. Das klingt nach einem endlosen Regress, deshalb
versuchen wir's noch mal anders: Es kann überhaupt nur die Gleichheit
von Werten bezeugt werden, wenn auf beiden Seiten des
Gleichheitszeichens _derselbe_ Wert steht. Diese Gleichheit ist eine
rein reflexive Angelegenheit. Wir nennen unseren Konstruktor deshalb
`refl`. Er muss diese Form haben: `refl : (=== x x)`. Dann haut aber
die Signatur des Typkonstruktors nicht mehr hin. Wir verlagern deshalb
das zweite Argument auf die rechte Seite des Doppelpunkts und machen
damit aus dem zweistelligen Typkonstruktor einen einstelligen, der
aber eine Funktion auf dem Typlevel zurückgibt.

```agda
data === (x : Nat) : Nat -> Set where
  refl : ((=== x) x)
```

Der einzige Konstruktor `refl` ist dabei gar kein Wert vom Typ `(=== x)`,
was eine Funktion wäre, sondern die Typenfunktion wird direkt
wieder auf `x` angewendet. Der Typ `(=== x)` beschreibt die
sog. "Familie" von Beweisen, dass ein Wert gleich `x` ist. Diese
Familie ist nur "inhabited" -- hat also nur Werte -- am Typindex `x`, also es ist nur `x`
gleich `x` und der Beweis davon ist `refl`. Das heißt, ein Anwender
kann zwar immer noch einen Typen wie `(=== (+ one one) three)`
hinschreiben. Behaupten kann man alles. Es ist jetzt bloß nicht mehr
möglich, auch einen Wert von diesem Typen zu erzeugen, denn `(+ one one)`
normalisiert zu `(suc (suc zero))` und `three` ist `(suc (suc (suc zero)))`.






[^1]: Agda erlaubt sogar die Definition von Infix- oder Mixfix-Operatoren. Damit könnten wir die Agda-Definition noch mehr der Papier-Mathematik angleichen. Der konzeptuellen Einfachheit verzichten wir in diesem Artikel auf diese Notationswerkzeuge und verwenden eine Syntax, die eher an S-Expressions erinnert.
