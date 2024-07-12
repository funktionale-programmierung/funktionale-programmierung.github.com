---
layout: post
title: "Einführung in Denotational Design, Teil III: Einführung in Agda"
author: markus-schlegel
tags: ["Denotational Design", "Modelling", "Formale Methoden", "Agda", "Semantik"]
---

Dieser Artikel ist der dritte einer Reihe über _Denotational
Design_. In diesem Artikel lernen wir
[Agda](https://agda.readthedocs.io/en/v2.6.4.3-r1/overview.html)
kennen. Agda ist eine Programmiersprache mit einem sehr
ausdrucksstarken Typsystem. Es lassen sich mit Agda fast beliebige
formale Aussagen als Typen ausdrücken. Implementierungen -- Werte
dieser Typen -- werden dadurch zu Beweisen, die zeigen, dass diese
Aussagen auch zutreffend sind. Wir legen uns im folgenden das Beispiel
mit den binären Numeralen aus dem [ersten
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

Hier sind die Zeichen `0` und `1` auf der linken Seite lediglich
Symbole ohne Bedeutung. Die Zeichen `0`, `1` und `2` auf der rechten
Seite der Gleichheitszeichen bedeuten die natürlichen Zahlen. `x0`
bzw. `x1` auf der linken Seite bedeutet, dass für ein beliebiges
Numeral `x` rechts eine Null bzw. Eins angehängt wird.

Was wir in unserem ersten Artikel dogmatisch vorausgesetzt haben --
nämlich die Existenz und Funktionsweise der natürlichen Zahlen --
wollen wir in diesem Artikel zunächst weiter formalisieren.

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

Unser Vorhaben mag aussichtslos scheinen. Wir wollen die _eine
Repräsentation_ der natürlichen Zahlen (binäre Numerale) auf eine
_andere Repräsentation_ überführen. Mehr können wir formal auch gar
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
lässt uns fast alle denkbaren Konstruktionen ausdrücken --
insbesondere detaillierte Aussagen über die Korrektheit unseres Codes.

In Agda führen wir einen neuen Datentyp mit `data` ein. Die Syntax ist
sehr ähnlich zu der von
[GADTs](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type)
in Haskell. Wir definieren zwei Konstruktoren: Anstatt bei der Eins
beginnen wir bei der Null und anstatt einer unendlichen Menge von
Symbolen definieren wir einen Hochzähl-Kombinator.

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
two = suc one

three : Nat
three = suc two
```

Die Bedeutung dieser an sich bedeutungslosen Konstruktionen kommt erst
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
* (suc n) m = (+ m (* n m))
```

Diese drei Codeblöcke -- die Datendefinition, die Addition und die
Multiplikation -- können wir nicht weiter verifizieren. Hier müssen
wir uns absolut sicher sein. Glücklicherweise ist die unäre Kodierung
so trivial, dass uns diese Sicherheit nicht schwer fällt. Hätten wir
eine binäre Kodierung als Grundlage der natürlichen Zahlen verwendet,
sähe das schon anders aus.

## Die Binärzahlen

Die eingangs beschriebene Definition der binären Numerale lässt sich
fast wortwörtlich in Agda übersetzen.

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
uns sagen, ob das wirklich das ist, was wir mit binären Numeralen
meinen. Erst beim nächsten Schritt können wir uns maschinelle
Untestützung erwarten.

## inc===+1

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
zwei Dinge `x` und `y` vom Typen `Nat` nimmt und einen Typen (`Set`)
zurückgibt. Hier sehen wir zum ersten Mal den Einsatz von Dependent
Types. Das `x` könnte nämlich so was sein wie `zero` oder `suc zero`,
also Werte und das ist schließlich auch das, was wir wollen, wenn wir
einen Typen wie `=== (+ one two) three` konstruieren:

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
hinschreiben -- behaupten kann man alles --, es ist jetzt bloß nicht mehr
möglich, auch einen Wert von diesem Typen zu erzeugen, denn `(+ one one)`
normalisiert zu `(suc (suc zero))` und `three` ist `(suc (suc (suc zero)))`.

Jetzt können wir endlich unser Korrektheitskriterium für `inc` ausdrücken:

```agda
inc===+1 : (x : Bin) -> (=== (𝛍 (inc x)) (+ (𝛍 x) one))
inc===+1 = ?
```

`inc===+1` ist ein ganz gewöhnlicher Name. Namen in Agda können fast
alles sein. Wir sagen hier, dass `inc===+1` vom Typen `(x : Bin) ->
(=== (𝛍 (inc x)) (+ (𝛍 x) one))` ist, d.h. es ist eine Funktion von
einem beliebigen binären Numeral zu einem Beweisobjekt, das bezeugt,
dass eine Gleichheit besteht zwischen `(𝛍 (inc x))` und `(+ (𝛍 x)
one)`. Es gibt nur einen einzigen Konstruktor, der eine solche Gleichheit bezeugen könnte, nämlich `refl`. Trotzdem wird die folgende Implementierung noch nicht funktionieren.

```agda
inc===+1 : (x : Bin) -> (=== (𝛍 (inc x)) (+ (𝛍 x) one))
inc===+1 x = refl
```

Agda gibt diese Fehlermeldung aus:

```
𝛍 (inc x) != + (𝛍 x) one of type Nat
when checking that the expression refl has type
=== (𝛍 (inc x)) (+ (𝛍 x) one)
```

`!=` ist dabei _nicht_ die Negation unseres
`===`-Vergleichsoperators. Die Fehlermeldung sagt also nicht direkt,
dass unser Beweisunterfangen zum Scheitern verurteilt ist. Die
Fehlermeldung sagt nur, dass `refl` nicht als Beweisobjekt taugt, denn
dazu müssten beide Seiten zu dem selben Term normalisieren, d.h. nach
Einsetzung aller Definitionen müsste derselbe Term auf beiden Seiten
von `===` stehen. Zumindest taugt `refl` nicht _im Allgemeinen_ als
ein solches Beweisobjekt. Für konkretere Aussagen klappt das schon:

```
inc10b : (=== (𝛍 (inc 10b)) (+ (𝛍 10b) one))
inc10b = refl
```

Hier haben wir das `x` in `inc===+1` durch `10b` konkretisiert. Wenn
wir alle Funktionsanwendungen nach Definition auflösen (z.B. `inc 10b`
= `inc (at-0 1b)` = `at-1 1b`), erhalten wir auf beiden Seiten des
`===` das Ergebnis `suc (suc (suc zero))`. Der Konstruktor `refl` ist
also anwendbar. Das gilt auch für die ersten beiden Fälle für `inc===+1`.

```agda
inc===+1 : (x : Bin) -> (=== (𝛍 (inc x)) (+ (𝛍 x) one))
inc===+1 0b = refl
inc===+1 1b = refl
inc===+1 (at-0 x) = refl
inc===+1 (at-1 x) = ?
```

Überraschenderweise ist der dritte Fall `inc===+1 (at-0 x)` ebenfalls
unmittelbar erfüllbar, obwohl `x` dort als freie Variable
vorkommt. Das liegt daran, dass auch hier die Normalisierung denselben
Term liefert:

```
(𝛍 (inc (at-0 x))) =
(𝛍 (at-1 x)) =
(+ (* two (𝛍 x)) one)
```

... auf der linken Seite und ...

```
(+ (𝛍 (at-0 x)) one) =
(+ (* two (𝛍 x)) one)
```

... auf der rechten Seite.

Nur im letzten Fall kann Agda den Beweis nicht automatisch führen,
weil die Normalisierung -- das Einsetzen von Definitionen -- stecken
bleibt. Wir können uns von Agda an der Stelle des Fragezeichens das
Beweisziel anzeigen lassen:

```
Goal: === (+ (𝛍 (inc x)) (+ (𝛍 (inc x)) zero))
          (+ (+ (+ (𝛍 x) (+ (𝛍 x) zero)) one) one)
```

Hier sehen wir die zwei steckgebliebenen Terme. Das sieht kompliziert
aus. Nachdem wir uns vom ersten Schock erholt haben, sehen wir aber,
dass dort ganz offensichtlich zu vereinfachende Terme stehen: `(+ (𝛍 (inc x)) zero)`
sollte sich ja wohl zu `(𝛍 (inc x))` vereinfachen
lassen. Wir erhalten dann:

```
Goal: === (+ (𝛍 (inc x)) (𝛍 (inc x)))
          (+ (+ (+ (𝛍 x) (𝛍 x)) one) one)
```

Nun machen wir hier ja Induktion, d.h. wir können voraussetzen, dass
die Aussage `(=== (𝛍 (inc x)) (+ (𝛍 x) one))` für das `x` hier schon
bewiesen ist. Diese Gleichheit können wir oben einsetzen und erhalten:

```
Goal: === (+ (+ (𝛍 x) one) (+ (𝛍 x) one))
          (+ (+ (+ (𝛍 x) (𝛍 x)) one) one)
```

Jetzt stehen links und rechts des `===` schon dieselben Terme mit `+`
verbunden, nur noch in unterschiedlicher Reihenfolge und
Klammerung. Wir wissen aber, dass für Addition auf den unären Zahlen
das Assoziativ- und Kommutativgesetz gelten.

Als Menschen sind wir also von der Geltung unserer Behauptung
überzeugt. Agda ist es noch nicht. Das liegt daran, dass die Regeln,
die wir eben angewandt haben, gar nicht bekannt sind. Die erste dieser
Regeln -- vielleicht die subtilste -- ist die, dass wir aus einer
Gleichheit von `x` und `y` und einer Gleichheit von `y` und `z` eine
Gleichheit von `x` und `z` folgern können -- dass `===` also transitiv
ist. Wir nennen diese Regel `trans` und anstatt sie als Axiom zu
bestimmen, beweisen wir sie:

```agda
trans : (x : Nat) -> (y : Nat) -> (z : Nat) -> (=== x y) -> (=== y z) -> (=== x z)
trans x y z refl refl = refl
```

Der Beweis ist trivial, aber `trans` ist trotzdem ein wichtiges Hilfsmittel.

Die nächste Regel, die wir oben verwendet haben, ist, dass `zero` ein
neutrales Element der Addition ist, also dass gilt: `(x : Nat) -> (=== (+ x zero) x)`.

```agda
zeroneutral : (x : Nat) -> (=== (+ x zero) x)
zeroneutral zero = refl
zeroneutral (suc x) = ?
```

Der Fall, dass `x` gleich `zero` ist, ist trivial bewiesen nach
Normalisierung. Der Fall, dass `x` gleich `suc x` ist, erfordert den
Beweis, dass `(suc (+ x zero))` gleich `(suc x)` ist. Wir wissen nach
Induktionsvoraussetzung, dass `(+ x zero)` gleich `x` ist, aber im
Beweisziel sind beide Seiten noch durch ein `suc` eingewickelt. Wir
brauchen eine weitere Regel, die besagt, dass aus einer Gleichheit von
`x` und `y` auch eine Gleichheit von `f x` und `f y` folgt für
beliebige Funktionen `f`. Diese Regel heißt Kongruenz und wir nennen
sie `cong`.

```agda
cong : (f : Nat -> Nat) -> (x : Nat) -> (y : Nat) -> (=== x y) -> (=== (f x) (f y))
cong f x y refl = refl
```

Im Beweis für `zeroneutral (suc x)` müssen wir jetzt nur noch einsetzen.

```agda
zeroneutral : (x : Nat) -> (=== (+ x zero) x)
zeroneutral zero = refl
zeroneutral (suc x) = cong suc (+ x zero) x (zeroneutral x)
```

Nun fehlen noch das Assoziativgesetz ...

```agda
+assoc : (x : Nat) -> (y : Nat) -> (z : Nat) -> (=== (+ x (+ y z)) (+ (+ x y) z))
+assoc zero y z = refl
+assoc (suc x) y z = cong suc (+ x (+ y z)) (+ (+ x y) z) (+assoc x y z)
```

... und Kommutativgesetz.

```agda
+commut : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut x y = ?
```

Hierbei begegnet uns allerdings noch eine Subtilität. Wir machen einen
Induktionsbeweis über `x`. Im `zero` Fall ist unser Beweisziel `=== y (+ y zero)`.
Das haben wir doch scheinbar schon gezeigt mit
`zeroneutral`, wenn auch anders herum. Wir brauchen also auch noch das
Kommutativgesetz für `===`. Das wird gern `sym` genannt, da es die Symmetrie von `===` zeigt.

```agda
sym : (x : Nat) -> (y : Nat) -> (=== x y) -> (=== y x)
sym x y refl = refl
```

Jetzt können wir die ersten Fälle für `+commut` implementieren.

```agda
+commut : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut zero y = sym (+ y zero) y (zeroneutral y)
+commut (suc x) zero = cong suc (+ x zero) x (zeroneutral x)
+commut (suc x) (suc y) = ?
```

Das Beweisziel für den zweiten Fall ist `(=== (suc (+ x y)) (+ y (suc x)))`.
Wir gönnen uns noch einen letzten kleinen Helfer, der die
Vertauschbarkeit von `+` und `suc` zeigt.

```agda
+suclr : (x : Nat) -> (y : Nat) -> (=== (+ x (suc y)) (+ (suc x) y))
+suclr zero y = refl
+suclr (suc x) y = cong suc (+ x (suc y)) (+ (suc x) y) (suclr x y)
```

Für den letzen Fall von `+commut` setzen wir nur noch alles zusammen.

```agda
+commut : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut zero y = sym (+ y zero) y (zeroneutral y)
+commut (suc x) zero = cong suc (+ x zero) x (zeroneutral x)
+commut (suc x) (suc y) = cong
                            suc
                            (+ x (suc y))
                            (+ y (suc x))
                            (trans
                              (+ x (suc y))
                              (+ (suc x) y)
                              (+ y (suc x))
                              (+suclr x y)
                              (+commut (suc x) y))
```

## Implizite Parameter

Ay caramba! Wir haben endlich bewiesen, dass das Kommutativgesetz für
die Addition gilt. Dieser Beweis ist schon recht komplex und wir sind
noch lang nicht bei unserem eigentlichen Beweisziel `inc===+1`
angekommen. Bevor wir uns diesem widmen, refaktorisieren wir unseren
Kommutativitätsbeweis noch ein wenig. Bisher haben wir nämlich aus
didaktischen Gründen jeden Beweis bis ins kleinste von Hand selbst
durchgeführt. Agda kann aber oft selbst triviale Einsetzungen selbst
vornehmen. Den Beweis für `+commut` oben können wir mit sog. Holes
vereinfachen:

```agda
+commut2 : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut2 zero y = sym _ _ (zeroneutral y)
+commut2 (suc x) zero = cong suc _ _ (zeroneutral x)
+commut2 (suc x) (suc y) = cong
                             suc
                             _
                             _
                             (trans
                               _
                               _
                               _
                               (+suclr _ _)
                               (+commut2 (suc x) y))
```

Ein Hole weist Agda an: Füll hier -- wenn möglich -- die einzige
mögliche Lösung ein. Dank Typinferenz lässt sich diese Vereinfachung
im Code oft anwenden. Wir können diese Vereinfachung sogar an der
Definitionsstelle veranlassen, indem wir manche Parameter als implizit
markieren. Dazu schreiben wir die Parameter nicht in normalen, runden
Klammern, sondern in geschweiften. Anwendungen von `trans`, `cong`,
etc. werden dadurch deutlich kürzer und damit lesbarer.

```agda
trans2 : {x : Nat} -> {y : Nat} -> {z : Nat} -> (=== x y) -> (=== y z) -> (=== x z)
trans2 refl refl = refl

cong2 : {x : Nat} -> {y : Nat} -> (f : Nat -> Nat) -> (=== x y) -> (=== (f x) (f y))
cong2 f refl = refl

sym2 : {x : Nat} -> {y : Nat} -> (=== x y) -> (=== y x)
sym2 refl = refl

+commut2 : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut2 zero y = sym2 (zeroneutral y)
+commut2 (suc x) zero = cong2 suc (zeroneutral x)
+commut2 (suc x) (suc y) = cong2
                             suc
                             (trans2
                               (+suclr _ _)
                               (+commut2 (suc x) y))
```

## Der finale Beweis

Etwas grundsätzlich neues kommt beim Beweis von `inc===+1` nicht
mehr. Wir setzen lediglich `trans`, `cong`, `sym` etc. zusammen. Der finale Beweis ist dann ein ziemliches Monstrum:

```agda
inc===+1 : (x : Bin) -> === (μ (inc x)) (+ (μ x) one)
inc===+1 0b = refl
inc===+1 1b = refl
inc===+1 (at-0 x) = refl
inc===+1 (at-1 x) = trans2 (+assoc (μ (inc x)) (μ (inc x)) zero)
                           (trans2 (zeroneutral (+ (μ (inc x)) (μ (inc x))))
                                   (trans2 (cong2 (λ y → (+ y (μ (inc x))))
                                                  (inc===+1 x))
                                           (trans2 (cong2 (λ y → (+ (+ (μ x) one) y))
                                                          (inc===+1 x))
                                                   (trans2 (+assoc (+ (μ x) one) (μ x) one)
                                                           (cong2 (λ y → (+ y one))
                                                                  (trans2 (+commut2 (+ (μ x) one) (μ x))
                                                                          (trans2 (+assoc (μ x) (μ x) one)
                                                                                  (cong2 (λ y → (+ y one))
                                                                                         (cong2 (λ y → (+ (μ x) y))
                                                                                                (sym2 (zeroneutral (μ x))))))))))))
```

## Zusammenfassung

Dieser letzte Beweis ist wieder recht komplex. Wir könnten diesen
Beweis noch vereinfachen und in eine deutlich verständlichere Form
bringen. Die Agda-Standardbibliothek bringt bspw. Werkzeuge für
sog. Equational-Reasoning mit. Diese Werkzeuge werden wir in einem der
nächsten Artikel kennen lernen.

Auch wenn der finale Beweis einigermaßen komplex ist, hält sich der
Gesamtaufwand für unser Unterfangen doch in Grenzen -- zumindest in
Anbetracht der Garantien, die wir bekommen. Die Definition der unären
natürlichen Zahlen und die Definition von `===` haben wir oben nur aus
didaktischen Gründen selbst programmiert. Normalerweise würden wir
diese auch der Standardbibliothek entnehmen. Die Beweishilfsmittel,
die wir selbst definiert haben, beziehen sich alle auf das
Zusammenspiel von natürlichen Zahlen und `===`. Auch diese Hilfsmittel
sind in der Standardbibliothek bereits enthalten. Dann bleibt als
eigentlicher neuer Code nur folgendes übrig:

```agda
data Bin : Set where
  0b : Bin
  1b : Bin
  at-0 : Bin -> Bin
  at-1 : Bin -> Bin

μ : Bin -> Nat
μ 0b = zero
μ 1b = one
μ (at-0 x) = (* two (μ x))
μ (at-1 x) = (+ (* two (μ x)) one)

inc : Bin -> Bin
inc 0b = 1b
inc 1b = 10b
inc (at-0 x) = at-1 x
inc (at-1 x) = at-0 (inc x)

inc===+1 : (x : Bin) -> === (μ (inc x)) (+ (μ x) one)
inc===+1 0b = refl
inc===+1 1b = refl
inc===+1 (at-0 x) = refl
inc===+1 (at-1 x) = ...
```

Das ist doch gar nicht schlecht. Die Definitionen für `Bin` und `inc`
sind zwingend notwendig, wenn überhaupt etwas laufen soll. Die
Denotation `μ` kann man in den meisten Programmiersprachen auch
ausdrücken, um dann bspw. Unit-Tests formulieren zu können. In Agda
gehen wir mit `inc===+1` noch einen Schritt weiter und beweisen
tatsächlich, dass sich unser `inc` wie `+1` verhält. Mehr
Verlässlichkeit geht nicht.

[^1]: Agda erlaubt sogar die Definition von Infix- oder Mixfix-Operatoren. Damit könnten wir die Agda-Definition noch mehr der Papier-Mathematik angleichen. Der konzeptuellen Einfachheit verzichten wir in diesem Artikel auf diese Notationswerkzeuge und verwenden eine Syntax, die eher an S-Expressions erinnert.
