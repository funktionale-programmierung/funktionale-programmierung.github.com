---
layout: post
description: Das 'Expression Problem' und einige Lösungen.
title: "Was ist das 'Expression Problem'?"
author: david-frese
tags: ["Expression Problem", Haskell, Scala, Clojure]
---

Das sogenannte 'Expression Problem' ist das Problem, dass sich
Programme in zwei Richtungen weiterentwickeln können, nämlich:

- neue Operationen für bestehende Datentypen, und
- neue Datentypen für bestehende Operationen,

und dass man sich gerne beide Möglichkeiten offen halten möchte, ohne
das Programmieren wesentlich komplizierter zu machen. Komplizierter
wird es zum Beispiel, wenn der bestehenden Code dazu geändert oder
neu kompiliert werden muss. (Phillip Wadler, der den Begriff geprägt
hat, formuliert es
[hier](http://www.daimi.au.dk/~madst/tool/papers/expression.txt) etwas
enger).

<!--
    Das _Expression Problem_ ist ein neuer Name für ein altes Problem.
    Das Ziel ist es, einen Datentyp durch eine Reihe von Fällen
    beschreiben zu können, sodass man neue Fälle zum Datentyp und neue
    Funktionen auf dem Datentyp hinzufügen kann, ohne dabei
    bestehenden Code neu kompilieren zu müssen, und ohne dabei
    die statische Typsicherheit des Codes aufzugeben (z.b. keine Typ-Casts).
-->

Der grundlegende Ansatz der objekt-orientierten Programmierung macht
es leicht neue Datentypen hinzuzufügen und schwer neue Operationen
hinzuzufügen, während es bei _klassischer_ funktionaler
Programmierung genau umgekeht ist (siehe z.B.
[hier](http://stackoverflow.com/questions/2078978/functional-programming-vs-object-oriented-programming)).

Im folgenden möchte ich das Problem anhand einfacher Beispiele
erläutern, und einige Lösungen auflisten, die in diversen Sprachen
dafür angeboten werden.

<!-- more start -->

Aus objekt-orientierter Sicht
===

Betrachten wir das Problem zunächst am Beispiel einer
objekt-orientierten Sprache, wie z.B. Java. Beginnen wir mit einem Typ
für Ausdrücke, `Expr`, zwei Ausprägungen dieses Typs (`Const` und
`Add`), und einer Operation `toString` zur hübschen Darstellung des
Ausdrucks als String:

{% highlight java %}
abstract class Expr {
    String toString();
}

class Const {
    public int value;
    String toString() {
        return Integer.toString(value);
    }
}

class Add {
    public Expr left;
    public Expr right;
    String toString() {
        return left.toString() + " + " + right.toString();
    }
}
{% endhighlight %}

Was nun in einer objekt-orientierten Sprache einfach ist, ist das
Hinzufügen einer neuen Art von Ausdruck, indem man einfach eine neue
Ableitung von `Expr` schreibt, z.B. für Negation:

{% highlight java %}
class Neg extends Expr {
    public Expr base;
    String toString() {
        return "-" + base.toString();
    }
}
{% endhighlight %}

Für die bestehende Operation auf Ausdrücken, `toString`, gibt man in
der abgeleiteten Klasse einfach eine Implementierung an, wodurch man
diese Operation auf die neue Art von Ausdruck erweitert hat - und das
ohne den bestehenden Code geändert zu haben!

Wenn es aber nun darum geht eine neue Operation für Ausdrücke
hinzuzufügen, also z.B. eine Funktion `evaluate` die den Wert eines
Ausdrucks errechnet, dann geht das in diesem Fall nicht ohne die
bestehende abstrakte Klasse `Expr` zu modifizieren, genauso wie alle
Ableitungen davon (oder zumindest viele davon).

Wie bereits erwähnt, ist es in funktionalen Sprachen üblicherweise
genau andersherum, wie der nächste Abschnitt zeigt.

Aus funktionaler Sicht
===

Um Ausdrücke verschiedener Ausprägung und Operationen darauf zum
Beispiel in [Haskell](http://www.haskell.org/) zu definieren, würde
man üblicherweise einen Datentyp `Expr` mit dem Konstrukt `data`
definieren, und die Operationen darauf über Pattern-Matching:

{% highlight haskell %}
data Expr =
    Const Int
  | Add Expr Expr

toString (Const i) = show i
toString (Add left right) = (toString left) ++ " + " ++ (toString right)
{% endhighlight %}

Eine Operation wie eine Auswertungsfunktion ist dann ganz einfach zu
implementieren:

{% highlight haskell %}
evaluate (Const i) = i
evaluate (Add left right) = (evaluate left) + (evaluate right)
{% endhighlight %}

Hierfür musste der bestehende Code nicht angefasst werden! Aber wie
man leicht erkennt, ist die Erweiterung um einen Negations-Ausdruck
nicht mehr möglich ohne die Definition des Datentyps, sowie die
Definitionen der Operationen `toString` und dann auch `evaluate` zu
ändern.

Haskells Typklassen
===

Eine Möglichkeit dem Problem in Haskell zu begegnen ist, keinen
Datentyp für Ausdrücke zu definieren, sondern einen eigenen Typ für
jede Art von Ausdruck, und dann sogenannte _Typklassen_ für die
Operationen darauf.

Typklassen definieren eine Gruppe von Funktionen
und deren Signaturen, ähnlich wie Interfaces in Java. Für beliebige
Typen kann man dann jederzeit _Instanzen_ dieser Typen implementieren,
sowie für polymorphe Funktionen die _Einschränkung_ deklarieren, dass
für einen Typparameter eine Instanz von bestimmten Typklassen
vorhanden sein muß.

Bevor wir je eine Erweiterung in beide "Richtungen" machen, hier
zunächst die beiden Typen für Konstanten und Additionsausdrücke und
die erste Typklasse für die `toString`-Operation:

{% highlight haskell %}
data ConstExpr = Const Int
data AddExpr a b = Add a b 

class ToString e where
  toString :: e -> String

instance ToString ConstExpr where
  toString (Const i) = show i

instance (ToString a, ToString b) => ToString (AddExpr a b) where
  toString (Add a b) = (toString a) ++ " + " ++ (toString b)
{% endhighlight %}

Besonders zu beachten ist dabei, dass der Datentyp für
Additions-Ausdrücke keinerlei Einschränkungen für die Typen der beiden
Teilausdrücke (`a` und `b`) macht. Man könnte also auch eine `AddExpr`
mit zum Beispiel zwei String-Listen bilden. Die Einschränkung kommt
dann erst in der zugehörigen Implementierung von `ToString` - der Teil
links vom `=>`. Diesen kann man in etwa so lesen: Wenn die Typklasse
`ToString` auf einem Typ `a` und einem Typ `b` implementiert ist, dann
kann `ToString` auf dem Typ `AddExpr a b` folgendermaßen implementiert
werden.

Jetzt ist es möglich, eine Erweiterung der Ausdrücke auf
Negations-Ausdrücke zu machen, indem wir einen neuen Typ definieren,
sowie Instanzen für die bestehende Operation `toString`:

{% highlight haskell %}
data NegExpr e = Neg e

instance (ToString e) => ToString (NegExpr e) where
  toString (Neg e) = "-" ++ (toString e)
{% endhighlight %}

Fügt man einen neuen Typ hinzu, muss man entsprechend neue Instanzen
für alle Operationen implementieren, die dieser Typ unterstützen soll.
Der bestehende Code muß nicht modifiziert werden! Also genau das was
wir erreichen wollten!

Und neue Operationen hinzuzufügen, geht nach wie vor. Man definiert eine
neue Typklasse, und Implementierungen für jeden bestehenden Typ:

{% highlight haskell %}
class Evaluate a where
  evaluate :: a -> Int

instance Evaluate ConstExpr where
  evaluate (Const i) = i

instance (Evaluate a, Evaluate b) => Evaluate (AddExpr a b) where
  evaluate (Add a b) = (evaluate a) + (evaluate b)

instance (Evaluate e) => Evaluate (NegExpr e) where
  evaluate (Neg e) = 0 - (evaluate e)
{% endhighlight %}

Auch hier muss der bestehende Code nicht angefasst werden!

Nur ein kleiner Wermutstropfen: Vergisst man eine Typklasse zu
implementieren, bekommt man eine Fehlermeldung eventuell an einer ganz
anderen Stelle als gehofft - aber man bekommt eine! Aus diesem und
anderen Gründen, ist diese Art der Programmierung allerdings in
Haskell nicht sehr verbreitet, und man sollte sie nur wählen, wenn man
die Erweiterungsfähigkeit der Typen wirklich benötigt.

Eine ganz ähnliche Lösung ist übrigens in Scala möglich, mit den
gleichen Eigenschaften wie die Haskell-Lösung mit Typklassen - siehe
dazu zum Beispiel
[dieses PDF](http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf)

Clojures Protocols
===

Mit der JVM-Sprache [Clojure](http://clojure.org/) lässt sich das
Expression Problem ebenfalls gut lösen, und zwar mit dem
Sprach-Feature _Protocols_. Protocols definieren ebenfalls eine oder
mehrere Operationen, ähnlich wie Typklassen. Sie können dann separat
für verschiedene Typen implementiert werden - _extend_ nennt sich das
in Clojure. Da Clojure ein dynamisches Typsystem besitzt, fallen
gegenüber Haskell die Deklarationen der Einschränkungen weg - man kann
sie aber in Clojure als Annotation hinzufügen, wenn man das möchte.

Das Expression-Problem lässt sich in Clojure also wie folgt lösen
(reduziert auf zwei Typen und zwei Operationen, aber das Prinzip ist
das gleiche):

{% highlight clojure %}
(defrecord Const [e])

(defprotocol ToString
  (toString [this] "Return arbitrary string representation"))

(extend Const ToString
  { :toString (fn [this] (str (:e this))) })

;; neuen Typ hinzufügen

(defrecord Add [a b])

(extend Add ToString
  { :toString (fn [this]
      (str (toString (:a this)) " + " (toString (:b this))))
  })

;; neue Operation hinzufügen

(defprotocol Evaluate
  (evaluate [this] "Evaluate expression"))

(extend Const Evaluate
  { :evaluate (fn [this] (:e this)) })

(extend Add Evaluate
  { :evalute (fn [this]
      (+ (evaluate (:a this)) (evaluate (:b this))))
  })
{% endhighlight %}

In [diesem
Artikel](http://www.ibm.com/developerworks/library/j-clojure-protocols/)
ist diese Lösung etwas genauer beschrieben.

Zusammenfassung
===

Es gibt viele weitere Lösungen des Expression-Problems in diversen
Sprachen, aber auch viele Sprachen in denen sich nur sehr umständliche
Lösungen oder gar keine Lösungen finden lassen. Beim Design einer
Software-Komponente sollte man sich dann schon recht früh entscheiden,
ob sich diese eher in Richtung "mehr Operationen" oder in Richtung
"mehr Typen" weiterentwickeln wird - in die andere Richtung kann man
dann nur noch mit sehr großem Aufwand gehen.

Das Beste ist daher natürlich auf eine moderne (funktionale) Sprache
zu setzen, in der das Expression Problem gelöst werden kann.

<!--
non-solutions in C#:
- the visitor pattern (loose data extensibility)
- partial classes (no seperate compilation)
- static type cast switch (no type safety, no data ext)
- virtual type cast switch (no type safety)
- extension methods (not virtual no so data ext)
-->
