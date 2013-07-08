---
layout: post
description: Randomisierte Tests
title: "Randomisierte Tests mit QuickCheck"
author: andreas-bernauer
tags: ["QuickCheck", "Randomisierte Tests", "Racket"]
---


Die Programmiersprache [Racket](http://racket-lang.org/) (früher
bekannt als MzScheme) kommt mit einer QuickCheck-Bibliothek zum
randomisierten Testen, mit der sich die Testabdeckung in Programmen
erhöhen lässt: statt dass sich der Programmierer zahlreiche Tests
überlegt und ausprogrammiert, generiert die Bibliothek anhand einer
Spezifkation zufällige Testfälle, testet sie gegen definierte
Eigenschaften und berichtet Gegenbeispiele, falls welche gefunden
werden.

<!-- more start -->

## Testen über randomisierte Zahlen ##
 
Angenommen, wir wollen testen, ob für die Additionsfunktion
 `add(a,b) == add(b,a)` gilt (also Kommutativität).  Statt von Hand einige mehr
oder weniger interessante Fälle zu testen, können wir mit QuickCheck
die Eigenschaft definieren und gegen zahlreiche (pseudo-zufällig
gewählte) Testfälle testen lassen:

{% highlight scheme %}
(define property-commutative-add
  (for-all ((a number)
            (b number))
    (= (add a b) (add b a))))
{% endhighlight %}

`property-commutative-add` beschreibt, dass für alle Zahlen `a` und
`b` erwartet wird, dass `(= (add a b) (add b a)` sein soll (in der
Scheme-typischen Präfixnotation). `number` ist dabei der sogenannte
*Generator*, der die Beispielzahlen zufällig erzeugt.

Wenn man nun mit

{% highlight scheme %}
(check-property property-commutative-add)
{% endhighlight %}

die Eigenschaft testet, generiert die Bibliothek viele zufällige
Zahlenwerte `a` und `b` und prüft die Eigenschaft.  Damit deckt sie
mehr Fälle ab, als sich ein Programmierer in der Regel ausdenkt.

Sollte die Eingeschaft nicht erfüllt sein, gibt es eine Meldung. Zum
Beispiel könnten wir fälschlicherweise annehmen, dass auch für die
Subtraktionsfunktion `sub` Kommutativität gilt:

{% highlight scheme %}
(define property-commutative-sub
  (for-all ((a number)
            (b number))
    (= (sub a b) (sub b a))))
{% endhighlight %}

Beim Testen liefert Racket ein Gegenbeispiel:

    Eigenschaft falsifizierbar mit a = 0.0 und b = 1.5

denn `0.0 - (-1.5)` ist nicht gleich `(-1.5) - 0.0`.

Möchte man statt Fließkommazahlen nur exakte Zahlen prüfen, verwendet
man den Generator `rational`.  Alternativ erlaubt `expect-within`
statt `=` die Angabe einer tolerierten Abweichung. So erlaubt etwa
`(expect-within (add a b) (add b a) 0.001)`, dass die Additionen um
0.001 voneinander abweichen dürfen.

<!-- more complex examples such as distributivity? -->

Mit dem Implikationspfeil `==>` lassen sich Eigenschaften formulieren,
die von einer Bedingung abhängen. Zum Beispiel beschreibt

{% highlight scheme %}
(define property-transitivity-equality
  (for-all ((a number)
            (b number)
            (c number))
    (==> (and (= a b) (= b c))
         (= a c))))
{% endhighlight %}

dass wenn `a` = `b` und `b` = `c`, auch `a` = `c` sein soll.

## Testen über randomisierte Strukturen ##

Auch die Eigenschaften von Strukturen können beschrieben werden.  So
ist `(list gen)` ein Generator für Listen zufälliger Länge mit
Elementen des Generators `gen`. Mit

{% highlight scheme %}
(define property-associativity-concatenate
  (for-all ((list-1 (list number))
            (list-2 (list number))
            (list-3 (list number)))
    (expect (concatenate (concatenate list-1 list-2) list-3)
            (concatenate list-1 (concatenate list-2 list-3)))))
{% endhighlight %}

können wir also die Assoziativitäts-Eigenschaften beim Zusammenhängen
(`concatenate`) von zwei Listen testen lassen.  Hierbei prüft
`expect`, dass zwei Werte gleich sind.

## Testen über randomisierte Funktionen ## 

Racket erlaubt es auch, Funktionen höherer Ordnung randomisiert zu
testen, also Funktionen, die Funktionen als Argument oder Ergebnis
haben.  Ob zwei (verschiedene) Stück Code dasselbe tun, ist ein
eigener Forschungszweig der Theoretischen Informatik, darum testen wir
stattdessen, ob zwei verschiedene Stück Code bei der selben Eingabe
die selbe Ausgabe liefern.

Als Beispiel definieren eine die Eigenschaft von `curry`, die salopp
formuliert aus einer zwei-stelligen Funktion zwei ein-stellige
macht (zum Beispiel ist `((curry add) 3)` eine Funktion, die 3
addiert):

{% highlight scheme %}
(define property-curry
  (for-all ((a string)
            (b string)
            (proc (string string -> string)))
    (expect (((curry proc) a) b)
            (proc a b))))
{% endhighlight %}

Hier ist `(string string -> string)` die Signatur der zufällig
erzeugten Funktion: `proc` nimmt zwei Strings als Argumente und
liefert ein String als Ergebnis. Was `proc` mit den Argumenten macht,
ist nicht näher spezifiziert.

## QuickCheck ##

Zum ersten Mal wurde
[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/) von [Koen
Claessen](http://www.md.chalmers.se/~koen/) und [John
Hughes](http://www.md.chalmers.se/~rjmh/) 1999 für Haskell
vorgestellt.  Inzwischen gibt es QuickCheck [für über 20 andere
Programmiersprachen](http://en.wikipedia.org/wiki/QuickCheck), darunter für
[C](https://github.com/mcandre/qc),
[C++](http://software.legiasoft.com/quickcheck/) und
[Perl](http://search.cpan.org/~tmoertel/Test-LectroTest-0.3600/lib/Test/LectroTest/Tutorial.pod),
aber auch für
[Java](https://github.com/pholser/junit-quickcheck/),
[Clojure](https://bitbucket.org/kotarak/clojurecheck) und
[Scala](https://github.com/rickynils/scalacheck).
Es gibt sogar mit [quviq](http://www.quviq.com) eine kommerzielle
Firma, die auf QuickCheck spezialisiert ist.  
Zwar variiert die Qualität der Implementierungen etwas (zum Beispiel
fehlt es an manchmal an Determinismus oder Parallelisierbarkeit), doch
die Testabdeckung erhöhen können die Bibliotheken allemal.


