---
layout: post
description: Was ist funktionale Programmierung?
title: Was ist funktionale Programmierung?
author: david-frese
---

Was ist überhaupt "Funktionale Programmierung?" Zur Beantwortung
dieser Frage dürften sich viele Neueinsteiger in das Thema an
Wikipedia wenden. Die entsprechende Seite in [deutscher
Sprache](http://de.wikipedia.org/wiki/Funktionale_Programmierung)
lässt aber einiges zu Wünschen übrig, was mich dazu veranlasst hat den
Titel dieses Blogs einmal grundlegend zu erklären.

<!-- more start -->

Das Funktionale Paradigma
=========================

Computerprogramme sind im Grunde nichts anderes als die strukturierte
Form einer Berechnung. Die genaue Art dieser Formalisierung ist dabei
immer von der verwendeten Programmiersprache abhängig, und in der
Regel auch von Programmierer zu Programmierer unterschiedlich, aber es
lassen sich einige grundlegende Kategorieren ausmachen - die
sogenannten Programmierparadigmen.

Die zwei wichtigsten Paradigmen sind das _imperative Paradigma_, und
das _deklarative Paradigma_. Nach dem imperativen Paradigma beschreibt
man eine Berechnung als Folge von _Anweisungen_, die einen
Programmzustand modifizieren, der am Ende das Ergebnis der Berechnung
enthält. Man beschreibung also _wie_ man zu dem Ergebnis gelangt.

Nach dem deklarativen Paradigma beschreibt man eine Berechnung als
einen _Ausdruck_, der zu einem Ergebnis _ausgewertet_ werden kann, und
damit angibt, _was_ das Ergebnis der Berechnung sein soll. Dabei gibt
es grundlegende, _primitive_ Ausdrücke, und zusammengesetze,
_komplexe_ Ausdrücke.

Das Paradigma der funktionalen Programmierung ist nun eine
Verfeinerung des _deklarativen Paradigmas_, bei dem die grundlegenden
Ausdrücke die Erzeugung von Funktionen (die _Abstraktion_) und die
Anwendung von Funktionen (die _Applikation_) sind.

Der Ausdruck zur Erzeugung von Funktionen wird meist als
"Lambda-Ausdruck" bezeichnet, und als Beispiel oft die
Programmiersprache Scheme gewählt, da hier das Wort Lambda direkt
vorkommt:

{% highlight scheme %}
   (lambda (x) (+ x 2))
{% endhighlight %}

Die nächste Version von Java, Java 8, wird aber zum Beispiel auch
Lambda-Ausdrücke enthalten, die dann so notiert werden können:

{% highlight java %}
   (x) -> x + 2
{% endhighlight %}

In der breitesten Auslegung ist funktionales Programmieren ist also
das Programmieren im funktionalen Paradigma, oder das Programmieren
mit Lambda-Ausdrücken.

Funktionale Sprachen
====================

Die meisten Programmiersprachen lassen sich nicht einem einzelnen
Paradigma zuordnen, sondern erlauben oder erfordern das Programmieren
nach unterschiedlichen Paradigmen. Das nennt man dann
Multi-Paradigmen-Sprachen.

Wann eine Programmiersprache als "Funktionale Sprache" bezeichnet
werden kann, ist daher weniger gut definiert als der Begriff des
funktionalen Paradigmas. Die
[FAQ](http://www.cs.nott.ac.uk/~gmh/faq.html) der Newsgroup
'comp.lang.functional' definiert den Begriff aber recht passend als
Bezeichnung für Sprachen, die das Programmieren im funktionalen
Paradigma unterstützen und befördern.

Funktionale Konzepte
====================

Es gibt eine Reihe von Konzepten die typisch für funktionale Sprachen
sind, beziehungsweise das relativ grobe Konzept des funktionalen Paradigmas
in verschiedene Richtungen weiter untergliedern. Zu ihnen gehören:

Funktionen höherer Ordnung
--------------------------

Als Funktion höherer Ordnung wird eine Funktionen bezeichnet, die
ihrerseits Funktionen als Argument erwartet, oder eine Funktion zurück
gibt. Eng damit verknüpft ist das Konzept, dass Funktionen sogenannte
"First-class citizens" sind. Das bedeutet, dass Funktionen in einem
Programm überall dort auftauchen können wo auch andere primitive Werte
wie Zahlen oder Strings auftauchen können.

Beispiel in Scala:

{% highlight scala %}
  def twice(f: Int => Int): Int => Int = { x => f(f(x)) }
{% endhighlight %}

Die Funktion `twice` nimmer eine Funktion `f` und gibt eine neue
Funktion zurück, die `f` auf einen Wert `x` zweimal hintereinander
anwendet.

Reinheit, Pure functions
------------------------

Eine Funktion, bzw. ein Ausdruck ist _rein_, wenn seine Auswertung
keine _Wirkung_ hinterlässt (auch Seiteneffekt genannt).

Die Gruppe der _rein-funktionalen Programmiersprachen_ erlaubt
ausschließlich das Programmieren mit reinen Ausdrücke, bzw. erschwert
die Verwendung von unreinen Ausdrücken in besonderem Maß. Aber auch in
anderen Sprachen empfielt es sich die unreinen Ausdrücke auf ein
Minimum zu redizieren und besonders kenntlich zu machen.

Beispiel in D:

{% highlight d %}
pure int square(int x) {
  return x * x;
}
{% endhighlight %}

Der D-Compiler stellt hier sicher, dass die Funktion `square` keine
globalen Variablen zugreift und nur andere reine Funktionen aufruft.

Rekursion
---------

Als grundlegendes Element zur Wiederholung von Ausdrücken dient in
Funktionalen Sprachen häufig die Rekursion, d.h. dass eine Funktion
sich selbst aufrufen kann, oder auch zwei Funktionen die sich
gegenseitig aufrufen können. Rekursion ist notwendig um eine größere
Klasse von Berechnungen durchführen zu können.

Häufig wird dabei die sogenannte _Endrekursion_, das sind rekursive
Aufrufe an Stellen ohne Kontext, ohne Nutzung von zusätzlichen
Resourcen ausgewertet. Eine "endlose" Rekursion kann dann genauso
verwendet werden wie eine Endlos-Schleife in imperativen Sprachen.

Beispiel in Scheme:
{% highlight scheme %}
(letrec ((ping (lambda (ball) (pong ball)))
         (pong (lambda (ball) (ping ball))))
  (ping 42))
{% endhighlight %}

Hier rufen sich zwei Funktionen, `ping` und `pong` bis in alle
Ewigkeit gegenseitig auf (ohne dass dem Programm irgendwann der
Speicher ausgehen würde).

<!--Beispiel in Haskell:
{% highlight haskell %}
numbers = 1 : map (+1) numbers
{% endhighlight %}
-->

Lazy evaluation, oder nicht-strikte Auswertung
-----------------

Bei einer strikten Auswertung, werden für eine Funktionsanwendung
zunächst die Argumente ausgewertet, bevor die Funktion aufgerufen,
d.h. mit der Auswertung des Funktionsrumpfes forgefahren wird.

Dies ist aber insbesondere dann nicht notwendig, wenn es nur reine
Ausdrücke gibt. Dann kann man die Auswertung der Argumente an die
Stelle verschieben, an der der Wert benötigt wird (wenn überhaupt),
ohne dass sich das Ergebnis dadurch ändern könnte.

Die meisten funktionalen Sprachen verfolgen dennoch eine strikte
Auswertungsstrategie von Funktionsaufrufen, aber z.B. fast immer eine
nicht-strikte Auswertung bei Operatoren wie dem logisches Oder, oder
bei bedingten Ausdrücken.

<!-- TODO
Beispiel in Hakell:
{% highlight haskell %}
getOrElse [] default = default
getOrElse (x : _) default = x

{% endhighlight %}
-->

Typinferenz
-----------

Ein statisches Typsystem, bei dem jedoch die Typen aller (oder der
meisten) Ausdrücke inferiert, d.h. automatisch hergeleitet werden
können, ist seit den 1970er Jahren in vielen funktionalen Sprachen
vorhanden.

Die neueste Forschung an der nächsten Generation von Typsystemen (z.B.
Dependent types) findet ebenfalls in funktionalen Sprachen statt.

<!--
Ein Wort zur Objekt-Orientierung
===
Smalltalk ist funktional (?)
--
    Actually I made up the term "object-oriented", and I can tell you I did not have C++ in mind.
    Alan Kay, OOPSLA 97
-->

Zusammenfassung
==============

Funktionale Programmierung ist also im Kern einfach nur das
Programmieren mit Lambda-Ausdrücken, hat aber in seiner langen
Geschichte viele ausgereifte Konzepte hervorgebracht, die nicht nur in
diesem Blog jede Woche erläutert werden, sondern auch in der Industrie
immer größere Verwendung finden.
