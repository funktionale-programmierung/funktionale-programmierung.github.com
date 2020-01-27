---
layout: post
description: Was ist funktionale Programmierung?
title: Was ist funktionale Programmierung?
author: david-frese
tags: ["Einführung", "funktional", "imperativ"]
---

Was ist überhaupt "Funktionale Programmierung?" Zur Beantwortung
dieser Frage dürften sich viele Neueinsteiger in das Thema an
Wikipedia wenden. Die entsprechende Seite in [deutscher
Sprache](http://de.wikipedia.org/wiki/Funktionale_Programmierung)
lässt aber einiges zu Wünschen übrig, was mich dazu veranlasst hat den
Titel dieses Blogs einmal grundlegend zu erklären.

<!-- more start -->

Der Begriff funktionale Programmierung ist nicht eindeutig definiert,
aber man kann sich ihm aus drei verschiedenen Richtungen nähern. Der
Begriff kann für das sogenannte Funktionale Paradigma stehen, was
soetwas wie eine Minimalanforderung ist. Oder er steht für das
Programmieren in funktionalen Sprachen, was schon eine sehr viel
restriktivere Definition darstellt. Schließlich kann er auch als
Oberbegriff für "typisch funktionale" Programmierkonzepte stehen, die
auch in anderen Sprachen Verbreitung finden. Zunächst zum Begriff des
Funktionalen Paradigmas.

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
enthält. Man beschreibt also _wie_ man zu dem Ergebnis gelangt.

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
Programmiersprache Scheme gewählt, vielleicht weil hier das Wort
_Lambda_ direkt vorkommt. Hier eine Funktion die zu einer Zahl 2
addiert:

{% highlight scheme %}
(lambda (x) (+ x 2))
{% endhighlight %}

Die nächste Version von Java, Java 8, wird aber zum Beispiel auch
Lambda-Ausdrücke enthalten. Die gleiche Funktion kann dann dort so
notiert werden:

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
sind, beziehungsweise das relativ grobe Konzept des funktionalen
Paradigmas in verschiedene Richtungen weiter untergliedern. Hier kann
nur eine kleine Auswahl aufgelistet werden:

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

Der D-Compiler stellt hier sicher, dass die Funktion `square` auf keine
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

Lazy evaluation, oder nicht-strikte Auswertung
-----------------

Bei einer strikten Auswertung werden für eine Funktionsanwendung
zunächst die Argumente ausgewertet, bevor die Funktion aufgerufen,
d.h. mit der Auswertung des Funktionsrumpfes forgefahren wird.

Dies ist aber insbesondere dann nicht notwendig, wenn es nur reine
Ausdrücke gibt. Dann kann man die Auswertung der Argumente an die
Stelle verschieben, an der deren Wert benötigt wird, ohne dass sich
das Ergebnis dadurch ändern könnte. Außerdem kann auf die Auswertung
auch komplett verzichtet werden, wenn der Wert überhaupt nicht
gebraucht wird.

Die meisten funktionalen Sprachen verfolgen dennoch eine strikte
Auswertungsstrategie von Funktionsaufrufen, aber z.B. fast immer eine
nicht-strikte Auswertung bei Operatoren wie dem logisches Oder, oder
bei bedingten Ausdrücken.

Ein Beispiel in Haskell, einer Sprache mit nicht-strikter Auswertung
ist folgendes. Angenommen wir haben schon eine Funktion die Quick-Sort
implementiert, also einen Sortieralgorithmus, bei dem das erste
Element einer Liste genommen wird, dann rekursiv die beiden Listen
aller Elemente die kleiner bzw. größer sind sortiert werden, und diese
drei Teile dann wieder passend aneinander gehängt werden:

{% highlight haskell %}
quickSort [] = []
quickSort (x:xs) = quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)
{% endhighlight %}

Möchten wir nun eine Funktion definieren, die das kleinste Elemente
einer Liste zurückgibt, dann können wir einfach schreiben, dass wir
das erste Element der sortierten Liste haben wollen:

{% highlight haskell %}
minimum xs = head (quickSort xs)
{% endhighlight %}

Die Funktion `quickSort` wird dabei dann automatisch nur so
weit ausgewertet, wie es notwendig ist, um zu ermitteln welches das
erste Element ist.

Typinferenz
-----------

Ein statisches Typsystem, bei dem jedoch die Typen aller (oder der
meisten) Ausdrücke inferiert, d.h. automatisch hergeleitet werden
können, ist seit den 1970er Jahren in vielen funktionalen Sprachen
vorhanden.

An dem Beispiel von gerade eben, der Funktionen `miminum` und
`quickSort` kann man das schon gut erkennen:

{% highlight haskell %}
minimum xs = head (quickSort xs)
{% endhighlight %}

Es ist nicht nötig hier die Typen der Parameter oder des Rückgabewerts
zu deklarieren. Der Compiler inferiert den allgemeinsten Typ für die
Funktion `minimum` und würde eine Fehlermeldung ausgeben, wenn man sie
beispielsweise mit einer Zahl aufrufen wollte.

Zusammenfassung
==============

Funktionale Programmierung ist also im Kern einfach nur das
Programmieren mit Lambda-Ausdrücken, hat aber in seiner langen
Geschichte viele ausgereifte Konzepte hervorgebracht, die nicht nur in
diesem Blog jede Woche erläutert werden, sondern auch in der Industrie
immer größere Verwendung finden.

Hier noch ein paar Links auf frühere Blog-Artikel, die diese und
weitere Konzepte besonders beleuchten:

- [Eine kleine Einführung in die rein funktionale Programmierung]({% post_url 2013-03-12-rein-funktional %})
- [Warum funktional?]({% post_url 2013-03-20-warum-funktional %})
- [Funktionale API für JasperReports]({% post_url 2013-06-13-funktionale-api-jasper %})
- [Zeitreisen mit persistenten Datenstrukturen]({% post_url 2013-06-21-persistente-datenstrukturen %})
