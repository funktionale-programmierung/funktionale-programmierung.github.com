---
layout: post
description: "Erfahrungsbericht zu Eclipse Xtend"
title: "Eclipse Xtend"
author: david-frese
tags: ["Eclipse", "Xtend", "Java"]
---

Wir haben gerade ein Kundenprojekt im [Eclipse](http://eclipse.org/)-
bzw.
[Eclipse-Modeling-Framework](http://www.eclipse.org/modeling/emf/)-Umfeld
fertiggestellt, das aus der Erstellung von mehreren Plugins für die
bestehende Anwendung des Kunden bestand. Wir haben dabei die
Programmiersprache [Xtend](https://www.eclipse.org/xtend/) eingesetzt.
Dieser Artikel geht auf unsere Erfahrungen mit Xtend ein, die, soviel
sei schon einmal vorweg genommen, eher negativ sind.

<!-- more start -->

Ursprünglich wollten wir das Projekt in
[Scala](http://www.scala-lang.org/) umsetzten, einer Sprache in der
man sehr leicht die zwei [funktionalen
APIs](/2013/06/13/funktionale-api-jasper.html), die den Kern des
entwickelnten Plugins bilden, hätte implementieren können. Die eine
API ist eine kompositionale DSL für das Format tabellarischer Daten,
die andere ist ebenfalls kompositional und dient der Definition von
Update-Operationen eines EMF-Modell. Der Kunde wollte aber lieber bei
Sprachen und Bibliotheken bleiben, die bereits bei ihm im Einsatz
waren. Dazu zählte auch Xtend, eine Sprache die zumindest einige
Konzepte der funktionalen Programmierung enthält, und uns daher immer
noch besser erschien als "plain Java".

Xtend wirbt damit, dass es eine Erweiterung von Java sei, und man mit
ihm "Java 10 schon heute" bekommen würde. Eine Behauptung die so nicht
stimmt, was ich im Folgenden näher darlege.

Implementiert ist Xtend als Eclipse-Plugin auf der Basis von
[Xtext](http://www.eclipse.org/Xtext/). Xtext erlaubt die Definition
von neuen Sprachen auf Basis einer Grammatik und Übersetzungscode zu
beliebigen anderen Sprachen, z.B. Java. Es liefert einem außerdem mit
relativ wenig Aufwand eine Integration dieser Sprache in das
Eclipse-Framework. Diese Integration ist aber eher spärlich,
insbesondere verglichen mit der Integration von Java. Dies muss man
auch schon als einen ersten großen Negativpunkt aufführen, da Dinge
wie "Gehe zu Definition" und "Suche Verwendungen" nur deutlich
schlechter (z.B. erst nach einem Build), und teilweise gar nicht
funktionieren (bei sog. Templates).

Außerdem ist der Xtend-Compiler nicht gerade schnell, bzw. leckt
irgendeine Schicht der Implementierung offensichtlich Speicher,
wodurch die Kompilierung immer langsamer wird und regelmäßig Neustarts
von Eclipse notwendig werden. Ursprünglich beinhaltete unser Projekt
z.B. circa 1500 von uns generierte Xtend-Klassen. Diese Menge hat die
ganze Entwicklung aber derart behindert, dass wir für diesen Teil auf
die Generierung von Java-Code umgestiegen sind. Das hat die Sache
etwas entschärft.

Die Sprachelemente, die Xtend von Java abgrenzen, waren
nichtsdestotrotz sehr hilfreich für die Umsetzung des Projekts. Dazu
gehört der deklarative Grundansatz (keine Statements, alles sind
Expressions), sowie Funktionsliterale der Form

{% highlight java %}
[ Type arg | body ]
{% endhighlight %}

Diese kann der Xtend-Compiler, ähnlich wie Java 8, automatisch in
"Single-Abstract-Method"-Typen konvertieren. Für den allgemeinen Fall
sind einige generische Funktionstypen mit enthalten. Listen und
Map-Literale machen insbesondere das Testen deutlich einfacher. Auch
dass alle Parameter und "Variablen" per default "final" sind, also
nicht neu gebunden werden können, sowie die Typ-Inferenz von Variablen
und Rückgabetypen, machen das Programmieren in Xtend im allgemeinen
schon wesentlich angenehmer als in Java. Außerdem kann man Extension
Methods definieren und die Standardbibliothek tut dies auch reichlich
für die Java-Collections. Das macht beispielsweise folgenden Code
möglich:

{% highlight java %}
def static <A> filterFirst(List<A> all, (A) => boolean pred) {
  val matching = all.filter(pred)
  if (matching.empty)
    throw new Exception("...")
  else
    matching.get(0)
}
{% endhighlight %}

Der Typ `List` ist hier die Standard-Java-Collection `java.util.List`,
und `filter` eine Extension Method die die Xtend-Bibliothek hinzufügt.

Was dann aber wiederum sehr lästig ist, ist dass man keine "anonymen
Klassen" wie in Java definieren kann. Es gibt keine Expression zur
Instantiierung einer abstrakten Klasse mit mehr als einer Methode.
Gerade bei einem funktionalen Programmierstil stößt man sehr schnell
an dieses, gegenüber Java fehlende Sprachelement. Auch "inner classes"
gibt es nicht, und auf der Mailingliste taucht immer wieder der
Hinweis auf "das musst du dann in Java machen". Daher kann man Xtend
meiner Meinung nach auch nicht als Erweiterung von Java bezeichnen.
Die Entwickler versuchen zwar alles zu bieten was Java "kann", aber an
dieser und anderen Stellen fehlt etwas (bis zur aktuellen Version 2.5
fehlt es zumindest immer noch).

Ein weiteres Grundproblem von Xtend ist, dass der Compiler
offensichtlich kein korrektes bzw. vollständiges Modell von Java
verwendet. Immer wieder passiert es, dass der Xtend-Code fehlerfrei zu
sein scheint und der Compiler den Java-Code generiert hat, dass aber
anschließend der Java-Compiler einen Fehler im generierten Code
meldet. Ganz besonders schlimm war das in etwas älteren Versionen
(2.1), aber selbst in der aktuellen Version (2.5) kann das immer
noch passieren.

Insgesamt hat uns Xtend die Umsetzung des Projekts sicherlich
erleichtert, aber nur im Vergleich zu einer Implementierung in Java.
Mit einer "erwachsenen" Programmiersprache, wie z.B. Scala, kann Xtend
(noch) nicht mithalten.
