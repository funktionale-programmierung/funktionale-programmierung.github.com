---
layout: post
description: "Erfahrungsbericht zu Eclipse Xtend"
title: "Eclipse Xtend"
author: david-frese
tags: []
---

<!--
- Projekt im Eclipse-Umfeld, Kunde wollte kein Scala, Xtend war möglich, "immer noch besser als Java".
- Werbung als "Java 10 schon heute", "Erweiterung von Java".
- Implementiert in Xtext, kompiliert nach Java - sehr an Eclipse gebunden (Versionsprobleme)
- nicht gerade schnelle; anscheinend Speicherlecks.
- Integration in Eclipse schlechter als für Java (Find References funktioniert nur wenn man "Automatic build" an hat, und dann auch nicht richtig; Templates findet er gar nicht)

- Pro: Einige funktionale Elemente (Lambdas, Expression-basiert); aber nicht auf dem Level von Scala (keine Collections, Methoden sind keine Funktionen z.B.)
- Pro: Mehr als ein Element pro Datei; Exceptions müssen nicht deklariert/gefangen werden wie in Java.
- Con: Leider liegt kein richtiges Modell von Java zugrunde; dadurch kann generierter Java-Quellcode Fehler enthalten (was er auch häufig tut)
- Con: Leider fehlen einige Dinge die Java kann (es ist keine Erweiterung, sondern eine Nachimplementierung); z.B. anonyme Klassen (mit mehr als einem Member)

- Templates
- Extension methods
-->

Wir haben gerade ein Kundenprojekt im Eclipse- und EMF-Umfeld
fertiggestellt, das aus der Erstellung von mehreren Plugins für die
bestehende Anwendung des Kunden bestand. Wir haben dabei die
Programmiersprache [Xtend](https://www.eclipse.org/xtend/) eingesetzt.
Dieser Artikel geht auf unsere Erfahrungen mit Xtend ein, die, soviel
sei schon einmal vorweg genommen, eher negativ sind.

<!-- more start -->

Ursprünglich wollten wir das Projekt in
[Scala](http://www.scala-lang.org/) umsetzten, einer Sprache in der
man sehr leicht die zwei funktionalen APIs der beiden Hauptkomponenten
der zu entwickelnden Plugins hätte implementieren können. Der Kunde
wollte aber lieber bei Sprachen und Bibliotheken bleiben, die bereits
bei ihm im Einsatz waren. Dazu zählte auch Xtend, eine Sprache die
zumindest einige Konzepte der funktionalen Programmierung enthält, und
uns daher immer noch besser erschien als "plain Java".

Xtend wirbt damit, dass es eine Erweiterung von Java sei, und man mit
ihm "Java 10 schon heute" bekommen würde. Eine Behauptung die so nicht
stimmt, was ich im folgenden näher darlege.

Implementiert ist Xtend als Eclipse-Plugin auf der Basis von Xtext.
Xtext erlaubt die Definition von neuen Sprachen auf Basis einer
Grammatik und Übersetzungscode zu beliebigen anderen Sprachen, z.B.
Java. Es liefert einem außerdem mit relativ wenig Aufwand eine
Integration dieser Sprache in das Eclipse-Framework. Diese Integration
ist aber relativ spärlich, insbesondere verglichen mit der Integration
von Java. Dies muss man auch schon als einen ersten großen
Negativpunkt aufführen, da Dinge wie "Gehe zu Definition" und "Suche
Verwendungen" nur deutlich schlechter (z.B. erst nach einem Build), und
teilweise gar nicht funktionieren (bei sog. Templates).

Außerdem ist der Xtend-Compiler nicht gerade schnell, bzw. leckt
irgend eine Schicht der Implementierung offensichtlich Speicher,
wodurch die Kompilierung immer langsamer wird und regelmäßig Neustarts
von Eclipse notwendig werden. Ursprünglich beinhaltete unser Projekt
ca. 1500 generierte Xtend-Klassen, was die ganze Entwicklung aber
derart behindert hat, dass wir auf die Generierung von Java-Code für
diesen Teil umgestiegen sind.

Die Sprachelemente, die Xtend von Java abgrenzen, waren
nichtsdestotrotz sehr hilfreich für die Umsetzung des Projekts. Dazu
gehört der deklarative Grundansatz (keine Statements, alles sind
Expressions), sowie Funktionsliterale der Form

{% highlight java %}
  [ Type arg | body ]
{% endhighlight %}

Diese kann der Xtend-Compiler, ähnlich wie Java 8, autmatisch in
"Single Abstract Method" Typen konvertieren. Für den allgemeinen Fall
sind einige generische Funktionstypen mit enthalten. Listen und
Map-Literale macht insbesondere das Testen deutlich einfacher.
Auch dass alle Parameter per default "final", und "Variablen" per
default read-only sind, sowie die Typ-Inferenz von Variablen und
Rückgabetypen, macht das Programmieren in Xtend im allgemeinen schon
wesentlich angenehmer als in Java. Außerdem kann man Extension Methods
definieren und die Standardbibliothek tut dies auch reichlich für die
Java-Collections. Das macht beispielsweise folgenden Code möglich:

{% highlight java %}
  def static <A> filterFirst(List<A> all, (A) => boolean pred) {
    val matching = all.filter(pred)
    if (matching.empty)
      throw new Exception("...")
    else
      matching.get(0)
  }
{% endhighlight %}

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
(2.1), aber selbst in der aktuellsten Version (2.5) kann das immer
noch passieren.

Insgesamt hat uns Xtend die Umsetzung des Projekts sicherlich schon
erleichtert, aber nur im Vergleich zu einer Implementierung in Java.
Mit einer "erwachsenen" Programmiersprache, wie z.B. Scala, kann Xtend
(noch) nicht mithalten.
