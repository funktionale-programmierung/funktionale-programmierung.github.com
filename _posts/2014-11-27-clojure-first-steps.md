---
layout: post
description: "Einführung in Clojure, erste Gehversuche"
title: "Einführung in Clojure, erste Gehversuche"
author: michael-sperber
tags: ["Clojure"]
---

Dieses Posting beginnt eine kurze Reihe mit einer Einführung in die
funktionale Sprache [Clojure](http://clojure.org/).  Clojure ist eine
funktionale Sprache für die Java-Plattform, die schon des öfteren
durch unser Blog gegeistert ist (zum Beispiel [hier]({% post_url 2013-06-27-dsl-clojure %})).
Clojure ist in aktiver Entwicklung und hat
schon vor Jahren die für industrielle Projekte nötige Reife erreicht.
Ihre Schwestersprache
[ClojureScript](http://clojure.org/clojurescript), die nach JavaScript
kompiliert wird und so im Browser läuft, hatten wir schon
[hier]({% post_url 2014-02-14-clojurescript-react %}) und 
[hier]({% post_url 2014-11-11-funktional-frontend-clojurescript %}) behandelt.

In diesem Posting zeigen wir, was Sie für einen einfachen
Clojure-Einstieg an Software installieren müssen und wie Sie die
ersten Gehversuche machen können.

# Übrigens ...

Clojure ist auch Thema der Konferenz
[:clojured](http://www.clojured.de/), die am 24. Januar in Berlin
stattfindet.  Sie lässt sich zum Doppelpack mit der [BOB
2015](http://bobkonf.de) am Tag davor kombinieren!

<!-- more start -->

# Handwerkszeug

Es ist zwar möglich, Clojure "ohne alles" zu verwenden, es ist aber -
wie bei allen JVM-Sprachen - sinnvoll, ein Werkzeug zu benutzen, um
den Build-Prozess, den Classpath etc. zu verwalten.  Manuell macht das
nur wenig Spaß.  Die meisten Wege zu Clojure führen daher über
[Leiningen](http://leiningen.org/), das viele Aufgaben im Zusammenhang
mit der Clojure-Entwicklung automatisiert.  Es gibt außerdem
zahlreiche IDE- und Editor-Plugins (für
[Eclipse](https://code.google.com/p/counterclockwise/),
[IntelliJ](https://cursiveclojure.com/),
[Emacs](https://github.com/clojure-emacs/clojure-mode),
[Vim](https://github.com/guns/vim-clojure-static)).  Alle o.g. Plugins
kommen auch mit Leiningen-Unterstützung und bieten andere
Erleichterungen wie Syntax-Highlighting, Completion usw.

Am einfachsten ist der Einstieg mit der kleinen IDE
[Nightcode](https://nightcode.info/).  (Da ist Leinigen gleich schon
mit drin.)  Nightcode lässt sich in Form eines einzelnen Jar-Files
herunterladen.  Einfach draufklicken oder starten mit:

    java -jar nightcode-0.4.2-standalone.jar

Dann auf `New Project` klicken, als Projekttyp `Console` anklicken,
einen Namen auswählen (in diesem Posting `fp1`), und los geht's!

Nightcode legt schon ein Verzeichnis mit ein paar Dateien an.  Wir
arbeiten erstmal in `src/fp1/core.clj`, die Nightcode schon angelegt
hat.  Einfach im Projektüberblick links drauf klicken.  Es erscheint
folgendes kleine Programm:

{% highlight clojure %}
(ns fp1.core
  (:gen-class))

(defn -main
  []
  (println "Hello, World!"))
{% endhighlight %}

Was das Programm tut, liegt nahe - über dem unteren Fenster gibt es
einen `Run`-Knopf: Draufdrücken, und schon erscheint der erwartete
Output.  (Das dauert ziemlich lange, weil Nightcode erst einmal eine
JVM hochfahren muss.  Mit Clojure selbst hat das wenig zu tun.)

Ignorieren wir für die Zwecke dieser Einführung erstmal den
`ns`-Header und schauen uns die Definition von `-main` an.  Clojure
verrät sich sofort als Lisp-Dialekt: Zusammengesetzte Formen sind
immer von Klammern umschlossen (wobei Clojure neben den Lisp-üblichen
runden auch eckige und geschweifte Klammern kennt) und durch
Whitespace getrennt.  Diese Formen sind in Präfix-Notation; die erste
Teilform nach der offenen Klammer sagt also, um was es sich handelt.

Eine `defn`-Form definiert eine Funktion (hier namens `main-` - der
Bindestrich sorgt dafür, dass Clojure die
Java-Standard-`main`-Methode generiert) ohne
Parameter (dafür steht das `[]`), die, wenn sie aufgerufen wird,
ihrerseits die eingebaute Funktion `println` aufruft.

Keine große Sache also, wenn man sich an die vielen Klammern gewohnt
hat.  A propos Klammern: Wer noch nie in einem Lisp-Dialekt
programmiert ist, mag vielleicht denken, es sei umständlich, die immer
zu zählen.  Das ist allerdings nicht nötig - wenn der Cursor auf einer
Klammer sitzt, zeigt Nightcode (sowie alle anderen Clojure-IDEs) die
dazugehörige andere Klammer an.  Außerdem benutzen
Clojure-Programmierer *Einrückung* - in einem stark standardisierten
Format - um die Struktur des Codes lesbar zu machen.  Ein Druck auf
die Tab-Taste, und Nightcode rückt die aktuelle Zeile sinnvoll ein.

Fürs Lernen ist die Nightcode-*REPL* praktisch (REPL steht für
"Read-Eval-Print-Loop"), in der wir Ausdrücke eingeben können und
sofort das Ergebnis ausgedruckt bekommen.  Dazu gibt es über dem
unteren Fenster den Knopf `Run with REPL` - einfach mal draufdrücken
und im Fenster `(-main)` eingeben und auf Return drücken.  Das sollte
dann etwa so aussehen:

{% highlight clojure %}
fp1.core=> (-main)
Hello, World!
nil
{% endhighlight %}

`(-main)` ist ein *Aufruf* der Funktion `-main` - wenn Clojure diesen
auswertet, kommt zunächst die Ausgabe.  Außerdem wird auch der
*Rückgabewert* von `-main` gedruckt, nämlich `nil`, was soviel heißt
wie "nichts" - es gibt hier keinen sinnvollen Rückgabewert.  Versuchen
wir ein paar weitere Ausdrücke:

{% highlight clojure %}
fp1.core=> (+ 23 42)
65
fp1.core=> (+ (* 3 4) 42)
54
fp1.core=> (>= 4 3)
true
fp1.core=> (>= 3 4)
false
{% endhighlight %}

Sie sehen - alles in Präfix-Notation und vollständig geklammert,
ungewohnt aber leicht zu merken.

Als nächstes legen wir eine eine *Definition* an, die einen Wert an
einen Namen bindet.  Dazu schreiben wir ins obere Editor-Fenster
folgendes:

{% highlight clojure %}
(def pi 3.14159265)
{% endhighlight %}

Damit die Definition im REPL-Fenster sichtbar wird, ist es notwendig,
neu zu laden.  Dazu erstmal im Editor-Fenster `Save` (wird
gern vergessen) und dann über dem REPL-Fenster auf `Reload` drücken.
Nun können wir im REPL-Fenster auf `pi` zurückgreifen:

{% highlight clojure %}
fp1.core=> pi
3.14159265
{% endhighlight %}

Die Definition von `pi` können wir benutzen, um eine "richtige"
Funktion zu schreiben, die den Umfang eines Kreises mit bekanntem
Radius ausrechnet - und zwar wieder ins Editor-Fenster oben:

{% highlight clojure %}
(defn circumference
   [radius]
   (* 2 pi radius))
{% endhighlight %}

Diese Funktion hat zwischen den eckigen Klammern den *Parameter*
`radius`, der im Rumpf vorkommen kann.  Nach `Save` und `Reload`
können wir die Funktion in der REPL aufrufen:

{% highlight clojure %}
fp1.core=> (circumference 5)
31.4159265
{% endhighlight %}

Diese ersten Gehversuche geben hoffentlich schon einen ersten
Vorgeschmack auf Clojure und erlauben Ihnen, zumindest einfache
mathematische Funktionen selbst zu programmieren.  Etwas mehr in die
Tiefe steigen wir dann mit einem zukünftigen Blog-Posting ein, das
dieses hier fortsetzen wird.

# Material

Wer nach einem Buch über Clojure sucht, sollte berücksichtigen, dass
die Sprache sich gerade in den letzten Jahren stark weiterentwickelt
hat.  Darum behandeln viele Bücher
[hier](http://clojure.org/books) entscheidende Sprachelemente noch
nicht.  Am ehestens scheint uns das [Clojure
Programming](http://www.clojurebook.com/) empfehlenswert.

<!-- more end -->




