---
layout: post
description: ""
title: "Spezifikation von Clojure-Datentypen mit clojure.spec"
author: marcus-crestani
tags: ["Clojure", "Spezifikation", "Datentypen", "clojure.spec"]
---

Clojure ist eine dynamische Programmiersprache, was unter anderem
bedeutet, dass auch die Datentypen erst dynamisch zur Laufzeit
feststehen.  Clojure kennt zwar sogenannte
[_type hints_](http://clojure.org/reference/java_interop#typehints),
die sollen aber dem Compiler helfen, effizienteren Code zu generien
und sind nicht dafür da, Typeigenschaften von Datenstrukturen zu
erzwingen.  Die
[`clojure-spec`-Bibliothek](http://clojure.org/about/spec) füllt diese
Lücke: Damit ist es möglich, die Struktur der Daten als Programmcode
zu spezifizieren und so die zur Laufzeit vorhandenen Daten zu
validieren.  Das führt zu weniger Fehlern während der Programmlaufzeit
und im Fehlerfall zu besseren Fehlermeldungen.  Zusätzlich ist sogar
möglich, Daten aus der Spezifikation zu generieren, zum Beispiel für
[randomisiertes Testen](http://funktionale-programmierung.de/2013/07/10/randomisierte-tests-mit-quickcheck.html).

Dieser Artikel gibt eine Einführung in `clojure.spec`.

<!-- more start -->

## Mitmachen ##

Über die Programmiersprache Clojure haben wir in diesem Blog
[bereits viel geschrieben](http://funktionale-programmierung.de/search.html?query=clojure),
zum Beispiel finden Sie den ersten Teil einer mehrteiligen Einführung
[hier](http://funktionale-programmierung.de/2014/11/27/clojure-first-steps.html).

Um die Beispiele in diesem Artikel nachzuvollziehen, reicht eine
Clojure-REPL mit einer Clojure-Version, in der `clojure.spec` bereits
enthalten ist, also `1.9.0-alpha10` oder neuer.  Das kriegen Sie mit
einer neuen Version der Clojure-IDE
[Nightcode](https://sekao.net/nightcode/) oder mit
[Leiningen](http://leiningen.org/) und einer Datei `project.clj` mit
folgendem Inhalt:

{% highlight clojure %}
(defproject specplay "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha10"]])
{% endhighlight %}

Dann können Sie aus dem Verzeichnis, in dem die `project.clj`-Datei
liegt, die Clojure-REPL mit `lein repl` starten.

Um mit `clojure.spec` loszulegen, müssen Sie die Bibliothek einbinden:

{% highlight clojure %}
(require '[clojure.spec :as s])
{% endhighlight %}

## Spezifikationen ##

Jede Spezifikation beschreibt die Menge von erlaubten Werten.  Eine
Spezifikation ist zum Beispiel das Prädikat `odd?`, das bestimmt, ob
ein Wert ungerade ist.  Ob ein Wert auf eine Spezifikation passt, sagt
`s/valid?`:

{% highlight clojure %}
(s/valid? odd? 23)
;; => true
{% endhighlight %}

`s/valid?` akzeptiert eine Spezifikation und einen Wert.  `s/valid?`
gibt zurück, ob der Wert mit der Spezifikation übereinstimmt.

{% highlight clojure %}
(s/valid? odd? 42)
;; => false
{% endhighlight %}

Als Spezifikationen können alle Funktionen verwendet werden, die einen
Wahrheitswert zurückgeben, insbesondere alle Konstrukte, die es
bereits in Clojure gibt, wie zum Beispiel eingebaute Prädikate,
anonyme Funktionen, Funktionalität aus importierten Java-Bibliotheken
und Mengenprädikate:

{% highlight clojure %}
(s/valid? string? "Hello, World!")
;; => true
(s/valid? #(> % 5) 10)
;; => true
(s/valid? #(> % 5) 0)
;; => false
(import java.util.Date)
(s/valid? inst? (Date.))
;; => true
(s/valid? #{:mon :tue :wed :thu :fri :sat :sun} :tue)
;; => true
(s/valid? #{:mon :tue :wed :thu :fri :sat :sun} 23)
;; => false
{% endhighlight %}

## Namen geben ##

Spezifikationen können mit `s/def` auch an globale Namen gebunden
werden, um sie einfach wiederverwenden zu können und auch um bessere
Fehlermeldungen zu erhalten.  Um konfliktfreie Spezifikationen über
mehrere Bibliotheken und Namespaces zu verwenden, sollten die Namen
der Spezifikation _namespaced keywords_ sein, also qualifizierte
Schlüsselwörter mit Namespace-Prefix.  In Clojure erzeugt man
qualifizierte Schlüsselworter mit zwei Doppelpunkten:

{% highlight clojure %}
(s/def ::day-of-week #{:mon :tue :wed :thu :fri :sat :sun})
{% endhighlight %}

Das obige Beispiel bindet die Spezifikation in Form eines
Mengenprädikats an das qualifizierte Schlüsselwort `::day-of-week`.
Die benannte Spezifikation kann dann so benutzt werden:

{% highlight clojure %}
(s/valid? ::day-of-week :tue)
;; => true
{% endhighlight %}

## Zusammengesetzte Daten  validieren ##

Auch für zusammengesetzte Datentypen können wir Spezifikationen
schreiben, hier am Beispiel mit Gürteltieren und Papageien aus einem
[früheren Blogeintrag](http://funktionale-programmierung.de/2015/04/27/clojure-records.html):

{% highlight clojure %}
(defrecord Dillo
  [weight alive?])

(defrecord Parrot
  [weight sentence])
{% endhighlight %}

Ein Gürteltier hat ein Gewicht und ist lebendig oder tod, als
Spezifikation wie folgt ausgedrückt:

{% highlight clojure %}
(s/def ::weight number?)

(s/def ::alive? boolean?)

(s/def ::dillo
  (s/and
   #(instance? Dillo %)
   (s/keys :req-un [::weight ::alive?])))
{% endhighlight %}

Um Records zu spezifizieren, nutzen wir, dass Spezifikationen
komponierbar sind, sie können also nach belieben zusammengesetzt
werden.  Ein Record ist gültig, wenn der Typ des Records stimmt und
wenn die Bestandteile des Records den entsprechenden Spezifikationen
entsprechen.  Da beide Vorgaben gelten müssen, verknüpfen wir beide
Bedingungen mit `s/and`.  Die erste Bedingung überprüfen wir mit einer
anonymen Funktion, die die eingebaute Funktion `instance?` nutzt.  Für
die zweite Bedingungen nutzen wir aus, dass Clojure-Records auch als
Map funktionieren (siehe unsere
[Einführung in Datentypen](http://funktionale-programmierung.de/2014/12/08/clojure-datenstrukturen.html)).
Die obige Spezifikation für Gürteltiere ist also tatsächlich eine
Spezifikation für eine Map, in der die Schlüsselwörter `::weight` und
`::alive?` als unqualifizierte Schlüssel enthalten sein müssen.  Das
Argument `:req-un` für `s/keys` legt fest, dass die Spezifikation
unqualifizerte Schlüssel akzeptieren soll, da Clojure Records mit
unqualifizierten Schlüsselwörtern implementiert (Gegenstück wäre das
Argument `:req`, das qualifizerte Schlüssel erwartet, die von
`clojure.spec` grundsätzlich empfohlen werden, um Namenskonflikte zu
vermeiden --- die Clojure-Records passen aber nicht dazu).

Damit können wir Gürteltiere validieren:

{% highlight clojure %}
(s/valid? ::dillo (->Dillo 23 true))
;; => true
(s/valid? ::dillo "Gürteltier")
;; => false
(s/valid? ::dillo (->Dillo :male true))
;; => false
{% endhighlight %}

Da die Datenstrukturen jetzt schon komplizierter sind, ist es auf den
ersten Blick gar nicht mehr so einfach, herauszufinden, warum die
letzte Validierung fehl schlägt.  `clojure.spec` kann uns bei der
Fehlersuche helfen:

{% highlight clojure %}
(s/explain ::dillo (->Dillo :male true))
;; => In: [:weight] val: :male fails spec: :user/weight at: [:weight] predicate: number?
{% endhighlight %}

Die Funktion `s/explain` liefert eine hilfreiche Fehlerbeschreibung, aus
der sofort klar wird, dass `:male` keine Zahl ist, die an der Stelle
eigentlich erwartet wird.

## Gemischte Daten validieren ##

Wir können auch gemischte Daten spezifizeren, indem wir die
Spezifikationen anders zusammensetzen.  Um Tiere zu repräsentieren,
die entweder ein Gürteltier oder ein Papagei sind, liefern wir
zunächst die Spezifikation für Papageien nach, die ein Gewicht haben
und einen Satz sagen können:

{% highlight clojure %}
(s/def ::sentence string?)

(s/def ::parrot
  (s/and
   #(instance? Parrot %)
   (s/keys :req-un [::weight ::sentence])))
{% endhighlight %}

Um die Spezifikation für Tiere zu erhalten, können wir die
Spezifikationen von Gürteltieren und Papageien mit `s/or` verknüpfen:

{% highlight clojure %}
(s/def ::animal
  (s/or :dillo ::dillo :parrot ::parrot))
{% endhighlight %}

Die `s/or`-Spezifikation erfordert eine Auswahl beim validieren.  Jede
Möglichkeit wird mit einem Namen versehen um den Zweigen Namen zu
geben, hier `:dillo` und `:parrot`, damit im Fehlerfall die
Fehlerbeschreibung zielgerichtet helfen kann.

{% highlight clojure %}
(s/valid? ::animal (->Parrot 5 "I like cookies"))
;; => true
(s/valid? ::animal (->Parrot 5 true))
;; => false
(s/explain ::animal (->Parrot 5 true))
;; val: #user.Parrot{:weight 5, :sentence true} fails spec: :user/dillo at: [:dillo] predicate: (instance? user.Dillo %)
;; In: [:sentence] val: true fails spec: :user/sentence at: [:parrot :sentence] predicate: string?
{% endhighlight %}

Der letze Aufruf erläutert, dass der Wert kein Tier ist, weil es kein
Gürteltier ist und auch kein gültiger Papagei, da der Wert des Satzes
keine Zeichenkette ist, wie in der Spezifikation gefordert.  Diese
detaillierten Fehlerbeschreibungen helfen weiter.

## Nutzung zur Laufzeit ##

Wir haben inzwischen gesehen, wie Hilfreich `clojure.spec` beim
interaktiven Entwickeln und Debuggen von Programmen sein kann.
Sinnvoll ist aber auch, die Fähigkeiten von `clojure.spec` zur
Laufzeit des Programms zu nutzen, um zur Laufzeit die Gültigkeit der
Daten zu überprüfen.  Dazu kann man zum Beispiel Prädikate mit Hilfe
des bereits bekannten `s/valid?` implementieren:

{% highlight clojure %}
(defn animal?
  [thing]
  (s/valid? ::animal thing))
{% endhighlight %}

Eine weitere Möglichkeit ist, mit `s/assert` sicherzustellen, dass ein
Wert eine Spezifikation erfüllt.  `s/assert` akzeptiert wie `s/valid?`
eine Spezifikation und einen Wert.  Passt die Spezifikation, gibt
`s/assert` den Wert zurück, ansonsten wirft es eine Exception:

{% highlight clojure %}
(s/check-asserts true)
(s/assert ::animal "Dog")
;; => ExceptionInfo Spec assertion failed...
{% endhighlight %}

Das Überprüfen der Assertions ist standardmäßig deaktiviert, die
Funktion `s/check-asserts` kann Assertions aktivieren.

## Noch viel mehr... ##

In diesem Artikel haben wir nur einen kleinen Teil der Fähigkeiten von
`clojure.spec` abgedeckt.  `clojure.spec` kann noch viel mehr: mit
Hilfe von Spezifikationen Eingaben parsen und destrukturieren,
Collections und Funktionen spezifizieren; die Code-Dokumentation
erleichtern und automatische Tests erzeugen.  Mehr davon in einem
späteren Artikel.

<!-- more end -->
