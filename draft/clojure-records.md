---
layout: post
description: "Zusammengesetzte Daten in Clojure"
title: "Zusammengesetzte Daten in Clojure"
author: michael-sperber
tags: ["Clojure"]
---

Dieses Posting setzt unsere Clojure-Einführung (hier [Teil 1]({% post_url 2014-11-27-clojure-first-steps %}),
[Teil 2]({% post_url 2014-12-08-clojure-datenstrukturen %}), [Teil
3]({% post_url 2015-03-12-clojure-conditional %})) fort.  Dieses Mal
geht es um zusammengesetzte Daten.

<!-- more start -->

In [Teil 2]({% post_url 2014-12-08-clojure-datenstrukturen %}) der
Clojure-Einführung haben wir uns mit den eingebauten Datenstrukturen
beschäftigt.  Heute behandeln wir die Definition neuer
zusammengesetzter Datentypen.

Als Beispiel gehen wir in einen Computerladen und stellen uns einen
Individualrecher aus Einzelteilen zusammen.  Um diese Zusammenstellung
zu beschreiben (zum Beispiel für einen Online-Shop), entwerfen wir
eine Repräsentation als Daten.  Dieser Prozeß fängt an mit einer
Datendefinition:

{% highlight clojure %}
; Ein Computer besteht aus:
; - Prozessor
; - Hauptspeicher-Kapazität in Gbyte
; - Festplatten-Kapazität in Gbyte
{% endhighlight %}

Die Datendefinition sagt klar aus, dass ein Computer aus mehreren
Teilen besteht, wir brauchen also zusammengesetzte Daten.  In Clojure
sind für zusammengesetzte Daten *Records* zuständig, und wir können
einen Record-Typ für zusammengesetzte Daten mit der
[`defrecord`](http://conj.io/store/v0/org.clojure/clojure/1.6.0/clj/clojure.core/defrecord/)-Form
definieren:

{% highlight clojure %}
(defrecord Computer
    [processor ram hard-drive])
{% endhighlight %}

Diese Definition stellt eine Java-Klasse names `Computer` her, mit
Feldern `processor`, `ram` und `hard-drive`.  (Java-Programmierer
würden `Computer` eine [POJO-Klasse](http://de.wikipedia.org/wiki/Plain_Old_Java_Object) nennen.  Da Java-Klassen
in der Regel mit einem Großbuchstaben anfangen, übernehmen wir die
Konvention für Record-Typen.)  Damit können wir das in Clojure
eingebaute
[`new`](http://conj.io/store/v0/org.clojure/clojure/1.6.0/clj/clojure.core/new/)-Konstrukt
verwenden, um `Computer`-Objekte herzustellen:

{% highlight clojure %}
(def gamer (new Computer "Cell" 4 1000)) ; Cell, 4 Gbyte RAM, 1000 Gbyte Festplatte
(def workstation (new Computer "Xeon" 2 500)) ; Xeon, 2 Gbyte RAM, 500 Gbyte Festplatte
{% endhighlight %}

Wir können `new Computer` abkürzen mit `Computer.` (beachten Sie den Punkt):

{% highlight clojure %}
(def workstation (Computer. "Xeon" 2 500))
{% endhighlight %}

Allerdings ist zu beachten, dass `Computer.` keine Funktion, sondern
ein Makro ist.  Wenn wir versuchen, sie als eigenständiges Objekt zu
verwenden, passiert folgendes:

{% highlight clojure %}
records> Computer.
CompilerException java.lang.ClassNotFoundException: Computer.
{% endhighlight %}

Clojure stellt aber glücklicherweise auch noch eine
Konstruktor-Funktion namens `->Computer` zur Verfügung:

{% highlight clojure %}
(def workstation (->Computer "Xeon" 2 500))
{% endhighlight %}

Die einzelnen Teile eines Records können wir folgendermaßen
extrahieren:

{% highlight clojure %}
records> (:processor gamer)
"Cell"
records> (:ram gamer)
4
records> (:hard-drive gamer)
1000
{% endhighlight %}

Die Keywords mit den Feldnamen fungieren also als *Selektoren* - das
liegt daran, dass in Clojure jeder Record auch als Map funktioniert.
(Siehe unsere 
[Einführung in Datentypen]({% post_url 2014-12-08-clojure-datenstrukturen %}).)

Das reicht eigentlich schon, um mit Records in Clojure zu hantieren.
Allerdings tauchen zusammengesetzte Daten oft auch als Fälle in
gemischten Daten auf, wofür wir noch ein *Prädikat* für einen
Record-Typ brauchen, also eine Möglichkeit, Objekte der
Record-Definition von anderen Objekten zu unterscheiden.

Wir schreiben zur Illustration ein Programm zur Fütterung zweier
Sorten Tiere: Gürteltiere und Papageien.

Hier ist eine Datendefinition für Gürteltiere, eine dazu passende
Record-Definition und ein Beispiel-Gürteltier:

{% highlight clojure %}
; Ein Gürteltier hat folgende Eigenschaften:
; - Gewicht (in g)
; - lebendig oder tot
(defrecord Dillo
    [weight alive?])

(def d1 (->Dillo 55000 true)) ; 55 kg, lebendig 
{% endhighlight %}

... und hier ist das gleiche nochmal für den Papagei:

{% highlight clojure %}
; Ein Papagei hat folgende Eigenschaften:
; - Gewicht in Gramm
; - Satz, den er sagt
(defrecord Parrot
  [weight sentence])

(def p1 (->Parrot 10000 "Der Gärtner war's.")) ; 10kg, Miss Marple
{% endhighlight %}

Ein Gürteltier zu füttern erhöht dessen Gewicht, zumindest wenn es
noch lebt:

{% highlight clojure %}
(defn feed-dillo
  "Gürteltier füttern."
  [d]
  (if (:alive? d)
    (->Dillo (+ (:weight d) 500) true)
    d))
{% endhighlight %}

Bei einem Papagei klappt das immer:

{% highlight clojure %}
(defn feed-parrot
  "Papagei füttern."
  [p]
  (->Parrot (+ (:weight p) 50)
            (:sentence p)))
{% endhighlight %}

Nun schreiben wir eine Funktion, die sich sowohl für Gürteltiere als
auch für Papageien zuständig fühlt.  Die Datendefinition dafür sieht
so aus:

{% highlight clojure %}
; Ein Tier ist eins der folgenden:
; - ein Gürteltier
; - ein Papagei
{% endhighlight %}

Um eine Funktion `feed-animal` zu schreiben müssen wir eine 
[Verzweigung]({% post_url 2015-03-12-clojure-conditional %}) je
nachdem machen, ob es sich bei einem Tier um ein Gürteltier oder einen
Papagei handelt. Das funktioniert mit
[`instance?`](http://conj.io/store/v1/org.clojure/clojure/1.7.0-alpha4/clj/clojure.core/instance%3F/).
`(instance? C x)` testet, ob `x` eine Instanz der Klasse `C` ist,
insbesondere also, ob `x` zum Record-Typ `C` gehört:

{% highlight clojure %}
(defn feed-animal
  "Tier füttern."
  [a]
  (cond
   (instance? Dillo a) (feed-dillo a)
   (instance? Parrot a) (feed-parrot a)))
{% endhighlight %}

Damit wären alle Bestandteile beisammen, die wir für den Umgang mit
neuen Typen für zusammengesetzte Daten brauchen: Konstruktor,
Selektoren und Prädikat.  Genug für heute!
