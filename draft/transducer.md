---
layout: post
description: Transducer: Komposition, Abstraktion, Performance
title: "Transducer: Komposition, Abstraktion, Performance"
author: marco-schneider
tags: ["Clojure", "Transducer", "Abstraktion", "Performance"]
---

Funktionen höherer Ordnung wie `map`, `fold`, `filter` sind aus keinem funktionalen Programm wegzudenken.
Mit ihrer Flexibilität sind sie das Mittel der Wahl für Operationen auf Kollektionen aller Art.
Allerdings beschränkt sich ihr Anwendungsbereich nicht nur auf klassische Listen oder Vektoren.
In diesem Artikel betrachten wir fundamentalere Eigenschaften dieser Operationen und werfen
insbesondere einen Blick auf die sogennanten Transducer.

<!-- more start -->

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## Alles ist ein fold ##

Wer in der Einführung in die Informatik gut aufgepasst hat (und zugegeben etwas Glück mit der Auswahl der Themen hatte),
wird von dieser Aussage nicht überrascht sein. 
Um unser Gedächtnis etwas aufzufrischen betrachten wir zuerst eine Variante der üblichen Definitionen von `map` und `filter`.
(Hinweis: Alle Beispiele sind in Clojure geschrieben, daher nennen wir `fold` ab diesem Punkt bei seinem Clojure-Namen `reduce`):

{% highlight clojure %}
(defn map
  "Takes a function f and applies it to every element of xs."
  [f xs]
  (if (empty? xs)
    xs
    (cons (f (first xs)) (map f (rest xs)))))

(defn filter
  "Takes a predicate pred and returns a list with all elements
  of xs that satisfy pred."
  [pred xs]
  (if (empty? xs)
    xs
    (if (pred (first xs))
      (cons (first xs) (filter pred (rest xs)))
      (filter pred (rest xs)))))
{% endhighlight %}

So oder so ähnlich finden sich viele Defintionen und verschiedenen Standartbibliotheken. 
Wie versprochen lassen sich beide Funktionen auch über `reduce` definieren:

{% highlight clojure %}
(defn mapping
  "Takes a function f and returns a function.
    The returned function takes a collection acc 
    and an element x and conjoins x applied to f to acc."
  [f]
  (fn [acc x] (conj acc (f x))))

(defn map
  "Takes a function f and applies it to every element of xs."
  [f xs]
  (reduce (mapping f) [] xs))

(defn filtering
  "Takes a predicate pred and returns a function.
    The returned function takes a collection acc
    and an element x and conjoins x if it satisfies pred."
  [pred]
  (fn [acc x] (if (pred x) (conj acc x) acc)))

(defn filter
  "Takes a predicate pred and returns a list with all 
    elements of xs that satisfy pred."
  [pred xs]
  (reduce (filtering pred) [] xs))
{% endhighlight %}

Würden unsere Programme ausschliesslich aus Listenverarbeitung bestehen könnten wir an dieser Stelle zufrieden aufhören.
Etwas sticht jedoch ins Auge: sieht man einmal von dem Aufruf an `conj` ab verraten uns die Definitionen von `mapping` und `filtering`
nichts darüber, dass sie "nur" auf Kollektionen arbeiten!

## Von Kollektionen zu Prozessen ##

Was bedeutet es nun, dass `mapping` und `filtering` unabhängig von `conj` definierbar sind?
Für diese Überlegung ist es hilfreich nicht an Listen sondern Sequenzen von Schritten zu denken.
`mapping` und `filtering` nehmen hierbei die Rolle von "Prozessmodifikationen" an: sie nehmen einen Schritt entgegen und liefern eine modifizierte Version dieses Schrittes.
Definieren wir also eine über diesen "Schritt" parametrisierte Version unserer Funktionen:

{% highlight clojure %}
(defn mapping
  [f]
  (fn [step]
    (fn [acc x] (step acc (f x)))))

(defn map
  [f xs] 
  (reduce ((mapping f) conj) [] xs))

(defn filtering
  [pred]
  (fn [step]
    (fn [acc x] (if (pred x) (step acc x) acc))))

(defn filter 
  [pred xs]
  (reduce ((filtering pred) conj) [] acc))
{% endhighlight %}

Betrachten wir nun die daraus resultierenden Definitionen von `mapping` und `filtering` stellen wir fest, dass die Datenstruktur, auf die diese nun operieren
über den `step` Parameter festgelegt wird. Das bedeutet, dass wir nun nicht mehr von klassischen Kollektionen abhängig sind, sondern uns (wie wir später noch sehen werden), 
mehr oder weniger beliebige Datenstrukturen vornehmen und diese mit Hilfe von `mapping` und `filtering` verarbeiten können.

Eine Überlegung für diesen Schritt steht aber noch aus: während Listen und Vektoren es einfach machen, über einen "Anfang" und ein "Ende" nachzudenken,
von dem wir in `map` und `filter` Gebrauch machen, sieht es mit Datenstrukturen wie Streams oder Signalen anders aus.
Anstatt uns auf die Datenstruktur zu verlassen gehen wir einen anderen Weg:

1. Unsere Transformatoren (`mapping`, `filtering`) sollten von sich aus ein Konzept von "Anfang" und "Ende" haben.
2. Wir erwarten von unserer `step` Funktion nicht nur, dass sie binär ist, sondern auch, dass sie, aufgerufen mit keinem oder einem Argument, einen "Null"-Wert produziert.
   Beispiele hierfür wären in Clojure `(conj) => []`, `(conj [1]) => [1]` oder `(+) => 0`, `(+ 1) => 1`, etc.

Wir kümmern uns an dieser Stelle nur um Punkt 1. Ich werde zuerst eine Implementierung von `mapping` und `filtering` vorschlagen

{% highlight clojure %}
(defn mapping
  [f]
  (fn [step]
    (fn
      ([] (step))  ; 1.
      ([acc] (step acc))  ; 2.
      ([acc x] (step acc (f x)))  ; 3.
    )))

(defn filtering
  [pred]
  (fn [step]
    (fn
      ([] (step))
      ([acc] (step acc))
      ([acc x] (if (pred x)
                  (step acc x)
                  acc)))))
{% endhighlight %}

Auf den ersten Blick sieht das vielleicht etwas unintuitiv aus, ist aber am Beispiel von `mapping` schnell erklärt:

1. Dieser Fall deckt den "Anfang" eines Prozesses ab. Es wurde noch nichts berechnet und es liegt noch keine "nächste" Berechnung vor. 
   In diesem Fall wollen wir von unserer `step` Funktion ein "neutrales" Element, mit dem wir die Bechnung lostreten können.
2. Hier signalisieren wir das "Ende" eins Prozesses. Es kommmt kein Element mehr nach, also machen wir den letzten Schritt mit dem schon vorliegenden "Ergebnis".
3. Schliesslich existiert noch unser Fall, den wir schon von unseren Listen kennen.

Diese Funktionen sind nun so weit parametrisiert, dass wir beliebige Prozesse damit ausdrücken und, vermutlich noch wichtiger, mehrere solcher Prozessmodifikationen
hintereinander ausführen (oder komponieren) können!

Wie sieht das in der Realität aus?


## Beispiele für Prozessmodifikatoren ##

Im folgenden möchte ich zwei Beispiele dafür geben wie uns das Ganze nun in der echten Welt hilft und wann wir dieses Vorgehen möglicherweise der regulären Verkettung von Listenoperationen vorziehen wollen.

In beiden Beispielen beschäftigen wir uns mit folgendem Problem:
Das Semester ist zuende und die Professorin möchte wissen, wie gut die Durchschnittliche Leistung ihrer Masterstudent*innen im Übungsbetrieb war.
Mit Hilfe von Clojure Spec definieren wir einige Beispieldaten (wer mehr zu Spec erfahren möchte kann das [zum Beispiel in einem älteren Artikel in unserem Blog tun](http://funktionale-programmierung.de/2016/11/18/clojure-spec.html)):

{% highlight clojure %}
(ns my.namespace
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(s/def ::title string?)
(s/def ::student string?)
(s/def ::points (set (range 31)))
(s/def ::degree #{::msc ::bsc})
(s/def ::exercises (s/keys :req [::title ::student ::points ::degree]))

(sgen/sample (s/gen ::exercises))
;; =>
;; [{::name "Marco"
;;   ::title "Exercise 1"
;;   ::points 29
;;   ::degree ::msc}
;;  {::name "Mathias"
;;   ::title "Exercise 1"
;;   ::points 28
;;   ::degree ::bsc}
;;  ...]
{% endhighlight %}

Mit regulären Listenfunktionen könnten wir die Aufgabe so lösen:

{% highlight clojure %}
(defn sum-of-msc
  [exercises]
  (reduce + 0 (->> exercises
                    (filter #(= ::msc (::degree %)))
                    (map ::handins))))
{% endhighlight %}

Angewendet auf eine Liste von Übungen liefert uns das das richtige Ergebnis.
Es gibt allerdings ein Problem: jeder Aufruf von `filter`, `map` und `reduce` berechnet eine neue Liste!
Bei großen Mengen kann das, wie wir gleich sehen werden, schon mal zu Problemen führen.
Unten nun das gleiche, ausgedrück über unsere neu definierten Operatoren.
Die Funktion `reduce-with` funktioniert ähnlich wie `reduce`, nur, dass sie zusätzlich zur Reduktionsfunktion (hier `step`) einen Paramterer `xf` erwartet,
welcher eine Komposition unserer Funktionen darstellt.

{% highlight clojure %}
(defn reduce-with
  "Takes a function composed of process modifications xf,
    a step function, 
    an initial value and a sequence of operations.
    Applies xf with a step to every x in xs."
  [xf step init xs]
  (let [f (xf step)]
    (f (reduce f init xs))))

;; Compose two process modificators into one.
(def xform (comp
              (filtering #(= ::msc (::degree %)))
              (mapping ::points))

(defn sum-of-msc-2
  [exercises]
  (reduce-with xform + 0 exercises))
{% endhighlight %}

Ebenfalls das richtige Ergebnis liefernd offenbart diese Lösung einen großen Vorteil:

{% highlight clojure %}
(let [exercises (sgen/sample (s/gen ::exercises) 1000)]
  (= (time (sum-of-msc exercises))
      (time (sum-of-msc-2 exercises))))
;; =>
;; "Elapsed time: 407.378092 msecs"
;; "Elapsed time: 2.144632 msecs"
{% endhighlight %}

Der Grund liegt auf der Hand: Anstatt für jeden Schritt eine neue Liste zu berechnen werden die Prozesse nacheinander für jedes Element ausgeführt.
Damit sparen wir uns lange Zwischenergebnisse und gewinnen an Durchsatz.

Diese Konzept ist so praktisch, dass es mittlerweile unter dem Namen `Transducer` Teil der Clojure-Standartbibliothek ist.
Viele Funktionen sind bereits für den Gebrauch als Transducer eingestellt (darunter die altbekannten `map`, `filter`, `mapcat`, ...).
Unsere Funktion `reduce-with` ist dort als `transduce` bekannt.


## Mehr als Listen ##

Zum Abschluss noch das versprochene zweite Bespiel. 
Das Problem ist das gleiche, dieses Mal wollen wir allerdings keine Listen verwenden, sondern Channels (via `clojure.core.async`).
Hierbei schreiben wir alle Werte auf einen Channel `c` und beobachten, wie die selbe `xform` Prozessmodifikation auf ebendiese Channels anwendbar ist.
Ebenfalls verwenden wir hier nun die eingebaute Funktion `transduce`, die die Arbeit von `sum-of-msc-2` übernimmt.

{% highlight clojure %}
(ns my.namespace
  (:require [clojure.core.async :as async]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(let [exercises (sgen/sample (s/gen ::exercises))
      ;; Create a channel using the xform defined above.
      c (async/chan 1 xform)]
  ;; Put all our elements onto the channel.
  (async/go (async/onto-chan c exercises))
  (let [chan-res (loop [;; Read one element from the channel.
                        n (async/<!! c)
                        res 0]
                    (if-not n
                      res
                            ;; Read the next element from the channel.
                      (recur (async/<!! c)
                            ;; Accumulate the result.
                            (+ res n))))
        list-res (transduce xform + 0 exercises)]
    (= chan-res list-res)))
;; => true
{% endhighlight %}

Hier legen wir zuerst alle Elemente in den Channel (`onto-channel`).
Anschliessen lesen wir Schritt für Schritt so lange von diesem Channel, bis keine Elemente mehr darin vorhanden sind (`<!!`).
Wie wir am Ergebnis sehen, bewirkt unsere `xform` hier genau das gleiche, wie es schon bei Listen der Fall war.
Da unserer `xform` egal ist, auf welche Datenstruktur sie arbeitet (solange sie die oben genannte Infrastruktur zur Verfügung stellt)
lässt sie sich ohne eine Änderung ohne Probleme wiederverwenden!

## Fazit ##

Transducer sind die natürliche Fortsetzung viel verwendeter Listenfunktionen. 
In diesem Artikel haben wir uns mit der konsequenten Abstraktion auseinandergesetzt und haben auf diesem Weg das Konzept des `Transducer`s kennen gelernt.
Nicht nur erhalten wir durch Transducer eine mächtige Beschreibung von Datentransformationen, die rein auf Funktionskomposition beruht und vielseitig einsetzbar ist sondern
darüber hinaus noch entscheidente Performanceverbesserungen mit sich bringt.
Wer mehr zu diesem Thema erfahren möchte blablabla...

Referenzen...
Github repo mit code...
