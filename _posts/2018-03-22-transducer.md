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
insbesondere einen Blick auf die sogennanten Transducer in der Programmiersprache Clojure.
<!-- more start -->
Ausserdem werden wir sehen, wie wir mit Hilfe von sinnvoller Abstraktion nicht
nur sehr gute Wiederverwendbarkeit sondern auch eine höhere Performance erreichen
können.

<!-- more start -->

## Alles ist ein fold ##

Wer sich in der Vergangenheit mit der funktionalen Programmierung auseinandergesetzt hat ist eventuell schon mit dieser Aussage vertraut.
Falls Sie dazu bislang keine Gelegenheit hatten (oder Ihr Gedächtnis auffrischen wollen) betrachten wir zuerst eine Variante der üblichen Definitionen von `map` und `filter`.
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

So oder so ähnlich finden sich viele Defintionen und verschiedenen Standardbibliotheken. 
Wie die Überschrift dieses Abschnitts schon verrät lassen sich beide Funktionen auch über `fold` (oder eben `reduce`) definieren.
Die Signatur von `reduce` sieht, in Haskellnotation ausgedrückt, so aus `(b -> a -> b) -> b -> [a] -> b` (in diesem Fall spezialisiert für Listen).

{% highlight clojure %}
(defn mapping
  "Takes a function f and returns a function.
    The returned function takes a collection acc 
    and an element x and appends x applied to f to the end of acc."
  [f]
  (fn [acc x] (conj acc (f x))))

(defn map
  "Takes a function f and applies it to every element of xs."
  [f xs]
  (reduce (mapping f) [] xs))

(defn filtering
  "Takes a predicate pred and returns a function.
    The returned function takes a collection acc
    and an element x and appends x to the end of acc if it satisfies pred."
  [pred]
  (fn [acc x] (if (pred x) (conj acc x) acc)))

(defn filter
  "Takes a predicate pred and returns a list with all 
    elements of xs that satisfy pred."
  [pred xs]
  (reduce (filtering pred) [] xs))
{% endhighlight %}

Würden unsere Programme ausschließlich aus Listenverarbeitung bestehen könnten wir an dieser Stelle zufrieden aufhören.
Etwas sticht jedoch ins Auge: sieht man einmal von dem Aufruf an `conj` ab verraten uns die Definitionen von `mapping` und `filtering`
nichts darüber, dass sie "nur" auf Kollektionen arbeiten!

## Von Kollektionen zu Prozessen ##

Wir haben festgestellt, dass die Verbindung zu Kollektionen von unseren `mapping` und `filtering` Funktionen nur über das `conj` besteht.
Nun gehen wir einen Schritt weiter und sehen uns an was passiert, wenn wir über `conj` abstrahieren. 
Dafür ist es hilfreich nicht an Listen sondern Sequenzen von Schritten zu denken.
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

Wir kümmern uns an dieser Stelle nur um Punkt 1. Wir werden zuerst eine Implementierung von `mapping` und `filtering` vorschlagen.
Im Codebeispiel unten machen wir gebrauch von Clojures Syntax für "arity overloading". 
Das heisst, wir können in einer Funktion mehrere Implementierungen für verschiedene Anzahlen von Argumenten anbieten (mehr dazu auf [Clojure - Functional Programming](https://clojure.org/about/functional_programming#_first_class_functions)).
Clojure wird hierbei abhängig von der Anzahl der übergebenen Argumente die entsprechende Implementierung wählen.

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

Auf den ersten Blick sieht das vielleicht etwas unintuitiv aus, ist aber am Beispiel von `mapping` schnell erklärt. Die Nummerierung entspricht hier der Zahlen im obenstehenden Codebeispiel.

1. Dieser Fall deckt den "Anfang" eines Prozesses ab. Es wurde noch nichts berechnet und es liegt noch keine "nächste" Berechnung vor. 
   In diesem Fall wollen wir von unserer `step` Funktion ein "neutrales" Element, mit dem wir die Bechnung lostreten können.
2. Hier signalisieren wir das "Ende" eins Prozesses. Es kommmt kein Element mehr nach, also machen wir den letzten Schritt mit dem schon vorliegenden "Ergebnis".
3. Schliesslich existiert noch unser Fall, der das aktuelle Ergebnis und ein Element miteinander verarbeitet.

Diese Funktionen sind nun so weit parametrisiert, dass wir beliebige Prozesse damit ausdrücken und, vermutlich noch wichtiger, mehrere solcher Prozessmodifikationen
hintereinander ausführen (oder komponieren) können!

Wie sieht das in der Realität aus?


## Beispiele für Prozessmodifikatoren ##

Im Folgenden geben wir zwei Beispiele dafür, wie uns das Ganze nun in der echten Welt hilft und wann wir dieses Vorgehen möglicherweise der regulären Verkettung von Listenoperationen vorziehen wollen.

In beiden Beispielen beschäftigen wir uns mit folgendem Problem:
Das Semester ist zuende und die Professorin möchte wissen, wie gut die Durchschnittliche Leistung ihrer Masterstudent*innen im Übungsbetrieb war.
Mit Hilfe von Clojure-Spec definieren wir einige Beispieldaten. 
Die ersten vier Zeilen definieren die "Form" unserer Daten. 
Dabei ist z.B. `::title` ein Wert, der das `string?`-Prädikat erfüllt, in der
fünften Zeile geben wir `::exercise` als als Map mit den Schlüsseln an, die
darüber definiert wurden.
Die Ausgabe von `sample` wurde hier etwas aufgehübscht, das tatsächliche Ergbnis
sähe wohl etwas offentichtlich zufälliger aus.
Wer mehr zu Spec erfahren möchte kann das 
[in einem älteren Blog-Artikel tun](http://funktionale-programmierung.de/2016/11/18/clojure-spec.html).

{% highlight clojure %}
(ns my.namespace
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(s/def ::title string?)
(s/def ::student string?)
(s/def ::points (set (range 31)))
(s/def ::degree #{::msc ::bsc})
(s/def ::exercise (s/keys :req [::title ::student ::points ::degree]))


;; `sample` takes a generator for our `spec` and returns some random data that 
;; matches this spec.
(sgen/sample (s/gen ::exercise))
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

Zurück zu unserer Aufgabe. Diese liesse sich mit regulären Listenfunktionen zum
Beispiel so lösen:

{% highlight clojure %}
(defn sum-of-msc
  [exercises]
  (reduce + 0 (->> exercises
                    (filter #(= ::msc (::degree %)))
                    (map ::handins))))
{% endhighlight %}

Angewendet auf eine Liste von Übungen liefert es uns das richtige Ergebnis.
Es gibt allerdings ein Problem: Jeder Aufruf von `filter`, `map` und `reduce` berechnet eine neue Liste!
Bei großen Mengen kann das, wie wir gleich sehen werden, durchaus zu Problemen führen.

Als nächstes implementieren wir die gleiche Funktionalität, ausgedrückt über unsere neu definierten Operatoren.

Der erste Schritt wird eine leicht modifizierte Version von `reduce` names
`reduce-with` sein. Diese funktioniert ganz ähnlich wie `reduce`, ausser, dass sie 
zusätzlich zur Reduktionsfunktion (hier `step`) einen Paramterer `xf` erwartet,
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
{% endhighlight %}

Im Inneren der Funktion rufen wir wie gewohnt `reduce` auf.
Als Reduktionsfunktion kommt allerdings die Kompositionsfunktion `xf` zum Einsatz,
welche über den `step` Parameter (selbst eine Funktion) für die Auswertung spezialisiert und 
and `f` gebunden wird.
Anschliessend wird reduziert und das Ergebnis zum Schluss noch einmal mit `f`
aufgerufen, um ein finales Ergebnis zu erzeugen.

Wie muss allerdings das `xf` aussehen? 
Diese Funktion ist, ganz analog zu `sum-of-msc` oben, ebenfalls eine Komposition
von `filtering` und `mapping`. Wir nennen sie an dieser Stelle `xform` (eine
Konvention in der Clojurewelt).

{% highlight clojure %}
;; Compose two process modificators into one.
(def xform (comp
              (filtering #(= ::msc (::degree %)))
              (mapping ::points)))
{% endhighlight %}

Eventuell fragen Sie sich zurecht, warum sich die Reihenfolge der Aufrufe von `filtering`
und `mapping` nicht verändert, da Komposition "von rechts nach links" ausgewertet
wird, das Threading in unserer `sum-of-msc` aber von "links nach rechts" passiert.
An dieser Stelle würde die vollständige Antwort den Rahmen sprengen. Wir merken
uns für den Moment einfach, dass die Komposition dieser Art von Funktion ebenfalls
"von rechts nach links" auswertet.
Sie können das sehr leicht herausfinden, indem sie die Kopmposition der Funktionen
einmal von Hand vollständig reduzieren.

{% highlight clojure %}
(defn sum-of-msc-2
  [exercises]
  (reduce-with xform + 0 exercises))
{% endhighlight %}

Diese Lösung liefert ebenfalls das richtige Ergebnis und offenbart einen großen Vorteil:

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

Diese Konzept ist so praktisch, dass es mittlerweile unter dem Namen `Transducer` Teil der Clojure-Standardbibliothek ist.
Viele Funktionen sind bereits für den Gebrauch als Transducer eingestellt (darunter die altbekannten `map`, `filter`, `mapcat`, ...).
Unsere Funktion `reduce-with` ist dort als `transduce` definiert.


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
Nicht nur erhalten wir durch Transducer eine mächtige Beschreibung von Datentransformationen, 
die rein auf Funktionskomposition beruht und vielseitig einsetzbar ist, sondern
darüber hinaus noch entscheidende Performanceverbesserungen mit sich bringt.
<!-- more end -->
