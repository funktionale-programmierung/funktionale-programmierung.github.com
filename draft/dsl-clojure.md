---
layout: post
description: DSL-Entwicklung mit Clojure
title: "Systematisch eingebettete DSLs entwickeln in Clojure"
author: michael-sperber
tags: ["Clojure", "DSL", "EDSL"]
---

In letzter Zeit sind in der Software-Entwicklung [domänenspezifische
Sprachen](http://de.wikipedia.org/wiki/Dom%C3%A4nenspezifische_Sprache)
- kurz als *DSL* für "domain-specific language" bezeichnet -
populär geworden: Das Versprechen einer DSL ist es, für ein bestimmtes
Anwendungsgebiete zu ermöglichen, besonders kompakte und verständliche
Programme zu schreiben.  Diese Programme können außerdem auf
technische Details verzichten, die nichts direkt mit dem
Anwendungsgebiet zu tun haben.

Das Entwickeln einer konventionellen DSL ist aufwendig, da zur
Implementierung DSL alles dazugehört, was zu einer "normalen"
Programmiersprachenimplementierung auch fällig ist: Lexer, Parser,
Compiler oder Interpreter, IDE-Support.  Aus diesem Grund sind in
letzer Zeit substantielle Frameworks für die schnelle Entwicklung von
DSLs entstanden, z.B. [Spoofax](http://strategoxt.org/Spoofax), das
[Eclipse Modeling Project](http://www.eclipse.org/resources/resource.php?id=493) oder
[Xtext](http://www.eclipse.org/Xtext/).

In funktionalen Sprachen gibt es allerdings häufig einen einfacheren
Weg: die Entwicklung einer sogenannten *eingebetteten* DSL (kurz
[EDSL](http://c2.com/cgi/wiki?EmbeddedDomainSpecificLanguage) für
"embedded DSL").  Dabei werden die Abstraktionen der sogenannten
*Host-Sprache* so eingesetzt, daß es so *aussieht*, als ob innerhalb
der Host-Sprache eine DSL entsteht.  Dies hat eine Reihe von
Vorteilen:

<!-- more start -->

- Die Entwicklung der DSL ist deutlich einfacher: Entwickler müssen
  kein komplettes Compiler-Frontend implementieren, und können schon
  die bestehenden Sprachmittel der Host-Sprache für die
  Implementierung und ggf. auch in der DSL verwenden.

- Entwickler haben schon einen syntaktischen und semantischen Rahmen
  und müssen nicht das komplette Design der Sprache stemmen: Nicht nur
  ist das mühsam, die Ergebnisse sind auch oft
  [suboptimal](http://c2.com/cgi/wiki?GreenspunsTenthRuleOfProgramming).

- Benutzer können die EDSL zusammen mit der Host-Sprache verwenden.

<!-- more end -->

Der EDSL-Ansatz ist prinzipiell in vielen Sprachen zugänglich.  In
funktionalen Sprachen funktioniert er oft besonders gut, weil dort
typische Sprachmittel wie Higher-Order-Funktionen, Makros,
nicht-strikte Auswertung und Typklassen bei vielen EDSLs ganz
natürliche Einsatzgebiete finden.

In diesem Artikel demonstrieren wir den EDSL-Einsatz am Beispiel einer
kleinen in [Clojure](http://clojure.org/) eingebetteten Sprache für
"Stream-Prozessoren".  Clojure punktet innerhalb der funktionalen
Sprachen in ihrer Eigenschaft als Lisp-Variante bei der
EDSL-Implementierung besonders, da die Sprache es durch das
leistungsfähige Makro-System erlaubt, bei EDSL-Design und
Implementierung *systematisch* vorzugehen.

Ein Stream-Prozessor ist ein kleines Programm, das einen Strom von
Objekten als Eingabe einliest und einen Strom von Objekten als Ausgabe
produziert.  (Beide Ströme können potentiell unendlich lang gehen.)
Die Einlese-Operation heißt `get`, die Ausgabe-Operation `put`.  Der
Clou ist, daß zwei Stream-Prozessoren wie Gartenschläuche aneinander
angeschlossen oder *komponiert* werden können: Des ersten Prozessors
Ausgabe ist des nächsten Prozessors Eingabe.

<div id="center">
<img src="/files/dsl-clojure/stream-processors.png>
</img></div>

Ich würde gern einen Stream-Prozessor etwa so beschreiben:

{% highlight clojure %}
(get x)
(get y)
(put (* x y))
{% endhighlight %}

(Wer Clojure nicht kennt - das ist wie
[Racket](/tags-archive.html#Racket) eine Sprache, in der die Syntax
mit Klammern gebildet wird.)

Das sollte sinngemäß heißen: lies eine Zahl ein und nenne sie `x`,
lies eine weitere Zahl ein und nenne sie `y`
und gib schließlich das Produkt von `x` und `y` aus.

In Clojure (und jeder anderen Sprache) wäre es jetzt am einfachen,
wenn wir Funktionsdefinitionen für `get` und `put` schreiben könnten,
die etwa so aussehen:

{% highlight clojure %}
(defn put [x] ...)

(defn get [var] ...)
{% endhighlight %}

Ganz so einfach ist es leider nicht aus drei Gründen:

1. Die `get`-Form soll *eine Variable binden*, die nach der `get`-Form
   verwendbar ist.  Das geht nicht mit einem Funktionsaufruf.
  
2. Die einfache Hintereinanderausführung macht es schwer, zwei
   Prozessoren zu komponieren: Schließlich soll bei der Komposition
   zweier Prozessoren `sp1` und `sp2` jeweils `sp1` bis zum nächsten
   `put` laufen und dann sollte `sp2` die Gelegenheit haben, bis zum
   nächsten `get` zu laufen und den Wert da abzuholen.  Wir müssen also
   irgendwie ermöglichen, die Ausführung eines Prozessors nach einem
   `get` oder `put` zu unterbrechen.
   
3. Um zwei Prozessoren komponieren zu können, sollten wir sie als
   Objekte behandeln.  Oben steht aber nur eine einfache
   Anweisungsfolge.
  
Der erste Schritt zur Lösung des Problems ist Punkt 3: aus `get` und
`put` Objekte zu machen.  Dazu legen wir in Clojure
Record-Definitionen an:

{% highlight clojure %}
(defrecord Put [...])
(defrecord Get [...])
{% endhighlight %}

Bei den `...` müssen wir noch Felder eintragen.  Bei `Put` brauchen
wir auf jeden Fall den auszugebenenden Wert:

{% highlight clojure %}
(defrecord Put [value ...])
{% endhighlight %}

Nun können wir uns Punkt 2 zuwenden: Um die Ausführung eines Programms
zu unterbrechen (um später an der gleichen Stelle weiterzumachen),
bietet es sich in funktionalen Programmen an, eine Funktion zu
verwenden: Die wird aufgerufen, wenn es weitergeht.  Die brauchen wir
sowohl in `Get` als auch in `Put` und legen sie dort als Feld `next` an:

{% highlight clojure %}
(defrecord Put [value next])
(defrecord Get [next])
{% endhighlight %}

Die Funktion in `next` hat keine Parameter, und liefert einfach nur
den Prozessor, mit es weitergeht.  Wenn also `sp` ein Stream-Prozessor
ist, können wir mit `((:next sp))` den nächsten Stream-Prozessor
bekommen.

Es bleibt noch Punkt 1: Wir wollen bei `get` noch einen Bezeichner
binden.  Für's Binden sind aber in funktionalen Sprachen ebenfalls die
Funktionen zuständig.  Wir können einfach die Funktion, die eh schon im
`Get`-Record steckt mit einem Parameter versehen: die Funktion müssen
wir dann mit dem gelesenen Wert als Argument aufrufen.  Um die
parameterlose Funktion im `Put`-Record von der Funktion in `Get`
abzugrenzen, benennen wir sie in `consume` um:

{% highlight clojure %}
(defrecord Put [value next])
(defrecord Get [consume])
{% endhighlight %}

Durch diese Darstellung entsteht ein kleines Problem: Es gibt keine
Möglichkeit, anzuhalten: Jedes `Put` bzw. `Get` braucht eine Funktion,
um weiterzumachen.  Wir legen darum einen Singleton-Record-Typ für's
Anhalten an:

{% highlight clojure %}
(defrecord Stop [])
(def stop (Stop.))
{% endhighlight %}

Hier ist `Stop.` der Name des Konstruktors des Record-Typs `Stop`.

Jetzt können wir die ersten Beispiele aufschreiben:

{% highlight clojure %}
(def sp1 (Put. 5 (fn [] (Put. 3 (fn [] stop)))))
(def sp2 (Get. (fn [x] (Get. (fn [y] (Put. (+ x y) (fn [] stop)))))))
{% endhighlight %}

Auch hier sind `Put.` und `Get.` Konstruktor-Aufrufe.  Die `fn`-Formen
machen Funktionen - ähnlich wie `lambda` in anderen Lisp-Dialekten.

Der Prozessor `sp2` entspricht dabei dem Beispiel von oben.

Wir können auch einen Prozesso definieren, der einen unendlichen Strom
generiert:

{% highlight clojure %}
(defn sp-from [n] (Put. n (fn [] (sp-from (+ n 1)))))
(def nats (sp-from 0))
{% endhighlight %}

Der Prozessor `nats` generiert die natürlichen Zahlen.

Hier ist eine Funktion für etwas komplexere Prozessoren, sogenannte
*Filter*:

{% highlight clojure %}
(defn sp-filter
  [p]
  (Get. 
   (fn [x]
     (if (p x)
       (Put. x
             (fn []
               (sp-filter p)))
       (sp-filter p)))))
{% endhighlight %}

Die Funktion `sp-filter` reicht die Eingaben an die Ausgabe durch, die
ein bestimmtes Kriterium erfüllen: Sie akzeptiert ein Prädikat `p` (also eine
Funktion, die einen Wert akzeptiert und `true` oder `fals` liefert)
und konstruiert den dazugehörigen Prozessor: Das `Get.` liest einhen
Wert ein, das `fn` nennt ihn `x`, das `if` prüft das Kriterium - bei
`true` gibt das `Put.` den gelesenen Wert aus, ansonsten geht es ohne
den Wert rekursiv weiter.

(Erfahrene Leser erkennen jetzt, daß die Stream-Prozessoren in
[Continuation-Passing-Style](http://matt.might.net/articles/by-example-continuation-passing-style/)
geschrieben sind.)

Wir können jetzt also so ziemlich beliebige Stream-Prozessoren
herstellen.  Leider ...

1. ist die Notation ziemlich umständlich und weit weg, von dem, was
   wir uns ursprünglich vorgestellt hatten,
   
2. haben wir noch keine Funktion, um die Ausgabe eines Prozessors
   aufzusammeln
   
3. und auch noch keine Funktion, um zwei Prozessoren zu komponieren.

Fangen wir mit Punkt 2 an, das geht recht einfach:

{% highlight clojure %}
(defn run
  [sp]
  (cond
   (instance? Put sp) (lazy-seq (cons (:value sp) (run ((:next sp)))))
   (instance? Get sp) '()
   (instance? Stop sp) '()))
{% endhighlight %}

Die `run`-Funktion akzeptiert einen Stream-Prozessor und unterscheidet
dann nach den drei Record-Typen (`(instance? Put sp)` testet z.B., ob
`sp` ein `Put`-Record ist): Bei jedem `Put` steckt `run` den
ausgegebenen Wert in eine *lazy sequence*, also Folge, die auch
unendlich sein kann: `(:value sp)` extrahiert den ausgegebenen Wert,
und `(run ((:next sp)))` macht weiter.  `cons` konstruiert die Folge.
Damit können wir schon zwei Beispiele aufrufen:

{% highlight clojure %}
> (run sp1)
(5 3
> (take 5 (run nats))
(0 1 2 3 4)
{% endhighlight %}

Die `take`-Funktion extrahiert in diesem Fall die ersten 5 Elemente
der Folge.

Aber es bleiben noch Probleme 1 und 3: die umständliche Syntax und
die Komposition.  Machen wir uns erst einmal an die Komposition: Wir
hätten gern eine Funktion, die zwei Prozessoren akzeptiert und wieder
einen liefert:

{% highlight clojure %}
(defn >>>
  [sp1 sp2]
   ...
{% endhighlight %}

Nun gibt es für `sp1` als auch für `sp2` jeweils die drei
Möglichkeiten `Put`, `Get` und `Stop`,
insgesamt also potentiell neun.  Die Repräsentation, die wir gewählt
haben, erlaubt uns jetzt einen tollen Trick: wenn wir nur die `put`s
und `get`s paarweise nebeneinanderstellen, dann heben die sich quasi jeweils
gegenseitig auf:

{% highlight clojure %}
(defn >>>
  [sp1 sp2]
  (cond
   ;; put >>> get
   (and (instance? Put sp1) (instance? Get sp2))
   (>>> ((:next sp1)) ((:consume sp2) (:value sp1)))
{% endhighlight %}

Wir müssen noch die anderen Fälle abdecken: In allen Fällen, wo `sp2`
ein `Put` ist, können wir den angegebenen Wert direkt ausgeben:

{% highlight clojure %}
   ;; sp >>> put
   (instance? Put sp2)
   (Put. (:value sp2) (fn [] (>>> sp1 ((:next sp2)))))
{% endhighlight %}

Bei zwei `Get`s hintereinander ziehen wir das `>>>` nach innen:

{% highlight clojure %}
   ;; get >>> get
   (and (instance? Get sp1) (instance? Get sp1))
   (Get. (fn [i] (>>> ((:consume sp1) i) sp2)))
{% endhighlight %}

Damit haben wir alle Kombinationen von `Put` und `Get` abgedeckt.  Es
bleibt noch `Stop`.  `Stop` vorn heißt, daß der erste Prozessor fertig
ist - wir machen mit dem zweiten weiter:
   
{% highlight clojure %}
   ;; stop >>> sp2
   (instance? Stop sp1) sp2
{% endhighlight %}

Wenn der zweite Prozessor fertig ist, spielt es hingegen keine Rolle,
was der erste macht:

{% highlight clojure %}
   (instance? Stop sp2) stop))
{% endhighlight %}

Damit ist auch `>>>` fertig.  Wir können jetzt weitere Beispiele
ausprobieren:

{% highlight clojure %}
> (run (>>> sp1 sp2))
(8)
> (take 5 (run (>>> nats (sp-filter even?))))
(0 2 4 6 8)
{% endhighlight %}

Es bleibt also nur noch die umständliche Syntax. Hier kommt jetzt ein
*Makro* ins Spiel.  Dieser Makro definiert die eigentliche DSL.  Dazu
müssen wir der EDSL einen Namen geben - in diesem Fall bietet sich
`stream-processor` an.  Damit könnte die obigen Beispiel-Prozessoren
so geschrieben werden:

{% highlight clojure %}
(def sp1 (stream-processor (put 5) (put 3)))
(def sp2 (stream-processor (get x) (get y) (put x y)))
{% endhighlight %}

Die Definition des Makros fängt so an:

{% highlight clojure %}
(defmacro stream-processor
  [& ?clauses]
  (if (seq? ?clauses)
    (let [?clause (first ?clauses)
          ?rest (rest ?clauses)]
      ...
    `stop))
{% endhighlight %}

Der Makro ist nichts anderes als eine Funktion, die vom Compiler auf
alle Formen angewendet wird, die mit `stream-processor` anfangen.  An
den Beispielen sieht man, daß die Form eine beliebige Anzahl von
Operanden haben kann: einen für jede "Klausel".  Die Makro-Definition
steckt diese mit Hilfe der Parameterdeklaration `& ?clauses` in eine
Liste und nennt diese `?clauses`.  Der Rumpf der Makro-Definition
schaut dann nach, ob die Liste nichtleer ist - dann werden die erste
Klausel und der Rest extrahiert - und wenn nicht, wird `stop`
produziert.  (Der "accent grave" vor dem `stop`, der sogenannte
*Backquote*, ist für die Generierung des Ausgabe-Codes zuständig.)

Jede Klausel fängt jetzt entweder mit `get` oder `put` an, was wir mit
`case` unterscheiden:

{% highlight clojure %}
(defmacro stream-processor
  [& ?clauses]
  (if (seq? ?clauses)
    (let [?clause (first ?clauses)
          ?rest (rest ?clauses)]
      (case (first ?clause)
        get 
        (let [?var (second ?clause)]
          `(Get. (fn [~?var]
                   (stream-processor ~@?rest))))
        
        put
        (let [?val (second ?clause)]
          `(Put. ~?val (fn [] (stream-processor ~@?rest))))))
    `stop))
{% endhighlight %}

Innerhalb der Backquotes fügt `~?var` z.B. die Variable
ein bzw. `~@?rest` die restlichen Klauseln.  Das Muster der obigen
Beispiele ist offenbar erkennbar.

Mit dieser Definition funktionieren schon einmal die Beispiele `sp1`
und `sp2`.  Was ist aber mit `sp-from`?  Das enthält ja noch einen
rekursiven Aufruf, müßte also so aussehen:

{% highlight clojure %}
(defn sp-from [n] (stream-processor (put n) (sp-from (+ n 1))))
{% endhighlight %}

Der `stream-processor`-Makro kennt aber bisher nur `put`- und
`get`-Klauseln.  Wir müssen also noch einen Default-Fall einfügen:

{% highlight clojure %}
(defmacro stream-processor
  [& ?clauses]
  (if (seq? ?clauses)
    (let [?clause (first ?clauses)
          ?rest (rest ?clauses)]
      (case (first ?clause)
        get 
        (let [?var (second ?clause)]
          `(Get. (fn [~?var]
                   (stream-processor ~@?rest))))
        
        put
        (let [?val (second ?clause)]
          `(Put. ~?val (fn [] (stream-processor ~@?rest))))
        
        
        ?clause))
    `stop))
{% endhighlight %}

Es bleibt noch das `sp-filter`-Beispiel, das eine Fallunterscheidung
enthält.  Das könnte so aussehen:

{% highlight clojure %}
(defn sp-filter
  [p]
  (stream-processor
   (get x)
   (when (p x)
     (put x)
     (sp-filter p))
   (sp-filter p)))
{% endhighlight %}

Dazu brauchen wir noch einen weiteren Fall im Makro für `when`, der
das benötigte `if` generiert:

{% highlight clojure %}
(defmacro stream-processor
  [& ?clauses]
  (if (seq? ?clauses)
    (let [?clause (first ?clauses)
          ?rest (rest ?clauses)]
      (case (first ?clause)
        get 
        (let [?var (second ?clause)]
          `(Get. (fn [~?var]
                   (stream-processor ~@?rest))))
        
        put
        (let [?val (second ?clause)]
          `(Put. ~?val (fn [] (stream-processor ~@?rest))))
        
        when
        (let [?test (second ?clause)
              ?consequent (rest (rest ?clause))]
          `(if ~?test
             (stream-processor ~@?consequent)
             (stream-processor ~@?rest)))
        
        ?clause))
    `stop))
{% endhighlight %}

Fertig!

Um noch einmal zusammenzufassen:

- DSLs sind oft nützlich.

- EDSLs sind einfacher zu implementieren als "Stand-Alone-DSLs".

- Funktionale Sprachen unterstützen die Implementierung von EDSL in
  besonderer Weise.
  
- Die Kombination von First-Class-Funktionen und Makros in Clojure und
  anderen Lisp-Varianten unterstützt die systematische Entwicklung von
  EDSLs.

Ein Hinweis noch: Die Idee der Stream-Prozessoren habe ich aus dem sehr
lesenswerten [Paper](http://www.haskell.org/arrows/biblio.html#Hug00)
von John Hughes über [Arrows](http://www.haskell.org/arrows/).
