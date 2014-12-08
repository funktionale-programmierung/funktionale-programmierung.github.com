---
layout: post
description: "Eingebaute Datenstrukturen in Clojure"
title: "Eingebaute Datenstrukturen in Clojure"
author: michael-sperber
tags: ["Clojure"]
---

Dieses Posting setzt unsere Clojure-Einführung
(hier [Teil 1]({% post_url 2014-11-27-clojure-first-steps %})) fort.  Clojure war ein
Pionier bei der Bereitstellung von effizienten rein funktionalen
Datenstrukturen (siehe 
[hier]({% post_url 2013-03-12-rein-funktional %}) für eine
Einführung).
Dieses Posting erklärt, was es damit auf
sich hat.

# Übrigens ...

Auf unserer Konferenz [BOB 2015](http://bobkonf.de/) gibt es ein
[Clojure-Tutorial](http://bobkonf.de/2015/sperber.html)!

<!-- more start -->

# Beispiele nachvollziehen

Um die Beispiele in diesem Posting nachzuvollziehen, reicht eine einfache Clojure-REPL.
Entweder können Sie - wie in Teil 1 beschrieben -
[Nightcode hochfahren]({% post_url 2014-11-27-clojure-first-steps %}) und die dortige REPL
benutzen, oder Sie begeben sich in einem Kommandozeilen-Fenster in das
Projektverzeichnis vom letzten Mal und tippen ein

    lein repl
	
... und schon erscheint ein Prompt `fp1.core==>` (oder so ähnlich), an
dem Sie Clojure-Ausdrücke eintippen können und sichten, was dabei
herauskommt.

Einen guten Referenz für die eingebauten Clojure-Funktionen - nach
Datentyp sortiert - gibt es zu Beispiel [bei
Grimoire](http://conj.io/).

# Primitive Datentypen

In diesem Abschnitt kümmern wir uns erst einmal um "primitive"
Datentypen, die nicht wesentlich strukturiert sind.  Booleans zum
Beispiel sind `true` und `false`:

{% highlight clojure %}
fp1.core=> true
true
fp1.core=> false
false
{% endhighlight %}

Booleans werden meist mit der `if`-Form weiterverarbeitet:

{% highlight clojure %}
fp1.core=> (if true 1 2)
1
fp1.core=> (if false 1 2)
2
{% endhighlight %}

Neben `false` gibt es in Clojure noch einen weiteren Wert, der als
boolesch "falsch" durchgeht, `nil`:

{% highlight clojure %}
fp1.core=> (if nil 1 2)
2
{% endhighlight %}

`nil` entspricht Javas `null` und ist mit der gleichen Vorsicht zu
behandeln: Pragmatisch verwenden wir `nil` als Platzhalter dafür, dass
etwas *nicht da* ist.

Außer `false` und `nil` zählen alle anderen Werte als "wahr":

{% highlight clojure %}
fp1.core=> (if 1 1 2)
1
fp1.core=> (if "foo" 1 2)
1
{% endhighlight %}

Die erste Überraschung zeigt, dass Clojure beliebig große ganze Zahlen
verarbeiten kann:

{% highlight clojure %}
fp1.core=> 98234723894735984357439543534
98234723894735984357439543534N
fp1.core=> (+ 2137812634 1214723657843564375384)
1214723657845702188018N
{% endhighlight %}

Zeichenketten sind allerdings wie in Java:

{% highlight clojure %}
fp1.core=> "foo"
"foo"
{% endhighlight %}

Clojure ist ein Lisp-Dialekt, entsprechend gibt es dort auch
*Symbole*.  Diese kommen aber - anders als in anderen Lisps -
fast ausschließlich bei der Programmierung von Makros zum Einsatz.
Diese sparen wir uns deshalb für ein zukünftiges Posting auf.

Für den täglichen Einsatz hat Clojure stattdessen *Keywords*, die wir
benutzen können, um Aufzählungen zum Beispiel für Status-Werte und
ähnliches zu repräsentieren.  Keyword-Literale sind durch den Doppelpunkt am
Anfang zu erkennen:

{% highlight clojure %}
fp1.core=> :ok
:ok
fp1.core=> :fail
:fail
fp1.core=> :delayed
:delayed
{% endhighlight %}

# Folgen

Für Folgen kennt Clojure gleich drei verschiedene Datentypen: Listen,
Vektoren und sogenannte *Seqs*.  Sie sind allesamt *funktionale
Datenstrukturen* - es wird also niemals an eine Folge "in situ" etwas
hinzugefügt, wie etwa bei den Java-Collections üblich.

Die drei Folgentypen unterscheiden sich - grob - folgendermaßen:

* *Listen* sind die klassischen einfach verketteten Listen, wie in Lisp
  und anderen funktionalen Sprachen.  Der primitive Konstruktor `cons`
  hängt an eine bestehende Liste *vorn* ein neues Element dran. Hinten
  an eine Liste etwas dranhängen ist teuer.
  
* *Vektoren* bilden einen allgemeiner verwendbaren Datentyp.
  Insbesondere ist es billig, an einen Vektor *hinten* etwas
  dranzuhängen.  (Vektoren sind durch hochverzweigte Bäume effizient
  repräsentiert.)

* *Seqs* sind nicht-strikte ("lazy") Datenstrukturen, die erst
  wirklich konstruiert werden, wenn Elemente aus der Folge abgerufen
  werden.

Die folgenden Abschnitte gehen etwas mehr ins Detail:

## Listen

Der Listentyp entspricht der klassichen Datendefinition:

Eine Liste ist eins der folgenden ...

* die *leere Liste* (Literal `'()`)
* eine *Paar* bestehend aus erstem Element und dem *Rest*, ebenfalls
  eine Liste (konstruiert mit [`cons`](http://conj.io/1.6.0/clojure.core/cons/))
  
{% highlight clojure %}
; einelementige Liste aus 1
fp1.core=> (cons 1 '())
(1)
; zweielementige Liste
fp1.core=> (cons 1 (cons 2 '()))
(1 2)
{% endhighlight %}

## Vektoren

Vektoren entstehen komplementär zu Listen durch Anhängen *hinten* mit
der Funktion [`conj`](http://conj.io/1.6.0/clojure.core/conj/).  Der
leere Vektor wird durch das Literal `[]` gebildet:

{% highlight clojure %}
fp1.core=> []
[]
fp1.core=> (conj [] 1)
[1]
fp1.core=> (conj (conj [] 1) 2)
[1 2]
{% endhighlight %}

Außerdem können wir Vektoren aus Elementen direkt mit den eckigen
Klammern bilden:

{% highlight clojure %}
fp1.core=> [1 2 3]
[1 2 3]
{% endhighlight %}

Vektoren unterstützen außerdem noch sehr schnelles Umdrehen mit
[`rseq`](http://conj.io/1.6.0/clojure.core/rseq/) und außerdem die
Extraktion von Teilfolgen mit
[`subvec`](http://conj.io/1.6.0/clojure.core/subvec/).

Wegen der praktischen `[]`-Notation und der effizienten
Repräsentation sind Vektoren in Clojure eher der Typ der Wahl für
Folgen, nicht Listen wie in anderen Lisps.

## Seqs

Schließlich sind da noch die *Seqs*, die "lazy" konstruiert werden.
Aus jedem Folgen-Datentyp wird ein Seq mit Hilfe der Funktion
[`seq`](http://conj.io/1.6.0/clojure.core/seq/):

{% highlight clojure %}
fp1.core=> (seq [1 2 3])
(1 2 3)
{% endhighlight %}

(Die runden Klammern sind also nicht - wie bei anderen Lisps - den
Listen vorbehalten.)

Seqs entstehen im Alltag am häufigsten durch die Verwendung von
eingebauten Folgenkombinatoren wie
[`map`](http://conj.io/1.6.0/clojure.core/map/):

{% highlight clojure %}
fp1.core=> (map (fn [x] (+ x 1)) [1 2 3])
(2 3 4)
{% endhighlight %}

`map` produziert also - obwohl die Eingabefolge ein Vektor ist - als
Ausgabe eine Seq, keinen Vektor.
Das ist praktisch, weil die Funktion, die bei `map` benutzt wurde,
erst aufgerufen wird, wenn das entsprechende Element aus dem Ergebnis
herausgezogen ist.  Das bedeutet aber auch, dass Seiteneffekte in
diesen Funktionen problematisch sind, weil sie möglicherweise erst
viel später nach der Anwendung von `map` zur Ausführung kommen, wie
das folgende Beispiel zeigt:

{% highlight clojure %}
fp1.core=> (def x (map println [1 2 3]))
#'fp1.core/x
fp1.core=> x
(1
2
3
nil nil nil)
{% endhighlight %}

Hier wird das `println` erst dann angewendet, wenn der Inhalt von `x`
ausgedruckt wird, nicht schon bei der Definition von `x`.    (Clojure
druckt erst die öffnende Klammer, holt dann die drei Elemente heraus -
aus Effizienzgründen sind es meist mehrere auf einmal - diese werden
dabei ausgedruckt, und das Ergebnis ist dann die Liste aus den drei
`nil`s, die `println` zurückgegeben hat.)

Seqs merken sich die produzierten Elemente, so dass eine wiederholte
Auswertung von `x` nicht dazu führt, dass alle Elemente erneut
berechnet werden.

Wer strikte Ausführung benötigt, ist oft mit spezialisierten
Operationen wie [`mapv`](http://conj.io/1.6.0/clojure.core/mapv/)
(`map` auf Vektoren) besser bedient.  Außerdem gibt es die Funktion
[`doall`](http://conj.io/1.6.0/clojure.core/doall/), die alle Elemente
einer Seq auswertet.

## Gemeinsame Operationen

Viele Operationen in Clojure funktionieren auf allen drei
Repräsentationen gleichermaßen.  Zum Beispiel extrahiert
[`first`](http://conj.io/1.6.0/clojure.core/first/) immer das erste
Element und [`rest`](http://conj.io/1.6.0/clojure.core/rest/) die
Restfolge nach dem ersten Element.

Ein paar dieser Funktionen sind allerdings auch etwas verwirrend:
[`conj`](http://conj.io/1.6.0/clojure.core/conj/) zum Beispiel fügt
ein Element einer beliebigen Folge hinzu, allerdings hängt es vom
Folgentyp ab, an welchem Ende:

{% highlight clojure %}
fp1.core=> (conj [1 2 3] 4)
[1 2 3 4]
fp1.core=> (conj (list 1 2 3) 4)
(4 1 2 3)
fp1.core=> (conj (seq [1 2 3]) 4)
(4 1 2 3)
{% endhighlight %}

# Mengen

Clojure hat auch einen Typ für Mengen eingebaut.  Mengenliterale sind
von `#{...}` um umschlossen:

{% highlight clojure %}
fp1.core=> #{1 2 3}
#{1 3 2}
fp1.core=> #{:red :blue :green}
#{:green :red :blue}
{% endhighlight %}

Aus anderen Folgen können wir Mengen mit der Funktion
[`set`](http://conj.io/1.6.0/clojure.core/set/) konstruieren:

{% highlight clojure %}
fp1.core=> (set [1 2 3])
#{1 3 2}
{% endhighlight %}

Elemente können zu Mengen mit
[`conj`](http://conj.io/1.6.0/clojure.core/conj/) hinzugefügt und mit
[`disj`](http://conj.io/1.6.0/clojure.core/disj/) entfernt werden:

{% highlight clojure %}
fp1.core=> (conj #{1 2 3} 4)
#{1 4 3 2}
fp1.core=> (disj #{1 2 3} 2)
#{1 3}
{% endhighlight %}

Um festzustellen, ob ein Wert Element einer Menge ist, können wir
das Mengenobjekt wie eine Funktion behandeltn:

{% highlight clojure %}
fp1.core=> (def s #{1 2 3})
#'fp1.core/s
fp1.core=> (s 1)
1
fp1.core=> (s 4)
nil
{% endhighlight %}

Etwas lesbarer ist es aber vielleicht, die Funktion
[`contains?`](http://conj.io/1.6.0/clojure.core/contains_QMARK/) zu
verwenden:

{% highlight clojure %}
fp1.core=> (contains? s 1)
true
fp1.core=> (contains? s 4)
false
{% endhighlight %}

# Maps

Schließlich gibt es in Clojure noch Maps, also Tabellen aus Schlüsseln
und Werten.  Literale werden mit `{...}` umschlossen, innen wechseln
sich Schlüssel und Werte ab.  (Manche Clojure-Programmierer schreiben
noch ein Komma zwischen Schlüssel-/Wert-Paare.)

{% highlight clojure %}
fp1.core=> {:foo 1 :bar 2}
{:bar 2, :foo 1}
{% endhighlight %}

Maps können mit [`assoc`](http://conj.io/1.6.0/clojure.core/assoc/)
erweitert und mit
[`dissoc`](http://conj.io/1.6.0/clojure.core/dissoc/) verkleinert
werden:

{% highlight clojure %}
fp1.core=> (def m {:foo 1 :bar 2})
#'fp1.core/m
fp1.core=> (assoc m :baz 3)
{:baz 3, :bar 2, :foo 1}
fp1.core=> (assoc m :foo 3)
{:bar 2, :foo 3}
fp1.core=> (dissoc m :bar)
{:foo 1}
{% endhighlight %}

Um an den Wert zu einem Schlüssel zu kommen, können Maps - wie Mengen
- als
Funktion verwendet werden:

{% highlight clojure %}
fp1.core=> (m :foo)
1
fp1.core=> (m :baz)
nil
{% endhighlight %}

Verwirrenderweise können Schlüsselwörter auch als Zugriffsfunktion auf
Maps dienen:

{% highlight clojure %}
fp1.core=> (:foo m)
1
fp1.core=> (:baz m)
nil
{% endhighlight %}

Auch hier ist es unter Umständen lesbarer, auf die explizite
[`get`](http://conj.io/1.6.0/clojure.core/get/)-Funktion zurückzugreifen:

{% highlight clojure %}
fp1.core=> (get m :foo)
1
fp1.core=> (get m :baz)
nil
{% endhighlight %}

# So weit, so gut ...

Das war also der Turbo-Streifzug durch die eingebauten
Clojure-Datenstrukturen.  Ich hoffe, er war erhellend.  Den nächsten
Teil der Clojure-Einführung gibt es bald auf diesem Blog.

<!-- more end -->

