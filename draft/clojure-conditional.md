---
layout: post
description: "Verzweigungen in Clojure"
title: "Verzweigungen in Clojure"
author: michael-sperber
tags: ["Clojure"]
---

Dieses Posting setzt unsere Clojure-Einführung (hier [Teil 1]({% post_url 2014-11-27-clojure-first-steps %}),
[Teil 2]({% post_url 2014-12-08-clojure-datenstrukturen %})) fort.  Dieses Mal geht es um
Fallunterscheidungen.  (Zur Erinnerung: In [Teil 1]({% post_url 2014-11-27-clojure-first-steps %}) steht, wie Sie eine
einfache IDE installieren und betreiben können.)

<!-- more start -->

Nehmen wir, an, wir wollten für Verkehrssünder aus dem
Flensburg-Punktestand berechnen, welche
[Maßnahmen](https://www.bussgeldkatalog.org/punkte-flensburg/) daraus
folgen.  Dazu schreiben wir das Gerüst einer Funktion:

{% highlight clojure %}
(defn flensburg-massnahme
    "Maßnahme für eine gegebene Flensburg-Punktezahl berechnen." 
    [p]
    ...)
{% endhighlight %}

Hier wird gleich ein weiteres Clojure-Feature deutlich, nämlich die
Möglichkeit, einen *Docstring* für die Funktion mit einer kurzen
Beschreibung bereitzustellen.  Nun müssen wir irgendwie zwischen den
vier verschiedenen Stufen des "Punkte-Tachos" unterscheiden, der so
aussieht:

<table>
<tr><td>1 bis 3 Punkte</td><td>Vormerkung</td></tr>
<tr><td>4 bis 5 Punkte</td><td>Ermahnung</td></tr>
<tr><td>6 bis 7 Punkt</td><td>Verwarnung</td></tr>
<tr><td>8 Punkte</td><td>Entziehung der Fahrerlaubnis</td></tr>
</table>

Die Tests dafür sehen so aus:

{% highlight clojure %}
(<= 1 p 3)
(<= 4 p 5)
(<= 6 p 7)
(>= p 8)
{% endhighlight %}

Nun müssen wir diese Tests benutzen, um eine *Verzweigung*
vorzunehmen, also abhängig davon, welcher Test zutrifft, die
entsprechende Maßnahme zuordnen.  In Clojure geht das mit
[`cond`](http://conj.io/store/v0/org.clojure/clojure/1.6.0/clj/clojure.core/cond/)
(für "Conditional") und sieht so aus:

{% highlight clojure %}
(defn flensburg-massnahme
    "Maßnahme für eine gegebene Flensburg-Punktezahl berechnen." 
    [p]
    (cond
       (<= 1 p 3) ...
       (<= 4 p 5) ...
       (<= 6 p 7) ...
       (>= p 8) ...))
{% endhighlight %}

In einer `cond`-Form befinden sich abwechselnd *Tests* und Ausdrücke.
(Die Kombination von Test und Ausdruck heißt *Zweig*.)
Bei der Auswertung des `cond` werden nacheinander alle Tests
ausgewertet, bis einer zutrifft - in dem Fall wird dann der
zugehörige Ausdruck zum Wert der `cond`-Form gemacht.

Es fehlen im Beispiel noch die Ausdrücke, welche die jeweiligen
Maßnahmen liefern.  Die verschiedenen Maßnahmen bilden eine
*Aufzählung*, also eine feste Menge von Möglichkeiten.  In Clojure
kommen für Aufzählungen Keywords zum Einsatz.  Das sieht dann so aus:

{% highlight clojure %}
(defn flensburg-massnahme
    "Maßnahme für eine gegebene Flensburg-Punktezahl berechnen." 
    [p]
    (cond
       (<= 1 p 3) :vormerkung
       (<= 4 p 5) :ermahnung
       (<= 6 p 7) :verwarnung
       (>= p 8) :entziehung))
{% endhighlight %}

(Wer Scheme oder einen anderen Lisp-Dialekt kennt, muss sich merken,
dass in Clojure nicht jeweils ein Klammernpaar um jeden
Zweig steht.)

Aufpassen muss man, wenn es möglich ist, dass *keiner* der Tests
zutrifft.  Zum Beispiel:

{% highlight clojure %}
(flensburg-massnahme 0) => nil
{% endhighlight %}

Um dies zu verhindern, können wir dem `cond` einen Zweig hinzufügen,
der immer zutrifft, wenn alle anderen Tests fehlschlagen.
Prinzipiell ist es möglich, einfach `true` als Test zu verwenden:

{% highlight clojure %}
(defn flensburg-massnahme
    "Maßnahme für eine gegebene Flensburg-Punktezahl berechnen." 
    [p]
    (cond
       (<= 1 p 3) :vormerkung
       (<= 4 p 5) :ermahnung
       (<= 6 p 7) :verwarnung
       (>= p 8) :entziehung
       true :nichts))
{% endhighlight %}

In Clojure ist allerdings Konvention, stattdessen das Keyword `:else`
zu verwenden:

{% highlight clojure %}
(defn flensburg-massnahme
    "Maßnahme für eine gegebene Flensburg-Punktezahl berechnen." 
    [p]
    (cond
       (<= 1 p 3) :vormerkung
       (<= 4 p 5) :ermahnung
       (<= 6 p 7) :verwarnung
       (>= p 8) :entziehung
       :else :nichts))
{% endhighlight %}

Das funktioniert deshalb, weil in Clojure *jeder* Wert, der nicht
`false` oder `nil` ist, als "logisch wahr" gilt.

Oft ist eine Verzweigung *binär*, hat also nur einen "richtigen"
Test und einen `:else`-Zweig, wie etwa diese Funktion:

{% highlight clojure %}
(defn absolute
  "Absolutbetrag berechnen."
  [n]
  (cond
   (< n 0) (- n)
  :else n))
{% endhighlight %}

Binäre Verzweigungen können wir etwas kompakter mit
[`if`](http://conj.io/store/v0/org.clojure/clojure/1.6.0/clj/clojure.core/if/)
schreiben:

{% highlight clojure %}
(defn absolute
  "Absolutbetrag berechnen."
  [n]
  (if (< n 0)
    (- n)
    n))
{% endhighlight %}

Der Umstand, dass alles außer `false` und `nil` als "wahr" gilt,
machen sich Clojure-Programme oft zunutze, zum Beispiel beim Zugriff
auf Maps. Nehmen wir an, eine Funktion solle eine Adresse in einer Map
nachschlagen und, falls diese nicht vorhanden ist, `:unbekannt` zurückliefern:

{% highlight clojure %}
(def adressbuch
  {"Mike Sperber" "Hornbergstraße 49, 70794 Filderstadt"
   ...
   })

(defn adresse
  "Adresse nachschauen."
  [name]
  (if-let [a (get adressbuch name)]
    a
    :unbekannt))
{% endhighlight %}

Die
[`if-let`-Form](http://conj.io/store/v0/org.clojure/clojure/1.6.0/clj/clojure.core/if-let/)
akzeptiert eine Bindung der Form `[<name> <exp>]` als ersten
Operand, wobei `<name>` ein Name und `<exp>` ein Ausdruck ist.  Wenn
der Ausdruck einen wahren Wert liefert, wird er an `<name>` gebunden
und der erste Zweig der `if-let`-Form gebunden - ansonsten der zweite
Zweig.

Auch dieses Beispiel können wir noch kürzer schreiben, weil
[`or`](http://conj.io/store/v0/org.clojure/clojure/1.6.0/clj/clojure.core/or)
nicht einfach `true` liefert, wenn ein Operand "wahr" ergibt, sondern
stattdessen den *Wert des ersten Operanden, der "wahr" ergibt*:

{% highlight clojure %}
(defn adresse
  "Adresse nachschauen."
  [name]
  (or (get adressbuch name)
      :unbekannt))
{% endhighlight %}

Soweit so gut für heute!

<!-- more end -->
