---
layout: post
description: "DSLs ohne Nahtstelle mit Kontrollabstraktion"
title: "DSLs ohne Nahtstelle mit Kontrollabstraktion"
author: michael-sperber
tags: ["Racket", "Monaden", "Continuations"]
---

In einem [vorherigen Posting](FIXME) haben wir uns mit einer Monade
für Wahrscheinlichkeitsverteilungen beschäftigt.  Diese erlaubt uns,
ein probabilistisches Szenarium als Programm aufzuschreiben, deren
Ausführungen die Wahrscheinlichkeitsverteilung möglicher Resultate des
Szenarios liefert.  Allerdings ist die explizite monadische
Programmierung etwas umständlich.  Eigentlich hatten wir uns
gewünscht, die Szenarien als "normales" Racket-Programm unter
Verwendung der gewohnten Operatoren wie `and`, `if` und `let` zu
schreiben.  Stattdessen mussten wir bisher monadische Versionen `con`,
`if_` und `let_` benutzen.  In diesem Post zeigen wir, wie wir genau
das erreichen, und zwar unter Verwendung einer geradezu
bewusstseinserweiternden Technik namens *Kontrollabstraktion*, die es
in der Mächtigkeit nur in funktionalen Sprachen gibt.

<!-- more start -->

Um zum Punkt zu kommen, müssen wir allerdings einige Stellen des Codes
aus dem [vorherigen Posting](FIXME) nochmal Revue passieren lassen:

Schauen wir uns die Prozedur `grass-is-wet?` an, welche die
Wahrscheinlichkeitsverteilung von "der Rasen ist nass" vs. "der Rasen
ist nicht nass" anzeigt:

{% highlight scheme %}
(define (grass-is-wet? rain sprinkler)
  (dis (con (flip 0.9) rain)
       (dis (con (flip 0.8) sprinkler)
                (flip 0.1))))
{% endhighlight %}

Der verschachtelte Aufruf im Rumpf erzeugt eine Menge
Zwischenergebnisse, die ihrerseits wieder
Wahrscheinlichkeitsverteilungen sind.  Diese Zwischenergebnisse werden
jeweils "ausmultipliziert" durch die Verwendung des monadischen
`bind`-Operators, zu sehen zum Beispiel an der Implementierung von `con`:

{% highlight scheme %}
(define (con e1 e2)
  (pv-bind e1
           (lambda (v1)
             (if v1
                 e2
                 (pv-unit #f)))))
{% endhighlight %}

In der Implementierung von `pv-bind` findet dann das eigentliche
Ausmultiplizieren statt:

{% highlight scheme %}
(define (pv-bind m f)
  (map (lambda (p)
         (let ((prob (car p))
               (branch (cdr p)))
           (if (deferred? branch)
               (cons prob (defer (pv-bind (undefer branch) f)))
               (cons prob (defer (f branch))))))
       m))
{% endhighlight %}

Die `pv-bind`-Prozedur wendet `f` innen auf jeden Zweig der
Wahrscheinlichkeitsverteilung `m` an.  Man könnte auch sagen, es
schiebt das `f` nach *innen*, und zwar einmal für jeden Zweig.  Wir 
können das `f` aber als *Kontext* betrachten (siehe dazu auch [dieses
frühere Posting]({% post_url 2013-11-08-tail-calls %})): Das `f`
ist die Repräsentation dessen, was mit dem Resultat von `m` passiert.
In dieser Sichtweise wird also der *Kontext* nach innen geschoben.

Damit das überhaupt geht - den Kontext nach innen schieben - muss der
Kontext ein Objekt sein.  Dieses Objekt bekommt unser Programm dadurch
in die Finger, dass es in monadischer Form geschrieben ist, in der der
Kontrollfluss explizit ist.  Wir streben aber gerade an, unser
Programm *nicht* in monadischer Form schreiben zu müssen.  Wie
bekommen wir also den Kontext in die Finger, *ohne* die monadische
Form benutzen zu müssen?

Der Kontext wird normalerweise von der Ausführungsmaschinerie der
Programmiersprache hinter den Kulissen verwaltet.  (Das dort
verwaltete Objekt heißt dann meist *Continuation*.)  In den
Programmiersprachen Scheme und auch in Racket ist es allerdings
möglich, in die diese Ausführungsmaschinerie hereinzugreifen und
die Continuation als Objekt herauszuziehen - sie zu *reifizieren*.
Für diese Reifikation gibt es unterschiedliche APIs.  Wir verwenden
die `shift`/`reset`-API, die ihren Ursprung in [diesem
Paper-Klassiker](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.84)
von Olivier Danvy und Andrzej Filinski hat.  In Racket kommen wir da
heran, in dem wir `racket/control` importieren:

{% highlight scheme %}
(require racket/control)
{% endhighlight %}

Damit sind die Operatoren `shift` und `reset` verfügbar.  Der
Shift-Operator wird so benutzt:

{% highlight scheme %}
(shift k <exp>)
{% endhighlight %}

Dieser Ausdruck bindet an `k` (kann natürlich auch anders heißen),
eine Prozedur, die den Kontext des `shift`-Ausdrucks repräsentiert -
und zwar bis zum nächsten `reset`, das drumrumsteht.  `<exp>` wird
dann ausgewertet, und war im Kontext desselben `reset`.  Zu jedem
`shift` gehört also ein umschließendes `reset`.  Das Konzept von
`shift`/`reset` ist im ersten Anlauf schwer mental zu fassen, einige
Beispiele helfen vielleicht weiter:

{% highlight scheme %}
> (reset (+ 41 (shift k 2)))
2
{% endhighlight %}

Hier wird das `k` gar nicht benutzt, das Beispiel zeigt aber, dass
`shift` die `2` im Kontext vom `reset` auswertet - der Kontext vom
`shift`, also `(+ 41 [])`, wird verworfen.  (Das `[]` - gesprochen
"Loch" - ist der Parameter des Kontext.)  Dieser Kontext steckt
allerdings in `k`, und kann angewendet werden:

{% highlight scheme %}
> (reset (+ 41 (shift k (k 2))))
43
{% endhighlight %}

Also: Der Kontext vom `shift` wird an `k` gebunden, dann verworfen -
im Rumpf vom `shift` aber gleich wieder angewendet.  Der Kontext ist
wieder `(+ 41 [])`, in den wird `2` eingesetzt, es wird daraus also
`(+ 41 2)`.  Die an `k` gebundene Kontext-Prozedur verhält sich wie
jede andere Prozedur auch, kann also insbesondere mehrfach angewendet
werden:

{% highlight scheme %}
> (reset (+ 41 (shift k (k (k 2)))))
84
{% endhighlight %}

Auch hier wird an `k` ein Kontext `(+ 41 [])` gebunden, ist also eine
Prozedur, die 41 auf ihr Argument addiert.  Zweimal 41 auf 2 addieren
ergibt 84.

Zurück zu unserer probabilistischen Monade: Die Verteilung des
Kontexts auf die Zweige einer Wahrscheinlichkeitsverteilung backen wir
direkt in die `dist`-Prozedur ein:

{% highlight scheme %}
(define (dist ch)
  (shift k
         (map (lambda (p)
                (cons (car p)
                      (defer (k (cdr p)))))
              ch)))
{% endhighlight %}

Diese Prozedur schnappt sich also ihren eigenen Kontext und wendet
ihn auf jeden Zweig der gewünschten Wahrscheinlichkeitsverteilung an.
Wenn es mehrere Aufrufe von `dist` gibt, wie ins unserem Beispiel, so
wird diese Kontextanwendung geschachtelt, was gerade das
"Ausmultiplizieren" besorgt.

Jetzt können wir die monadischen Operatoren allesamt neu definieren
mit frappierend einfachen Definitionen:

{% highlight scheme %}
(define (neg e) (not e))

(define (con e1 e2) (and e1 e2))

(define (dis e1 e2) (or e1 e2))

(define (if_ et e1 e2)
  (if et
      (e1)
      (e2)))
          
(define (let_ e f)
  (f e))
{% endhighlight %}

Allerdings müssen wir nun noch das Ergebnis seinerseits wieder zu
einem Suchbaum machen, den wir mit `explore` verarbeiten können.
Insbesondere müssen wie da auch das `reset` unterbringen:

{% highlight scheme %}
(define (reify0 m)
  (reset (pv-unit (m))))
{% endhighlight %}

Wichtig - `reify0` akzeptiert eine nullstellige Prozedur - also
`grass-model`, nicht `(grass-model)`:

{% highlight scheme %}
> (explore #f (reify0 grass-model))
'((0.3219999999999999 . #f) (0.2838 . #t))
{% endhighlight %}

Da jetzt `neg`, durch `not`, `con` durch `and`, `dis` durch `or`,
`if_` durch `if` und `let_` wie `let`  implementiert ist, können wir
jetzt das Programm auch ohne Verwendung explizit monadischer
Konstrukte (im sogenannten *direct style*) schreiben:

{% highlight scheme %}
(define (grass-is-wet? rain sprinkler)
  (or (and (flip 0.9) rain)
      (and (flip 0.8) sprinkler)
      (flip 0.1)))

(define (grass-model)
  (let ((rain (flip 0.3))
        (sprinkler (flip 0.5)))
    (if (grass-is-wet? rain sprinkler)
        rain
        (fail))))
{% endhighlight %}

Der Wunsch vom Anfang ist also in Erfüllung gegangen!  (Sehr schön
auch, dass man `shift`/`reset` bei der probabilistischen
Programmierung nicht selbst in die Hand nehmen muss, sondern nur
einmal bei der Implementierung der primitiven Operationen.)  Nicht nur das,
das Direct-Style-Programm ist auch effizienter.  Während beim
monadische Programm `explore` den Baum bis zu einer Tiefe von 6
abgrasen muss, reicht beim Direct-Style-Programm 4.

Tatsächlich ist es möglich, einem Racket- oder Scheme-Programm
nachträglich eine *beliebige* Monade unterzujubeln (also nicht bloß
die für Wahrscheinlichkeitsverteilungen wie hier), wie die seinerzeit
bahnbrechende [Arbeit von Andrzej
Filinski](http://www.diku.dk/hjemmesider/ansatte/andrzej/papers/RM-abstract.html)
gezeigt hat.

Damit ist Racket/Scheme eine sehr mächtige Plattform für die
Implementierung eingebetteter DSLs, weil sie folgende einzigartige
Kombinationen von Eigenschaften mitbringen:

* Abstraktion über Funktionen (wie jede andere Funktionale Sprache)
* Abstraktion über Syntax mit Makros (hat nicht jede andere funktionale Sprache)
* Abstraktion über Kontrolle mit `shift`/`reset` und anderen
  Operatoren (hat so gut wie niemand sonst)

Geneigenten Lesern sei die
[HANSEI-Seite](http://okmij.org/ftp/kakuritu/logic-programming.html)
mit mehr Informationen zur Einbettung probabilistischer Programmierung
empfohlen.

<!-- more end -->

