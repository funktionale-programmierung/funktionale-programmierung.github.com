---
layout: post
description: "Funktional Programmieren mit Wahrscheinlichkeiten"
title: "Die Wahrscheinlichkeitsverteilungsmonade"
author: michael-sperber
tags: ["Racket", "Monaden"]
---

Uns erreichte neulich die Frage "Wozu sind
Monaden eigentlich in dynamisch getypten Sprachen" gut?  In Haskell
zum Beispiel sind ja [Monaden fast allgegenwärtig]({% post_url 2013-04-18-haskell-monaden %}) 
und haben dort insbesondere die Aufgabe, anhand der Typen deutlich zu
machen, welche Effekte ein Entwickler bei der Auswertung eines
Ausdrucks befürchten muss.  Da Typen in dynamisch getypten Sprachen
fehlen, ist die Frage legitim, da viele Monaden nur jeweils einen
Wert als Resultat einer Berechnung mit Seiteneffekten produzieren.  Es
gibt allerdings auch Monaden, die mehr leisten wie zum Beispiel die
Monade der Wahrscheinlichkeitsverteilungen.  Um die geht es in diesem
Beitrag - und zwar in einer dynamisch getypten Sprache, nämlich in
[Racket](http://racket-lang.org/).  Racket bietet für diese Anwendung
relevante Abstraktionsmöglichkeiten, die noch über die Fähigkeiten von
Haskell hinausgehen.

<!-- more start -->

Zunächst mal gibt es auch in Uwe Schmidts [Posting zu Monaden in
Haskell]({% post_url 2013-05-22-haskell-monaden2 %}) ein Beispiel für eine
Monade, die nicht bloß "einen Wert nach Effekten" liefert, nämlich die
Nichtdeterminismus-Monade, die erlaubt, aus einem Ausdruck gleich
mehrere mögliche Werte zu liefern.  Das ist bei dem dortigen
Interpreter hauptsächlich ein Gimmick, aber die Idee lässt sich zu
einer Monade ausbauen, mit der Wahrscheinlichkeits-Szenarien bewertet
werden können.  

Wir orientieren uns an der Sprache
[HANSEI](http://okmij.org/ftp/kakuritu/index.html), die ursprünglich
in OCaml eingebettet war.  Das dazugehörige Paper beschreibt folgendes Szenario:

> Ein Rasen kann nass werden, und zwar durch Regen, Sprinklereinsatz
> oder aus einem anderen Grund.  Die Wahrscheinlichkeitsverteilung ist
> folgendermaßen definiert:
>
> Pr(rain) = 0.3
>
> Pr(sprinkler) = 0.5

Das bedeutet soviel wie: Zu einem beliebigen Zeitpunkt regnet es mit
30% Wahrscheinlichkeit und der Sprinkler ist mit 50%
wahrscheinlichkeit eingeschaltet.   Außerdem:

> Wenn es regnet, ist der Rasen mit 90% Wahrscheinlichkeit nass.  Wenn
> der Sprinkler an ist, ist der Rasen mit 80% Wahrscheinlichkeit
> nass.  Außerdem besteht eine 10%ige Wahrscheinlichkeit, dass der
> Rasen aus einem anderen Grund nass ist.

Nun wollen wir in diesem Posting die Wahrscheinlichkeit Pr(rain |
grass-is-wet) ausrechnen, also die Wahrscheinlichkeit, dass es regnet,
*wenn der Rasen nass ist*.  Wie könnte so etwas als Programm aussehen?
Hier ist ein Vorschlag:

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

(Eine Einführung in Racket befindet sich in einem [früheren
Posting]({% post_url 2013-03-12-rein-funktional %}).)

Die erste Prozedur hält die Bedingung dafür fest, dass der Rasen nass
ist.  Dabei bedient sie sich einer Prozedur `flip`, die "eine Münze
wirft" und entweder `#t` oder `#f` produziert.  `(flip 0.9)`
produziert mit 90% Wahrscheinlichkeit `#t`.  `(and (flip 0.9) rain)`
liefert also die Wahrscheinlichkeit, dass die Münze `#t` liefert *und*
es regnet etc.

Die Prozedur `grass-model` sagt dann, ob es regnet - und zwar nur
dann, wenn der Rasen nass ist, entsprechend der Aufgabenstellung,
Pr(rain | grass_is_wet) auszurechnen.  Die Funktionen sehen aus wie
ganz normale Scheme-Funktionen.  Idealerweise würden sie eine präzise
Wahrscheinlichkeitsverteilung liefern.  Das allerdings scheint erstmal
schwierig: Da der Rückgabewert von `flip` als Argument für `and`
verwendet wird, müsste `flip` einen einzelnen booleschen Wert liefern.
Dieser boolesche Wert könnte zum Beispiel durch einen
Pseudozufallszahlengenerator erzeugt werden und bei `(flip 0.9)` mit
90% Wahrscheinlichkeit `#t` und mit 10% Wahrscheinlichkeit `#f`
liefern.  Damit eignet sich das Programm scheinbar nur fürs
*Sampling*: Wir lassen sie einfach 1000mal laufen und zählen nach, wie
oft `#t` und wie oft `#f` herausgekommen ist.  Das ist hier aber
suboptimal, da es möglich ist, die Wahrscheinlichkeit genau
auszurechnen, ohne Sampling zu machen.  Damit das geht, müssen wir zunächst
(aber nur vorläufig) einen Umweg über - na klar - *Monaden* gehen.

Wir bauen also eine Monade für Wahrscheinlichkeitsverteilungen, die
also zum Beispiel explizit sagen kann, "dieser Wert ist mit 90%
Wahrscheinlichkeit `#t` und mit 10% `#f`", oder "diesert Wert ist mit
40% Wahrscheinlichkeit 1, mit 30% Wahrscheinlichkeit 2, mit 20% 3 und
mit 10% 4" usw.  Zu diesem Zweck benutzen wir eine Liste aus Paaren.
Jedes Paar besteht aus einer Wahrscheinlichkeit und dem Wert, der mit
dieser Wahrscheinlichkeit herauskommt.  Die obigen Beispiele lauten
also folgendermaßen:

{% highlight scheme %}
((0.9 . #t) (0.1 . #f))
((0.4 . 1) (0.3 . 2) (0.2 . 3) (0.1 . 4))
{% endhighlight %}

Jetzt definieren wir die Prozedur `flip`, die wir oben benutzt haben:

{% highlight scheme %}
(define (flip p)
  (dist (list (cons p #t)
              (cons (- 1.0 p) #f))))
{% endhighlight %}

Diese macht eine Liste, in der mögliche Werte ihren
Wahrscheinlichkeiten zugeordnet sind, und ruft dann eine weitere
Funktion `dist` auf, die daraus ein
Wahrscheinlichkeitsverteilung-Objekt macht.  Diese Funktion hat eine
ziemlich triviale Definition, da diese Liste bereits das richtige Format:

{% highlight scheme %}
(define (dist ch) ch)
{% endhighlight %}

(Warum dann überhaupt die Funktion einführen, wenn sie trivial ist?
Weil wir sie später ändern wollen!)

Entsprechend zu `flip` definieren wir noch `fail` als leere
Verteilung:

{% highlight scheme %}
(define (fail) (dist '()))
{% endhighlight %}

Natürlich funktionieren die eingebauten Operatoren `and` und `or`
nicht mit solchen Wahrscheinlichkeitsverteilungen.  Wir müssen also
zunächst eigene Prozeduren definieren:

{% highlight scheme %}
(define (con e1 e2) ...)
(define (dis e1 e2) ...)
{% endhighlight %}

Mit deren Hilfe können wir `grass-is-wet` umschreiben:

{% highlight scheme %}
(define (grass-is-wet? rain sprinkler)
  (dis (con (flip 0.9) rain)
       (dis (con (flip 0.8) sprinkler)
            (flip 0.1))))
{% endhighlight %}

Aber wie könnten wir `con` und `dis` definieren?  Wir müssten alle
Kombinationen möglicher Werte ihrer Argumente durchspielen!  Ähnliches
für jede Andere Prozedur, die auf Wahrscheinlichkeitsverteilungen
arbeiten soll.  Das jedesmal von vorn zu programmieren, ist mühsam.
Es geht aber auch einfacher: Eine Wahrscheinlichkeitsverteilung
beschreibt eine stochastische Berechnung, und
Wahrscheinlichkeitsverteilungen können miteinander kombiniert werden.
Erfahrene funktionale Programmierer denken bei diesem Satz *Mal
schauen, ob das nicht eine Monade ist.*  Dafür bräuchten wir eine
"unit"- und eine "bind"-Operation.  "Unit" ist einfach: `(pv-unit v)`
muss aus einem einzelnen Wert `v` eine Wahrscheinlichkeitsverteilung
machen - es liegt nahe, die Verteilung zu wählen, bei der sicher `v`
herauskommt:

{% highlight scheme %}
(define (pv-unit x)
  (list (cons 1.0 x)))
{% endhighlight %}

Bei "bind" ist es etwas schwieriger: Es akzeptiert eine
Wahrscheinlichkeitsverteilung und eine Funktion, die einen Wert aus
der Wahrscheinlichkeitsverteilung akzeptiert.  Da eine
Wahrscheinlichkeitsverteilung mehrere Werte enthalten kann, müssen wir
auch die Funktion auf jeden einzelnen möglichen Wert loslassen:

{% highlight scheme %}
(define (pv-bind m f)
  (map (lambda (p)
         (let ((prob (car p))
               (branch (cdr p)))
           (cons prob ... (f branch) ...)))
       m))
{% endhighlight %}

Das blöde ist nur, dass `(f branch)` selbst wieder eine
Wahrscheinlichkeitsverteilung liefert, die wir irgendwie
"ausmultiplizieren" müssten.  Wir lösen dieses Problem erstmal
dadurch, dass wir uns darum drücken.  (Der tiefere Sinn davon wird
sich noch später erschließen.)  Wir erweitern die Repräsentation von
Wahrscheinlichkeitsverteilungen so, dass bei einem Paar statt eines
regulären Wertes auch eine *verzögerte Wahrscheinlichkeitsverteilung*
kleben kann.  Damit wir diese von den regulären Werten unterscheiden
können, machen wir einen neuen Typ dafür names `deferred`:

{% highlight scheme %}
(struct deferred (promise))
{% endhighlight %}

Dies definiert ein Struct mit einem einzelnen Feld, das (wie der Name
sagt) ein *Promise* ist, also eine verzögerte Berechnung.  In Racket
wird eine Promise mit `(delay <exp>)` erzeugt.  Der Ausdruck `<expr>`
wird erst dann ausgewertet, wenn die Promise mit `(force p)`
eingefordert wird.  Diese beiden Operationen - das Einpacken und
Auspacken eines `deferred`-Wertes, können wir in zwei Abstraktionen
packen:

{% highlight scheme %}
(define-syntax defer
  (syntax-rules ()
    ((defer ?x) (deferred (delay ?x)))))

(define (undefer d)
  (force (deferred-promise d)))
{% endhighlight %}

Die Funktion `undefer` packt die Promise aus dem `deferred`-Struct
aus, und fordert dann ihren Wert mit `force` an.  Fürs Einpacken wäre
eine naive Lösung folgende Definition:

{% highlight scheme %}
(define (defer x)
  (deferred (delay x)))
{% endhighlight %}

Das funktioniert allerdings nicht, weil Racket eine
*Call-by-Value*-Sprache ist: Wenn `defer` aufgerufen wird, wird vorher
ihr Argument berechnet.  Das wollen wir gerade verzögern.  (Hier sind
Haskell-Programmierer natürlich fein raus, weil da *alles* verzögert
wird.)  Deshalb müssen wir einen *Makro* bemühen, der mit `define-syntax`
definiert wird.  Die obige Definition besagt einfach, dass ein
Makro-"Aufruf" der Form `(defer ?x)` durch `(deferred (delay ?x))`
ersetzt wird.  (Das `?` in `?x` ist reine Konvention.)

Mit Hilfe von `defer` und `undefer` können wir jetzt die Definition
von `pv-bind` vervollständigen.  Dafür müssen wir jetzt bei jedem
Listelement der Wahrscheinlichkeitsverteilung nachschauen, ob es sich
um ein `deferred` oder einen regulären Wert handelt:

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

Nun können wir `pv-unit` und `pv-bind`  benutzen, um `con` und `dis`
zu programmieren:

{% highlight scheme %}
(define (con e1 e2)
  (pv-bind e1
           (lambda (v1)
             (if v1
                 e2
                 (pv-unit #f)))))

(define (dis e1 e2)
  (pv-bind e1
           (lambda (v1)
             (if v1
                 (pv-unit #t)
                 e2))))
{% endhighlight %}

(Da `pv-unit` und `pv-bind` nur wenige Male verwendet werden,
verzichten wir auf sexy Monadensyntax wie in Haskell.)

Jetzt können wir gleich mal ausprobieren, was bei `grass-is-wet?` so
rauskommt, wenn wir es auf die Verteilungen für Regen und Sprinkler loslassen:

{% highlight scheme %}
> (grass-is-wet? (flip 0.3) (flip 0.5))
'((0.9 . #<deferred>) (0.09999999999999998 . #<deferred>))
{% endhighlight %}

Jetzt rächt sich, dass wir das Problem mit dem "Ausmultiplizieren"
vor uns hergeschoben haben: Wir müssen noch eine Prozedur schreiben,
die das erledigt.  Dadurch, dass die `deferred`-Verteilungen beliebig
geschachtelt werden, entsteht eine Art Suchbaum mit versteckten
Zweigen, den die Prozedur `explore` bis zu einer vorgegebenen Tiefe
`maxdepth` ausfaltet.  Das Argument `maxdepth` kann auch `#f` sein,
dann wird der Baum komplett ausgefaltet.  Die Prozedur ist für das
Verständnis der weiteren Betrachtungen unwichtig, darum lassen wir sie
unerläutert:

{% highlight scheme %}
(define (explore maxdepth choices)
  (define (loop this-prob depth down? choices ans susp)
    (if (null? choices)
        (values ans susp)
        (let ((prob (car (car choices)))
              (branch (cdr (car choices)))
              (rest (cdr choices)))
          (if (deferred? branch)
              (if down?
                  (let-values (((ans susp)
                                (loop (* prob this-prob)
                                      (+ 1 depth)
                                      (or (not maxdepth) (< depth maxdepth))
                                      (undefer branch)
                                      ans susp)))
                    (loop this-prob depth down? rest ans susp))
                  (loop this-prob depth down? rest ans (cons (cons (* prob this-prob) branch) susp)))
              (loop this-prob depth down? rest
                    (dict-update ans branch (lambda (old) (+ old (* prob this-prob))) 0)
                    susp)))))

  (let-values (((ans susp) (loop 1.0 0 #t choices '() '())))
    (foldl (lambda (p a)
             (cons (cons (cdr p) (car p)) a))
           susp
           ans)))
{% endhighlight %}

Damit können wir jetzt die Verteilung anschauen:

{% highlight scheme %}
> (explore #f (grass-is-wet? (flip 0.3) (flip 0.5)))
'((0.39419999999999994 . #f) (0.6058 . #t))
{% endhighlight %}

Die Wahrscheinlichkeit, dass das Gras nass ist, ist also 60.58%.  Aber
wir wollten ja eine andere Frage beantworten, nämlich, wie hoch die
Wahrscheinlichkeit ist, dass es regnet, wenn das Gras nass ist.  Die
Prozedur `grass-model` benutzt auch noch die Konstrukte `let` und
`if`, die nicht direkt mit der Monade funktionieren.  Wir müssen also
erst noch monadische Pendants programmieren.  Bei `if` sieht das so
aus:

{% highlight scheme %}
(define (if_ et e1 e2)
  (pv-bind et (lambda (t)
                (if t
                    (e1)
                    (e2)))))
{% endhighlight %}
                    
Die Prozedur benutzt also `pv-bind`, um den booleschen Wert des Tests
zu extrahieren und wendet dann darauf das "normale" `if` an.  Die
beiden Zweige `e1` und `e2` werden als nullstellige Prozeduren
übergeben, damit nicht beide ausgewertet werden, bevor der Wert des
Tests überhaupt feststeht.

Bei `let` ist die Sache etwas diffiziler.  Tatsächlich könnten wir das
eingebaute `let` verwenden, das führt aber zu falschen Resultaten, da
in `grass-model` die Variable `rain` zweimal auftaucht, und das
eingebaute `let` einfach die Verteilung kopieren würde.  Tatsächlich
müssen sich beide Vorkommen von `rain` aber auf den gleichen Punkt der
Verteilung beziehen.  Hinzu kommt, dass der `let`-Ersatz eine Variable
binden muss.  Wir benutzen die gängige Technik, eine Funktion zu
übergeben, welche die Variable bindet und in der der Rumpf des `let`
steht:

{% highlight scheme %}
(define (let_ e f)
  (pv-bind e
           (lambda (x)
             (f (pv-unit x)))))
{% endhighlight %}

Mit Hilfer von `let_` und `if_` können wir jetzt `grass-model` so
schreiben, dass es funktioniert:

{% highlight scheme %}
(define (grass-model)
  (let_ (flip 0.3) 
    (lambda (rain)
      (let_ (flip 0.5) 
        (lambda (sprinkler)
          (if_ (grass-is-wet? rain sprinkler)
               (lambda () rain)
               (lambda () (fail))))))))
{% endhighlight %}

Diese Prozedur können wir jetzt aufrufen:

{% highlight scheme %}
> (explore #f (grass-model))
'((0.3219999999999999 . #f) (0.2838 . #t))
{% endhighlight %}

Da `grass-model` gelegentlich `fail` aufruft, ist diese Verteilung
nicht *normalisiert* - die Summe der Wahrscheinlichkeiten ergibt nicht
100%.  Das können wir mit mit zwei `fold`s und einer kleinen
Hilfsprozedur nachholen:

{% highlight scheme %}
(define (normalize choices)
  (let ((total (foldl (lambda (p acc) (+ (car p) acc)) 0 choices)))
    (map (lambda (p)
           (cons (/ (car p) total)
                 (cdr p)))
         choices)))
{% endhighlight %}

Damit bekommen wir:

{% highlight scheme %}
> (normalize (explore #f (grass-model)))
'((0.5315285572796301 . #f) (0.46847144272036984 . #t))
{% endhighlight %}

Wenn also der Rasen nass ist, regnet es mit 46.85% Wahrscheinlichkeit.

Die Version von `grass-model` mit `let` und `if_` sieht jetzt
zumindest schonmal so ähnlich aus wie das ursprüngliche
Wunschprogramm.  Wir können es mit einfachen Mitteln noch etwas
hübscher machen, indem wir die Benutzung von `let_` und `if_` etwas
vereinfachen.  Hier stören ja die ganzen `lambda`s.  Diese können wir
durch Makros einsparen:

{% highlight scheme %}
(define-syntax if-
  (syntax-rules ()
    ((if- ?t ?c ?a)
     (if_ ?t (lambda () ?c) (lambda () ?a)))))
     
(define-syntax let-
  (syntax-rules ()
    ((let- ?v ?e ?b)
     (let_ ?e (lambda (?v) ?b)))))    
{% endhighlight %}

Damit sieht `grass-model` so aus:

{% highlight scheme %}
(define (grass-model)
  (let- rain (flip 0.3) 
    (let- sprinkler (flip 0.5) 
      (if- (grass-is-wet? rain sprinkler)
           rain
           (fail)))))
{% endhighlight %}

Das ist schon nah dran, wo wir hinwollen, aber eben noch nicht ganz
da.  Für den letzten Schritt brauchen wir neben der Monade und der
Makros noch eine weitere Abstraktionsform, die *Kontrollabstraktion*.
Das ist so cool, dass wir uns das Thema für einen separaten Beitrag aufheben.


<!-- more end -->
