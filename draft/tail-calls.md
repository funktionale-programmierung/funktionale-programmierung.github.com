---
layout: post
description: "Tail Calls und ihre Bedeutung in der funktionalen Programmierung"
title: "Tail Calls"
author: michael-sperber
tags: ["Endrekursion", "tail calls", "Schleifen"]
---

Wer mit funktionalen Programmierern über die Programmierung an sich
diskutiert, wird früher oder oder später auf das Thema *proper tail
calls* bzw. im deutschen *Endrekursion* stoßen.  Funktionale
Programmierer halten dieses Feature bei der Programmierung mit einer
Selbstverständlichkeit unerlässlich, die Vertreter anderer Sprachen
oft als fanatisch empfinden.

Tatsächlich ist ist Endrekursion in der funktionalen Programmierung
von zentraler Bedeutung.  Tatsächlich jedoch sollte sie
objektorientierten Programmierern eigentlich noch wichtiger sein.

<!-- more start -->

# Was ist Endrekursion?

Das tolle an Funktionsaufrufen, dass sie geschachtelt werden können.
Hier ein Beispiel in Scheme:

{% highlight scheme %}
(define (f x)
  (+ (g x) 15))
{% endhighlight %}

Uns interessiert der Aufruf von `g` in diesem Beispiel - dieser hat
sogenannten *Kontext*, nämlich die Addition von 15.  Anders gesagt:
wenn der Aufruf von `g` fertig ist, so ist `f` noch nicht fertig - `f`
muss noch 15 auf das Ergebnis addieren.

Die meisten Programmierer haben für die Auswertung eines Aufrufs von
`f` ein mentales Modell, das auf einem Stack basiert: Bevor die
Maschine, die `f` ausführt, zur Ausführung von `g` übergeht, sichert
sie auf dem Stack eine *Rücksprungadresse* zu dem Code, der die 15
addiert.  Zusammen mit dieser Rücksprungadresse werden unter Umständen
noch die Werte lokaler Variablen zu einem *Aktivierungsblock*
kombiniert.  (Der funktionale Programmierer spricht gern von
[*Continuation*]({% post_url 2013-10-31-continuations-praxis %}).)
Die Maschine springt die Rücksprungadresse an, sobald die Arbeit von
`g` erledigt ist.

Damit ist klar: Ein Funktionaufruf benötigt zur Laufzeit Platz.  In
vielen Implementierungen von Programmiersprachen befindet sich dieser
Platz auf einem *Stack*, für den oft nur sehr begrenzter Platz zur
Verfügung steht.

Was aber ist mit diesem Programm?

{% highlight scheme %}
(define (f1 x)
  (g x))
{% endhighlight %}

Hier hat der Aufruf von `g` keinen Kontext. Wenn also `g` fertig ist,
dann besteht der Code an der Rücksprungadresse selbst nur aus einem
Sprung zur Rücksprungadresse des Aufrufs von `f1`, ist also trivial.
Das lässt sich demnach "optimieren": Anstatt dass beim Aufruf von `g`
eine neue triviale Continuation angelegt wird, könnte die Maschine
diese gar nicht erst anlegen und so dafür sorgen, dass, wenn `g`
fertig ist, die Maschine direkt zur Continuation des Aufrufs von `f1`
zurückspringt.  (Ganz so einfach ist es nicht, da im Aktivierungsblock
noch lokale Variablen stehen, deren Platz wiederverwendet werden muss,
aber das Prinzip stimmt.)

Aufrufe wie der von `g` in `f1` - die keinen Kontext haben - heißen
*tail calls*.  ("Tail", da sie gewissermaßen den Schwanz der
umschließenden Funktion bilden.)  Im Deutschen hat sich leider noch
kein griffiger Begriff dafür durchsetzen können.  Wenn bei einem *tail
call* keine neue neue Continuation angelegt wird, spricht man von
*proper tail call*.  Programmiersprachen, die alle *tail calls* als
*proper tail calls* behandeln, heißen *properly tail-recursive*
bzw. unterstützen *Endrekursion*.  In der
Programmiersprache Scheme ist Endrekursion [die
fundamentale
Eigenschaft](http://www.wisdomandwonder.com/article/509/lambda-the-ultimate-goto).

Ob ein Funktionsaufruf ein *tail call* ist, ist eine syntaktische
Eigenschaft des Kontextes des Aufrufs.  Hier ist zum Beispiel [die
Definition dafür im
Scheme-Standard](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.20).
 
# Endrekursion in der funktionalen Programmierung

Endrekursion ist in der funktionalen Programmierung alltäglich.  Hier
ist eine einfache endrekursive Funktion in Scheme, welche die Summe
einer Liste bildet:

{% highlight scheme %}
(define (list-sum lis)
  (list-sum-helper lis 0))
  
(define (list-sum-helper lis sum)
  (cond
    ((null? lis) sum)
    ((pair? lis)
	 (list-sum-helper (cdr lis)
	                  (+ sum (car lis))))))
{% endhighlight %}

Der rekursive Aufruf von `list-sum-helper` ist ein *tail call* und
Scheme garantiert, dass er als *proper tail call* ausgeführt wird.
Das heißt, dass, egal wie lang die Liste ist, nicht unbegrenzt
Continuations angelegt werden und den Speicher vollmachen.  Würde
Scheme nicht Endrekursion unterstützen, müsste die Programmiererin
befürchten, dass bei langen Listen der Speicher überläuft.  Obwohl
*proper tail calls* erstmal nicht notwendigerweise rekursiv sind,
kommt dieser positive Aspekt vor allem rekursiven Aufrufen zugute.

Ein etwas ausführlicheres Beispiel für eine endrekursive Funktion
findet sich im
[Posting von Andreas Bernauer]({% post_url 2013-10-23-schleifen-scala %}).

In traditionellen Sprachen kommen für Funktionen wie `list-sum`
vornehmlich Schleifenkonstrukte wie `while` oder `for` zum Einsatz.
Diese haben aber fast alle ein fundamentales Designproblem: Sie lassen
sich nur imperativ benutzen - die Kommunikation zwischen
aufeinanderfolgenden Schleifendurchläufen sowie aus der Schleife
heraus geht nur mit Zuweisungen, und auf diese verzichten funktionale
Programmierer [bekanntermaßen]({% post_url 2013-03-12-rein-funktional %}), wenn irgendmöglich.

Das muss allerdings nicht sein: Das
[`do`-Schleifenkonstrukt](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-6.html#node_chap_5)
in Scheme zeigt, dass es auch ohne Zuweisungen geht.  Außerdem ist es
so, dass zum Beispiel viele Scheme-Implementierungen (wegen der
Unterstützung von
[First-Class-Continuations](http://en.wikipedia.org/wiki/Continuation#First-class_continuations))
erlauben, unbegrenzt viele Continuations anzulegen, ohne also durch
einen unterdimensionierten Stack beschränkt zu sein.

Paradoxerweise ist die Endrekursion also in funktionalen Sprachen gar
nicht von so zentraler Bedeutung und rechtfertigt den Fantismus
eigentlich nicht.

Einige coole Beispiele gibt es aber trotzdem:

* Automaten lassen sich mit Endrekursion [deutlich eleganter
  implementieren](http://www.cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/)
  als mit Schleifen.
* In Erlang wird Endrekursion oft benutzt, um [Hot Code
  Swapping](http://theschemeway.blogspot.de/2008/09/hot-code-swapping-pitfalls-in-erlang.html)
  zu implementieren.

# Endrekursion in der objektorientierten Programmierung

In der objektorientierten Programmierung ist Endrekursion mindestens
ebenso relevant:

Die objektorientierten Programmierung basiert ja eigentlich auf dem
Paradigma des *Message-Passing*, bei dem die Programmausführung im
wesentlichen daraus besteht, dass sich Objekte gegenseitig Nachrichten
schicken, die dann beim jeweiligen Zielobjekt mit Hilfe des
objektorientierten Dispatch zur Ausführung einer Methode führt.  Wenn
die Programmiersprache nicht endrekursiv ist - jedes Versenden einer
Nachricht kostet also Speicherplatz - so sind alle Nachrichtenketten
in ihrer Länge widerspricht.

Dies führt dazu, dass in den meisten OOP-Sprachen zentrale Konstrukte
gerade *nicht objektorientiert* sind: So haben sich spezialisierte
Schleifenkonstruke wie `while` und `for` in vielen OO-Sprachen
gehalten.  Diese funktionieren allesamt nicht über Methodendispatch
sondern sind traditionelle Konstrukte.

Dies ist umso deprimierender, weil es eigentlich einschlägige
OO-Patterns wie das
[*Visitor-Pattern*](http://en.wikipedia.org/wiki/Visitor_pattern)
gibt.  Das Visitor-Pattern ist aber gerade in Sprachen ohne
Endrekursion oft nur eingeschränkt verwendbar und fristen deshalb in
vielen Pattern-Handbüchern nur ein (unverdiente)
Mauerblümchen-Dasein.

Ausführlichere Betrachtungen zu diesem Thema finden sich in [Guy
Steeles
Blog-Post](http://www.eighty-twenty.org/index.cgi/tech/oo-tail-calls-20111001.html).

# Die JVM und die Endrekursion

Die JVM ist ein Paradebeispiel dafür, welche Chancen für sauberes
Programmiersprachendesign vertan werden, wenn die 
Maschine keine Endrekursion unterstützt.

Die JVM kombiniert dabei gleich zwei Probleme: Sie unterstützt keine
Endrekursion und JVM-Jacks sind in der Regel in der Größe stark
beschränkt, so dass schon kleinere Datenstrukturen, die mit
Methodenaufrufen durchlaufen werden, zu Stack Overflows führen.

Entsprechend müssen alle JVM-Sprachen, die mit anderen JVM-Programmen
interoperabel bleiben wollen, spezielle Konstrukte für Schleifen
anbieten.

Scala erlaubt immerhin lokale *tail calls* durch die
[`tailrec`-Annotation](http://www.scala-lang.org/api/current/index.html#scala.annotation.tailrec).
Das reicht aber [nicht für alle Probleme](http://apocalisp.wordpress.com/2011/10/26/tail-call-elimination-in-scala-monads/).
Clojure lässt das Schleifenkonstrukt so aussehen, als würde es durch
Funktionsaufrufe funktionieren.  Die grundlegenden Probleme bleiben
also.

Andere Einschränkungen der JVM verbünden sich mit der fehlenden
Unterstützung für Endrekursion: So dürfen zum Beispiel Methoden nur
64k groß werden.  Es ist zwar möglich, eine zu große Methode in
mehrere kleinere zu
[splitten](https://bitbucket.org/sperber/asm-method-size/), das
funktioniert aber nur unter bestimmten Bedingungen, da für den
Kontrolltransfer zwischen den Teilen wiederum nur nicht-endrekursive
Methodenaufrufe zur Verfügung stehen.

Es wird also höchste Zeit, dass sich Oracle des Problems annimmt.  Wie
das technisch geht, ist [schon lange
bekannt](https://blogs.oracle.com/jrose/entry/tail_calls_in_the_vm).

# Das Argument mit dem Stacktrace

Forderungen nach Endrekursion gibt es in vielen Sprachen, stoßen aber
leider oft auf Unverständnis.  Ein [viel beachtetes Posting von Guido
van
Rossums](http://neopythonic.blogspot.de/2009/04/tail-recursion-elimination.html)
argumentiert, warum Python nicht endrekursiv ist.  Dabei stellt van
Rossum spektakuläres technisches Unverständnis zur Schau: Während er
verstanden hat, dass Endrekursion nicht als bloße optionale
Optimierung taugt, argumentiert er, dass "TRE is incompatible with
nice stack traces" ist.  (TRE = "tail recursion elimination", eine andere
Bezeichnung für Endrekursion.)

Eine naive Implementierung von Endrekursion generiert in der Tat für
*tail calls* keineAktivierungsblöcke, die damit auch nicht im Stack
Trace auftauchen.  Dort wären sie allerdings unter Umständen beim
Debuggen hilfreich.  Allerdings vergisst er anzumerken, dass die
Schleifenkonstrukte in Python, die statt *tail calls* verwendet werden
müssen, ebenfalls keine Aktivierungsblöcke generieren.  Wenn ihm
Stack-Traces wichtig wären, könnte er einfach einen größenbeschränkten
Ringpuffer für die letzten N Aktivierungsblöcke anlegen, welche damit
sogar noch mehr Information liefern würden als Pythons jetzige
Implementierung.  Dies würde die gleichen Speichereigenschaften haben
wie die naive Implementierung - zumindest asymptotisch, und das [zählt
in diesem
Fall](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.4500).

# Fazit

Funktionale Programmierer schätzen die zentrale Bedeutung von
Endrekursion korrekt ein.  Objektorientierte Programmierer sollten
dies auch tun und Endrekursion von ihren Sprachen fordern.

<!-- more end -->

