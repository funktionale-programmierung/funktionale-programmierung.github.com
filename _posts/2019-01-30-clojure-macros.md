---
layout: post
description: "Makros in Clojure"
title: "Makros in Clojure"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "unquote", "backquote", "quasiquote"]
---

*If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.*
  ---Guy L. Steele

Mit diesem &mdash; zugegebenermaßen provokanten &mdash; einführenden Zitat möchten wir Dir
das heutige Blogpost-Thema *Clojure Makros* näherbringen.
Wir werden "Programme schreiben, die Programme schreiben"!

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*

## Warum *Du* Makros willst

Makros erlauben es, die Programmiersprache auf natürliche Art und Weise zu erweitern.
So kann man Sprachfeatures, welche die Sprache selbst nicht besitzt,
eigenständig hinzufügen und muss nicht darauf hoffen, dass die SprachenentwicklerInnen
dieses Feature irgendwann einbauen (oder auch nicht).
Beginnen möchten wir mit einer hilfreichen Kontrollstruktur:

`when` ist die Abkürzung von `if-then-else` wenn die Alternative, der `else`-Zweig,
`nil` ist.
Man kann mit `when` also

{% highlight clojure %}
(when true-or-false
      (some stuff))
{% endhighlight %}

statt

{% highlight clojure %}
(if true-or-false
    (some stuff)
    nil)
{% endhighlight %}

schreiben.

Angenommen, die Clojure-EntwicklerInnen hätten "vergessen", `when` zu implementieren,
in unserem Code möchten wir das Feature trotzdem gerne verwenden.
Als Funktion könnte das folgendermaßen aussehen:

{% highlight clojure %}
(defn my-when
  [pred then]
  (if pred
      then
      nil))
{% endhighlight %}

Schön, `(my-when true "Hallo Du")` liefert tatsächlich `"Hallo Du"` und
`(my-when false "Hallo Du")` liefert `nil`.
Doch Halt! Was passiert bei der Auswertung von
`(my-when false (println "Ich sollte nicht geprintet werden"))`?
Richtig, die Funktion druckt &mdash; trotz `false` als Bedingung &mdash;
den Satz "Ich sollte nicht geprintet werden" in die Konsole.

Eine einfache Funktion erfüllt scheinbar unsere Anforderungen nicht ganz.
Wieso nicht? In Clojure werden bei Funktionsaufrufen die Argumente
ausgewertet, bevor die Funktion aufgerufen wird.
Damit sind gerade Konsequenten (`then`-Zweige)  mit Nebeneffekten problematisch.
(Man bedenke nur `(when enemy? (shoot-missiles!))`)

## Makros

Eine Funktion aufzurufen, ist offenbar nicht das richtige, um die
gewünschte Funktionsweise von `my-when` zu realisieren: Stattdessen
hätten wir gern, dass der Compiler die `my-when`-Form durch die
entsprechende `if`-Form *ersetzt*: Das geht mit einem Makro.

Ein Makro ist eine Funktion, die vom Compiler bei der Verarbeitung des
Quelltexts aufgerufen wird:

{% highlight clojure %}
(defmacro my-when
  [pred then]
  ...)
{% endhighlight %}

Der Makro muss Output produzieren, der vom Compiler als Code
akzeptiert wird.  Das geht mit einem Trick: Es ist möglich,
Clojure-Werte zu erzeugen, die ausgedruckt genauso aussehen wie
Clojure-Quelltext.

Wir hätten zum Beispiel gern den Quelltext

{% highlight clojure %}
(if true "Hallo" nil)
{% endhighlight %}

Vom `if` einmal abgesehen, könnten wir folgendes versuchen:

{% highlight clojure %}
(list ??? true
      "Hallo"
      nil)
{% endhighlight %}

Die Liste sorgt für die nötigen runden Klammern.  Wir benötigen nur
noch einen Wert, der als `if` ausgedruckt wird.  Dafür gibt es einen
Extra-Datentyp in Clojure, das *Symbol*.  Symbol-Literale fangen mit
einem Apostroph an:

{% highlight clojure %}
(list 'if true
      "Hallo"
      nil)
{% endhighlight %}

Mit dem Apostroph gibt `(list 'if true "Hallo" nil)` nun tatsächlich die gewünschte Form
`(if true "Hallo" nil)` zurück! Zum eigentlichen Makro ist es nun nicht mehr weit:

{% highlight clojure %}
(defmacro my-when
  [pred then]
  (list 'if pred
        then
        nil))
{% endhighlight %}

Wenn im Source-Code steht `(my-when (= 1 1) "Hallo!")`, dann ruft der
Compiler den `my-when`-Makro mit den Argumenten `(= 1 1)`  und
`"Hallo!"` auf: Das erste Argument ist eine Liste mit den Elementen
`=` (ein Symbol), `1` und `1`, das zweite einfach die Zeichenkette
`"Hallo!"`.  Das heißt der Compiler übergibt an den Makro Quelltext
für die Operanden der Makro-Benutzung und erwartet *expandierten*
Quelltext zurück.

Wichtig zu wissen ist, dass bei der Kompilierung von Clojure-Code Makros *vor* der
eigentlichen Evaluierung des restlichen Codes ausgeführt werden
(Makro-Expansionszeit, dazu in einem späteren Beitrag mehr).
Um zu sehen, was ein Makro zur Expansionszeit zurückgibt, kann man die Funktion
`macroexpand-1` (bzw. `macroexpand`) benutzen:

{% highlight clojure %}
(macroexpand-1 '(my-when (= 1 1)
                         "Hallo!"))
{% endhighlight %}

Damit `macroexpand-1` funktioniert, müssen wir dem zu expandierenden
Quelltext ebenfalls ein Apostroph voranstellen.  (Im nächsten Blog-Post
dieser Reihe werden wir das näher erläutern.)

Dies liefert uns:

{% highlight clojure %}
(if (= 1 1) "Hallo!" nil)
{% endhighlight %}

also genau den Code, den wir abkürzen wollten.
Wenn man also Makros schreiben möchte, ist es sinnvoll, sich im Vorhinein
immer zu überlegen, welchen *Code* man erzeugen möchte.

Als weiteres Beispiel möchten wir nun ein Makro schreiben,
das Berechnungen in Infix-Notation akzeptiert.
Das heißt `(calc-infix (2 + 3))` soll `5` ergeben.
Unser Makro `calc-infix` bekommt also eine Form
(Liste mit drei Elementen) und soll darin das erste und zweite Argument vertauschen.

{% highlight clojure %}
(defmacro calc-infix
  [form]
  (list (second form) (first form) (nth form 2)))
{% endhighlight %}

Hier muss kein einziger Ausdruck mit Apostroph versehen sein.
`calc-infix` zeigt die Mächtigkeit von Makros:
Wir können nun Code schreiben, der eine andere Syntax erlaubt! Dies ist gerade für
DSLs (domänenspezifische Sprachen) ein großer Vorteil.


## Fazit und Ausblick

Mit Makros können wir auf elegante Art und Weise unsere Programmiersprache individuell
an gegebene Problemstellungen anpassen.
In diesem ersten Blogpost wurde die Funktionsweise von und Denkweise hinter Makros
erläutert. Anhand von zwei Beispielen wurde gezeigt, wie wir die Syntax von Clojure
an unsere Bedürfnisse anpassen können.

In einem späteren Blogpost werden wir auf das sogenannte *Back-* oder *Syntax-Quote*, den *Unquote-* und *Unquote-Splicing-*Operator eingehen.
Dabei entwickeln wir nebenher unser eigenes Recordtypen-Makro.

<!-- more end -->
