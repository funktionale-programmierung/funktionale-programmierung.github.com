---
layout: post
description: "Clojure Makros"
title: "Clojure Makros"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "unquote", "backquote", "quasiquote"]
---

*If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.*
- Guy L. Steele

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
Wieso nicht? In Clojure werden bei Funktionsaufrufen die Argumente automatisch ausgewertet.
Damit sind gerade Konsequenten (`then`-Zweige)  mit Nebeneffekten problematisch.
(Man bedenke nur `(when enemy? (shoot-missiles!))`)

## Makros

Anders als Funktionen werten Makros ihre Argumente nicht automatisch aus, sondern
geben sie unausgewertet an ihren Rumpf weiter.
Ein weiterer Unterschied ist in ihrer Benutzung zu sehen:
Funktionen erhalten einen Wert berechnen damit ggf. etwas und geben dann einen Wert zurück.
Makros hingegen erhalten *Quellcode*, manipulieren diesen ggf. und geben *Quellcode* zurück
(wir sagen dazu ab jetzt auch *(unausgewertete) Formen*).

Zurück zu `my-when`: Der Ausdruck `(my-when true "Hallo")` soll nun,
wenn wir `my-when` als Makro schreiben, eine *Liste* mit *vier* Elementen zurückgeben:
das `if`, das Prädikat `true`, die Konsequente `"Hallo"` und das feste `nil`.
Listen können wir mit dem Listenkonstruktor `(list ...)` erzeugen.
Ein erster Versuch wäre also:

{% highlight clojure %}
(list if true
      "Hallo"
      nil)
{% endhighlight %}

Wenn dieser Code ausgewertet wird, bekommen wir eine Fehlermeldung:
*"Unable to resolve symbol: if in this context"*.
Der Clojure-Evaluator versucht die Argumente von `list` *auszuwerten* und stößt auf `if`.
Wir können den Clojure-Compiler mit `'` anweisen,
das nachfolgende Symbol *nicht* auszuwerten. Wenn wir bspw. `'if` in die REPL eingeben,
wird einfach nur `if` zurückgegeben, statt der obigen Fehlermeldung!

Mit `'` gibt `(list 'if true "Hallo" nil)` nun tatsächlich die gewünschte Form
`(if true "Hallo" nil)` zurück! Zum eigentlichen Makro ist es nun nicht mehr weit:

{% highlight clojure %}
(defmacro my-when
  [pred then]
  (list 'if pred
        then
        nil))
{% endhighlight %}

`pred` und `then` sollen ja ausgewertet werden
(also wie oben im Beispiel `pred` zu `true` und `then` zu `"Hallo"`), deshalb vor ihnen
kein `'`.
`nil` soll eigentlich auch "stehen bleiben", also nicht ausgewertet werden und
müsste normalerweise mit einem `'` versehen sein, da aber `nil` zu `nil` auswertet,
ist dies nicht nötig.

Oben wurde behauptet, dass Makros Quellcode zurückgeben, jetzt wird beim Aufruf von bspw.
`(my-when (= 1 1) "Hallo!")` aber nicht der Quellcode `(if (= 1 1) "Hallo! nil)` zurückgegeben,
sondern `"Hallo"`. Wieso?
Wichtig zu wissen ist, dass bei der Kompilierung von Clojure-Code Makros *vor* der
eigentlichen Evaluierung des restlichen Codes ausgeführt werden
(Makro-Expansionszeit, dazu in einem späteren Beitrag mehr). Danach wird dann ausgewertet
(*auch* der von den Makros produzierte Code!).
Das heißt, dass wir im obigen Beispiel keine Chance hatten,
das Zwischenergebnis des Makros zu sehen,
da die Evaluation unmittelbar nach der Makro-Expansion geschah.
Um zu sehen, was ein Makro zur Expansionszeit zurückgibt, kann man die Funktion
`macroexpand-1` (bzw. `macroexpand`) benutzen:

{% highlight clojure %}
(macroexpand-1 '(my-when (= 1 1)
                         "Hallo!"))
{% endhighlight %}

Dies liefert uns:

{% highlight clojure %}
(if (= 1 1) "Hallo!" nil)
{% endhighlight %}

also genau den Code, den wir abkürzen wollten.
Wenn man also Makros schreiben möchte, ist es sinnvoll, sich im Vorhinein
immer zu überlegen, welchen *Code* man erzeugen möchte.

Als weiteres Beispiel möchten wir nun ein ein Makro schreiben,
das Berechnungen in Infix-Notation akzeptiert.
Das heißt `(calc-infix (2 + 3))` soll `5` ergeben.
Unser Makro `calc-infix` bekommt also eine Form
(Liste mit drei Elementen) und soll darin das erste und zweite Argument vertauschen.

{% highlight clojure %}
(defmacro calc-infix
  [form]
  (list (second form) (first form) (nth form 2)))
{% endhighlight %}

Hier muss kein einziger Ausdruck mit `'` versehen sein.
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
