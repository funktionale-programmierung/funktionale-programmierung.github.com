---
layout: post
description: "Clojure Makros"
title: "Clojure Makros"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "unquote", "backquote", "quasiquote"]
---

*If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.*
- Guy L. Steele

Mit diesem - zugegebenermaßen provokanten - einführenden Zitat möchten wir dem geneigten
Leser das heutige Blogpost-Thema *Clojure Makros* näherbringen.
Die in diesem Zusammenhang oft fallende Floskel *"code is data, data is code"*
wird hierzu eine entscheidende Rolle spielen.
Wir werden "Programme schreiben, die Programme schreiben"!

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*

## Warum *Du* Makros willst

Makros erlauben es, die Programmiersprache auf natürliche Art und Weise zu erweitern.
So kann man Sprachfeatures, die die Sprache selbst nicht besitzt,
eigenständig hinzufügen und muss nicht darauf hoffen, dass die Sprachenentwickler
dieses Feature irgendwann einbauen (oder auch nicht).
Zu Anfang ein populäres Beispiel, das einem eine kleine Syntaxerleichterung bringt:

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

Angenommen, die Clojure-Entwickler hätten 'vergessen', `when` zu implementieren,
wir aber möchten dieses Feature in unserer Sprache benutzen.
Eine Herangehensweise mittels einfacher Funktionsdeklaration könnte folgendermaßen
aussehen:

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
Richtig, es wird - trotz `false` als Bedingung - der Satz "Ich sollte nicht geprintet werden"
in die Konsole geschrieben.

Das heißt, unser Ansatz mit einer einfachen Funktion ist hier nicht möglich.
Wieso nicht? In Clojure werden bei Funktionsaufrufen die Parameter automatisch ausgewertet.
Damit sind gerade Konsequenten (`then`-Zweige)  mit Nebeneffekten problematisch
(Man bedenke nur `(when enemy? (shoot-missiles!))`.)
Makros im Gegensatz zu Funktionen evaluieren ihre Parameter nicht automatisch,
sondern geben sie unevaluiert an ihren Rumpf weiter.

Hier eine (zunächst unkommentierte) funktionierende Version mit `defmacro` statt `defn`:
{% highlight clojure %}
(defmacro my-when
  [pred then]
  (list 'if pred
        then
        nil))
{% endhighlight %}

Nun wird beim Aufruf von

{% highlight clojure %}
(my-when false (println "Ich sollte nicht geprintet werden"))
{% endhighlight %}

nicht mehr in die Konsole gedruckt.

Nachfolgend lüften wir das Geheimnis,
das hinter dem geflügelten Wort *"Programme schreiben Programme"* steckt,
und damit auch wie und warum unser `my-when`-Makro funktioniert.


## Code is Data - Data is Code

Die bei vielen Anfängern (und vor allem Programmiersprachenwechslern) oft
gewöhnungsbedürftige Syntax einer Lisp-Sprache macht das möglich,
was "code is data, data is code" (etwas schwammig) aussagt.
Lisp-Code wird in Listen ausgedrückt, welche in Lisp wiederum Daten entsprechen.
Präziser gesagt ist die *externe Darstellung* des Codes eine Teilmenge der
externen Darstellung der Daten.

Ein Beispiel an dieser Stelle:

{% highlight clojure %}
(+ 5 7)
{% endhighlight %}

evaluiert zu `12`.
Hingegen evaluiert

{% highlight clojure %}
'(+ 5 7)
{% endhighlight %}

zu `(+ 5 7)`, es ist also eine Liste, die drei Elemente, `+`, `5`, `7`,  besitzt.
Das Hochkomma-Zeichen `'` ist syntaktischer Zucker für die Funktion `quote`.
`quote` (um genau zu sein ist `quote` eine sogenannte
[*special form*](https://clojure.org/reference/special_forms))
gibt ihren übergebenen Parameter *unevaluiert* zurück.

Mit `quote` können wir also Daten erstellen, die Code repräsentieren.
Doch was tun mit einer "Liste von Code"? Das sehen wir folgend.

## Makros

Um Makros (bzw. ihre Benutzung) besser zu verstehen, ist es sinnvoll,
sie von Funktionen abzugrenzen.
Funktionen erhalten einen Wert berechnen damit ggf. etwas und geben dann einen Wert zurück.
Makros hingegen erhalten *Quellcode*, manipulieren diesen ggf. und geben *Quellcode* zurück
(wir sagen dazu ab jetzt auch *(unausgewertete) Formen*).

Zurück zu unserem `my-when`-Beispiel: Wieso wird beim Aufruf von bspw.
`(my-when (= 1 1) "Hallo!")` kein Quellcode zurückgegeben, sondern "Hallo"?
Wichtig zu wissen ist, dass bei der Kompilierung von Clojure-Code Makros *vor* der
eigentlichen Evaluierung des restlichen Codes ausgeführt werden
(Makro-Expansionszeit, dazu in einem späteren Beitrag mehr). Danach wird dann evaluiert
(*auch* der von den Makros produzierte Code!).
Das heißt, wir hatten im obigen Beispiel keine Chance,
das Zwischenergebnis des Makros zu sehen,
da die Evaluation unmittelbar nach der Makro-Expansion geschah.
Um zu sehen, was ein Makro zur Expansionszeit zurückgibt, kann man die Funktion
`macroexpand-1` (bzw. `macroexpand`) benutzen:

{% highlight clojure %}
(macroexpand-1 '(my-when (= 1 1)
                         "Hallo!"))
{% endhighlight %}

Dies liefert uns

{% highlight clojure %}
(if (= 1 1) Hallo! nil)
{% endhighlight %}

also genau den Code, den wir abkürzen wollten.
Wenn man also Makros schreiben möchte, ist es sinnvoll, sich im Vorhinein
immer zu überlegen, welchen *Code* man erzeugen möchte.

Bei `my-when` wollten wir eine *Liste* zurückgeben, mit *vier* Elementen,
dem `if`, dem Prädikat `pred`, der Konsequente `then` und das feste `nil`.
Das `if` soll stehenbleiben
(und nicht evaluiert werden, wir haben ja keinen Wert an `if` gebunden)
und wird deshalb gequotet, das Prädikat und die Konsequente müssen natürlich evaluiert werden
(wenn wir diese stattdessen quoten würden, stünde nach der Makro-Expansion sonst
`(if pred then nil)` da!). `nil` soll stehenbleiben, also müsste es normalerweise auch
gequotet sein, da aber `nil` zu `nil` evaluiert, ist dies nicht nötig.

Bevor wir zu einem praktischen Beispiel kommen, möchten wir zunächst zur Übung
ein Makro schreiben,
das Berechnungen in Infix-Notation akzeptiert.
Das heißt `(calc-infix (2 + 3))` soll `5` ergeben.
Unser Makro `calc-infix` bekommt also eine Form
(Liste mit drei Elementen) und soll eine Liste zurückgeben, die die Elemente der Form hat,
wobei das erste und das zweite vertauscht sind.

{% highlight clojure %}
(defmacro calc-infix
  [form]
  (list (second form) (first form) (nth form 2)))
{% endhighlight %}

Hier muss kein einziger Ausdruck gequotet sein.
`calc-infix` zeigt die Mächtigkeit von Makros.
Wir können nun Code schreiben, der eine andere Syntax erlaubt! Dies ist gerade für
DSLs (domänenspezifische Sprachen) ein großer Vorteil.

## Das Record-Makro

Clojure bietet uns mit dem Typkonstruktor `defrecord` die Möglichkeit,
sogenannte *zusammengesetzte Daten* strukturiert zu erstellen.
Nach `(defrecord Computer [cpu ram])` können wir
(mithilfe des dadurch zur Verfügung gestellten Record-Konstruktors `->Computer`)
Computer-Records erstellen, die zwei Felder besitzen, `cpu` und `ram`. Z.B.:

{% highlight clojure %}
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16))
{% endhighlight %}

Nun bietet Clojure noch die Möglichkeit, via *Keywords* auf die einzelnen Felder
zuzugreifen, beispielsweise liefert `(:ram office-pc)` den Wert `2`.
Wir nehmen wieder an, in Clojure existierte `defrecord` nicht, verzagen aber nicht,
sondern schreiben es einfach selbst!

Als unterliegende Struktur des Records verwenden wir eine Clojure-*Hashmap*.
Wir brauchen einen Typkonstruktor `def-my-record`, der uns zu einem gegebenen Namen
eine Funktion (oben war es `->Computer`, wir nehmen `->>Computer`) erstellt,
mit welcher man die tatsächlichen Records erzeugen kann.
(Zunächst wird unser Typkonstruktor nur Rekordtypen mit einem Feld erlauben.)

Ein nicht-generischer Konstruktor für Computer-Records lautet:

{% highlight clojure %}
(defn ->>Computer
  [ram-value]
  {:ram ram-value}))
{% endhighlight %}

und wir könnten einen Computer mit 4 GB RAM via `(->>Computer 4)` erzeugen.
Für ein Auto entsprechend:

{% highlight clojure %}
(defn ->>Car
  [color-value]
  {:color color-value}))
{% endhighlight %}


Wir wollen beim Aufruf unseres Makros diese Konstruktoren zu den Typen *Computer*
und *Auto* automatisch erzeugen lassen!
Wir übernehmen den obigen Code, mit ein paar Änderungen, in unseren Makro-Code:

{% highlight clojure %}
(defmacro def-my-record
  [type-name field]
  (list 'defn type-name ['arg] {(keyword field) 'arg}))
{% endhighlight %}

`defn` soll nicht evaluiert werden, `type-name` aber natürlich schon. Das soll ja unser
Konstruktorname (`->>Car` bzw. `->>Computer`) sein, der als Parameter übernommen wird.
(Bisher ist der Konstruktorname aber nur bspw. "Car" statt "->>Car".)
Wenn wir einen validen `defn`-Ausdruck erzeugen wollen, benötigt dieser zum Namen noch
einen Parametervektor und den Rumpf. Der Rumpf besteht aus der Hashmap,
die dem übergebenen Feld `field` das Argument `arg` zuweist
(das Symbol `field` wird zuvor noch in ein Keyword verwandelt).
Wir wollen, dass `arg` nach der Makro-Expansion `arg` bleibt, deshalb das quote.

Hier das Ergebnis der Makro-Expansion:

{% highlight clojure %}
(macroexpand-1 '(def-my-record1 Computer1 ram))
--> (defn Computer1 [arg] {:ram arg})
{% endhighlight %}

Es stört noch, dass der Recordkonstruktor denselben Namen wie der Recordtyp hat.
Wir ersetzen also `type-name` mit `(symbol (str "->>" type-name))`.

Um die Records auch sinnvoll benutzbar zu machen, benötigen wir noch eine Möglichkeit
zur Überprüfung, ob ein Record eine Instanz eines bestimmten Typs ist.
Clojure-Records machen das über `(instance? Computer my-computer)`.
Unser Implementierung fügen wir einen eindeutigen Typ hinzu (hier der Einfachheit halber
der Typname als String) und erstellen eine neue Funktion,
das *Typ-Prädikat*, sodass mit `(Computer? my-computer)` diese Überprüfung durchgeführt
werden kann.

Das Makro soll einen Ausdruck liefern, der (wenn wir ihn von Hand für das Computer-Beispiel
schreiben würden) so aussehen soll:

{% highlight clojure %}
(defn Computer? [el] (= "Computer" (:__type__ el)))
{% endhighlight %}

In unserem Makro-Code nur
{% highlight clojure %}
(list 'defn (symbol (str type-name "?")) '[el] (= (str type-name)
                                                 '(:__type__ el)))
{% endhighlight %}

zu schreiben, wäre falsch, da `(= (str type-name) (:__type__ el))`
sonst bereits ausgewertet werden und immer `false` ergeben würde,
da ein String `(str typ-name)` mit einer Liste `(:__type__ el)` verglichen wird).
Stattdessen möchten wir *die Liste* zurückgeben, die diesen Ausdruck enthält:

{% highlight clojure %}
(defmacro def-my-record
  [type-name field]
  (list 'do
        (list 'defn (symbol (str "->>" type-name)) '[arg] {(keyword field) 'arg
                                                           :__type__ (str type-name)})
        (list 'defn (symbol (str type-name "?")) '[el] (list '= (str type-name)
                                                             '(:__type__ el)))))
{% endhighlight %}

Wir benötigen das zusätzliche `do` um die beiden Funktionsdefinitionen herum,
weil das Makro ansonsten zur Makro-Expansionszeit
zwar beide Ausdrücke berechnen, aber nur den Letzten zurückgeben würde.
Die geschachtelten `list`-Ausdrücke machen das Ganze nun etwas unleserlich.
Dies beheben wir - mithilfe des sogenannten *Syntax-Quotes* - im nächsten Beitrag.

Jetzt können wir verschiedene Record-Typen definieren und mit ihnen arbeiten:

{% highlight clojure %}
(def-my-record Car color)
(def fire-truck (->>Car "red"))
(:color fire-truck)        ; --> "red"
(Car? fire-truck)          ; --> true
{% endhighlight %}

Durch `(macroexpand-1 '(def-my-record Car color))` können wir uns wieder zeigen lassen,
ob unser Makro auch die gewünschten Funktionen erstellt.

## Fazit und Ausblick

Mit Makros können wir auf elegante Art und Weise unsere Programmiersprache individuell
an gegebene Problemstellungen anpassen.
Die Notwendigkeit und Wichtigkeit von Makros wurde in diesem ersten Blogpost anhand eines
Bedürfnisses (Spracherweiterung durch Record-Typen) dargestellt und
die Handhabung von Makros über die stückweise Entwicklung des Record-Typen nähergebracht.

In einem späteren Blogpost werden wir auf das sogenannte *Back-* oder *Syntax-Quote*, den *Unquote-* und *Unquote-Splicing-*Operator und den Unterschied zwischen
*Syntax-Quote* und *Quote* eingehen.
Zudem wollen wir die Funktionsweise von Makros und den Kompilierungsprozess näher beleuchten
und nebenher unser Record-Makro (das bisher nur ein Feld erlaubt) erweitern.

<!-- more end -->
