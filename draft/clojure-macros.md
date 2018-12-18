---
layout: post
description: "Clojure Makros"
title: "Clojure Makros"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "unquote", "backquote", "quasiquote"]
---

*If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.*
- Guy L. Steele

Mit diesem -zugegebenermaßen provokanten- einführenden Zitat möchten wir dem geneigten
Leser das heutige Blogpost-Thema, *Clojure Makros*, näherbringen.
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
eigenständig hinzufügen, und muss nicht darauf hoffen, dass die Sprach-Entwickler
dieses Feature irgendwann einbauen (oder auch nicht).

Zu Anfang ein populäres Beispiel, das einem eine kleine Syntaxerleichterung bringt.

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
wir aber wollen dieses Feature in unserer Sprache benutzen.

Eine Herangehensweise mittels einfacher Funktionsdeklaration könnte folgendermaßen
aussehen

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
Richtig, es wird -trotz `false` als Bedingung- der Satz "Ich sollte nicht geprintet werden"
in die Konsole geschrieben.

Das heißt, unser Ansatz mit einer einfachen Funktion ist hier nicht möglich.
Wieso nicht? In Clojure werden bei Funktionsaufrufen die Parameter automatisch ausgewertet.
Damit sind also gerade Konsequenten (`then`-Zweige)  mit Nebeneffekten problematisch
(Man bedenke nur `(when enemy? (shoot-missiles!))`!).

Makros im Gegensatz zu Funktionen evaluieren ihre Parameter nicht automatisch,
sondern geben sie unevaluiert an ihren Body weiter.

Hier eine funktionierende Version mit `defmacro` statt `defn`
{% highlight clojure %}
(defmacro my-when
  [pred then]
  (if (eval pred)
      then
      nil))
{% endhighlight %}

Nun wird beim Aufruf von

{% highlight clojure %}
(my-when false (println "Ich sollte nicht geprintet werden"))
{% endhighlight %}

nicht mehr in die Konsole gedruckt.
(Tatsächlich passiert dies auch nur deshalb nicht, da `if` seinerseits ein Makro ist,
wäre `if` nur eine Funktion, so wuerde der `println`-Befehl ausgeführt werden.)
Die Notwendigkeit des zusätlichen `eval`s, wird weiter unten erklärt.

Dieses einführende Beispiel hatte nun weniger mit Makros als mit Ausführungsstrategien
zu tun. Nachfolgend lüften wir das Geheimnis,
das hinter dem geflügelten Wort *"Programme schreiben Programme"* steckt.


## Code is Data - Data is Code

Die bei vielen Anfängern (und vor allem Programmiersprachenwechslern) oft
gewöhnungsbedürftige Syntax einer Lisp-Sprache, macht das möglich,
was "code is data, data is code" aussagt.

Lisp-Code wird in Listen ausgedrückt, welche in Lisp wiederum Daten entsprechen.
Präziser gesagt ist die *externe Darstellung* des Codes eine Teilmenge der
externen Darstellung der Data.

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
`quote` (um genau zu sein, ist `quote` eine sogenannte
[*special form*](https://clojure.org/reference/special_forms))
gibt ihren übergebenen Parameter unevaluiert zurück.

Mit `quote` können wir also Daten erstellen, die Code repräsentieren.
Doch was tun mit einer "Liste von Code"?

Hier kommt die Funktion `eval` ins Spiel. Sie evaluiert den übergebenen Parameter.

{% highlight clojure %}
(eval '(+ 5 7))
{% endhighlight %}

liefert demnach `12`.

Warum benötigt man Das Hochkomma `'` überhaupt, und konstruiert nicht einfach eine
Liste mithilfe des Listenkonstruktors `list`?
Also `(list + 5 7)`?

Sie haben es bestimmt schon herausgefunden. Der Listenkonstruktor `list` liefert
als Rückgabewert zwar eine Liste, *evaluiert* aber die Parameter. Deshalb evaluiert
`(list + 5 7)` auch nicht zu `(+ 5 7)` sondern zu `(#function[clojure.core/+] 5 7)`.
Da `5` und `7` *Atome* sind, werden sie "zu sich selbst" evaluiert, nur am `+` sieht man
die Evaluation, das *Symbol* `+` wurde evaluiert zu `clojure.core/+`.

Noch offensichtlicher ist der Unterschied bei den folgenden beiden Codeschnipseln:
{% highlight clojure %}
(list (println "println in der Liste") 5 7)
;; evaluiert zu (nil 5 7) und printet "println in der Liste" in die Konsole

'((println "println in der Liste") 5 7)
;; evaluiert zu ((println "println in der Liste") 5 7) ohne Konsolenprint.
{% endhighlight %}

*Symbole* haben in Makros eine entscheidende Rolle. Sie nicht etwa vom Typ `string`,
was man zunächst vermuten könnte, sondern eigenständige Datentypen,
die es so nur in wenig anderen Sprachen gibt.

Man kann Symbole durch `quote` erzeugen, z.B. liefert `(quote ich-bin-ein-symbol)`
das Symbol `ich-bin-ein-symbol`.
Hingegen würde `ich-bin-ein-symbol` ohne `quote` einen Fehler erzeugen, da Clojure versucht,
das Symbol zu evaluieren, es aber noch nicht gebunden wurde (z.B. durch ein `def`).

Folgendermaßen wäre es also auch möglich, mit dem Listenkonstruktor `list` den Wert
`(+ 5 7)` zu erhalten

{% highlight clojure %}
(list '+ 5 7)
{% endhighlight %}

Nun wird vielleicht auch klar, wieso wir oben, bei der Definition des `my-when`-Makros,
die Bedingung (`pred`) mit einem `eval` ausstatten mussten. Nehmen wir an `pred`
wäre `(= 2 1)`. Mit `eval` wird das zu `false` und `if` wird erwartungsgemäß ausgeführt.
Ohne `eval` jedoch wird *die Liste* `(= 2 1)` als "truthy"-Wert
(da alles "truthy" außer `nil` und `false` ist) im `if` interpretiert, und somit
fälschlicherweise der `then`-Zweig zurückgegeben.

Das ganze `my-when`-Makro können wir mit unserem neuen Wissen nun so schreiben:
{% highlight clojure %}
(defmacro my-when
  [pred then]
  (list 'if pred then nil))
{% endhighlight %}


## Das Record-Makro

Clojure bietet einem mit dem Typenkonstruktor `defrecord` die Möglichkeit,
sogenannte *zusammengesetzte Daten* strukturiert zu erstellen.
Nach `(defrecord Computer [cpu ram])` können wir
(mithilfe des dadurch zur Verfügung gestellten Record-Konstruktors `->Computer`)
Computer-Records erstellen, die zwei Felder besitzen, `cpu` und `ram`. z.B.

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
eine Funktion (oben war es `->Computer`) erstellt,
mit welcher man die tatsächlichen Records also erzeugen kann.

Ein erster Ansatz ohne Makro und nur mit einem Feld:

{% highlight clojure %}
(defn def-my-record
  [type-name field]
  (defn type-name [arg] {field arg}))
{% endhighlight %}

Dadurch ergeben sich einige Unschönheiten:
Wir können den Typkonstruktor `def-my-record` nicht so wie `defrecord` aufrufen,
`(def-my-record Computer ram)` würde einen Fehler werfen,
"Unable to resolve symbol: Computer",
da `Computer` an nichts gebunden ist (gleichermaßen mit `ram`).
Wir können den Aufruf verbessern zu `(def-my-record 'Computer 'ram)`, dies läuft durch.
Einen Record auf die gewollte Weise erstellen wird aber immer noch nicht funktionieren!
`(Computer 16)` wirft denselben Fehler, das Symbol `Computer` ist an keinen Wert gebunden.
Wieso nicht? `def-my-record` erstellt eine Funktion namens `type-name`
(also *wirklich* `type-name`) und nicht eine Funktion namens `Computer`,
da `defn` den übergebenen Parameter `type-name` ignoriert.
Das ist fatal, denn nun würde jeder neue Record-Typ die Konstruktorfunktion `type-name`
der vorherigen überschreiben.

Wie können wir die Evaluation von `defn` aufhalten? Richtig, mit Hochkomma bzw. `quote`.
Wir bauen uns den Funktionsausdruck also zusammen mit `list` und `quote`s:

{% highlight clojure %}
(defn def-my-record
  [type-name field]
  (list 'defn type-name '[arg] {field 'arg}))
{% endhighlight %}

Nur `type-name` und `field` müssen nicht gequotet sein, da wir ja gerade diese beiden
Parameter übernehmen wollen. Der Aufruf von `def-my-record`
(bspw. mit den Parametern `'Computer` und `'ram`) liefert nun natürlich zunächst
nur `(defn Computer [arg] {ram arg})` zurück,
da es tatsächlich *unausgewerteter* Code ist (also *Data*). Um diese auch benutzen zu können,
muss der Ausdruck noch evaluiert werden.

Das ist nicht zufriedenstellend, wir müssen `eval` benutzen,
was man generell vermeiden sollte,
und müssen zusätzlich den Typnamen quoten sowie den Feldnamen
als Keyword angeben. (Zudem wird der Record-Konstruktor hier auch erst zur *Laufzeit*
erstellt und nicht wie bei Makros bereits zur *Kompilierzeit*. Dazu in einem weiteren
Blogpost mehr.)

Jetzt die Makro-Version

{% highlight clojure %}
(defmacro def-my-record
  [type-name field]
  (list 'defn type-name '[arg] {(keyword field) 'arg}))
{% endhighlight %}

Hiermit ist es möglich, einen Recordtyp "Computer" via `(Computer ram)` zu erstellen!
Einen tatsächlichen Record bekommen wir mit `(Computer 2)` und die `2` erhalten wir
auch zurück via Keywordzugriff `(:ram (Computer 2))`.
Es stört noch, dass der Recordkonstruktor denselben Namen wie der Recordtyp hat.
Besser wäre es, einen Record z.B. durch `(make-Computer 3)` zu erstellen.
Wir ersetzen also `type-name` mit `(symbol (str "make-" type-name))`.

Um die Records auch sinnvoll benutzbar zu machen, benötigt man noch eine Möglichkeit
zur Überprüfung, ob ein Record eine Instanz eines bestimmten Typs ist.
Clojure-Records machen das über `(instance? Computer my-computer)`.
Unser Implementierung fügen wir einen eindeutigen Typ hinzu und erstellen eine neue Funktion,
das *Typ-Prädikat*, sodass mit `(Computer? my-computer)` diese Überprüfung durchgeführt
werden kann.

{% highlight clojure %}
(defmacro def-my-record
  [type-name field]
  (list 'do
        (list 'defn (symbol (str "make-" type-name)) '[arg] {(keyword field) 'arg
                                                             :__type__ (str type-name)})
        (list 'defn (symbol (str type-name "?")) '[el] (list '= (str type-name)
                                                             '(:__type__ el)))))
{% endhighlight %}

Das Makro soll einen Ausdruck liefern, der (wenn wir ihn von Hand für das Computer-Beispiel
schreiben würden) so aussehen soll `(defn Computer? [el] (= "Computer" (:__type__ el)))`.
Deshalb ist klar,
dass im Body der Funktion nicht einfach `(= (str type-name) (:__type__ el))` stehen darf
(das würde sonst schon ausgewertet werden und resultierte in einem Fehler,
da `type-name` ungebunden ist), sondern *die Liste*, die diesen Ausdruck enthält.

Wir benötigen das zusätzliche `do` um die beiden Funktionsdefinitionen herum,
weil das Makro ansonsten zur Makro-Expansionszeit
zwar beide Ausdrücke berechnen, aber nur den Letzten zurückgeben würde.

## Fazit und Ausblick

Mit Makros können wir auf elegante Art und Weise unsere Programmiersprache individuell
an gegebene Problemstellungen anpassen.
Die Notwendigkeit und Wichtigkeit von Makros wurde in diesem ersten Blogpost anhand eines
Bedürfnisses (Spracherweiterung durch Record-Typen) dargestellt und
die Handhabung von Makros über die stückweise Entwicklung des Record-Typen nähergebracht.

In einem weiteren Blogpost werden wir auf das sogenannte *Back-* oder *Syntax-Quote*, den *Unquote-* und *Unquote-Splicing-*Operator eingehen.
Zudem wollen wir die eigentliche Funktionsweise von Makros und den Unterschied zwischen
Kompilier- und Laufzeit näher beleuchten
und nebenher unser Record-Makro (das bisher nur ein Feld erlaubt) erweitern.

<!-- more end -->
