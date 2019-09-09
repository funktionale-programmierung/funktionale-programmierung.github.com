---
layout: post
description: "Clojure-Makros"
title: "Makros in Clojure - 2"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "syntaxquote", "gensym"]
---

Dieser Blogpost ist eine Fortführung von [Makros in
Clojure](https://funktionale-programmierung.de/2019/01/30/clojure-macros.html).
Wir werden weitere Makro-Begriffe, wie zum Beispiel das *Syntax-Quote*,
kennenlernen und uns mit Makro-Hygiene beschäftigen. Dies wird es uns
erleichtern, auch komplexere Makros fehlerfrei zu schreiben. Es empfiehlt sich,
den vorherigen Beitrag gelesen zu haben.

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*


## Apostroph (Quote) und Syntax-Quote

Im [vorherigen
Blogpost](https://funktionale-programmierung.de/2019/01/30/clojure-macros.html)
dieser Reihe haben wir bereits das Apostroph `'` kennengelernt. Das wurde
benutzt, um Symbole zu erzeugen. Zum Beispiel gibt `'if` nach Auswertung das
Symbol `if` zurück. Doch `'` kann noch mehr als bisher beschrieben: `'` ist
syntaktischer Zucker für die Funktion `quote`. `quote` ist eine sogenannte
[*special form*](https://clojure.org/reference/special_forms)). Sie gibt ihren
übergebenen Parameter *unausgewertet* zurück. `(quote (+ 1 2))` gibt demnach die
*Liste* `(+ 1 2)` mit dem Symbol `+` und den beiden Zahlen `1` und `2` zurück,
nicht den Wert `3`. Statt `(list 'if true "Hallo" nil)` hätten wir also im
vorherigen Blogpost auch `'(if true "Hallo" nil)` schreiben können.

Doch unser `my-when`-Makro würde mit Apostroph

```clojure
(defmacro my-when
  [pred then]
  '(if pred
       then
       nil))
```

nicht funktionieren. Da `quote` den übergebenen Parameter *unausgewertet*
zurückgibt, wird beispielsweise der Ausdruck `(my-when (= 1 1) "Hallo")` nach
Makro-Expansion zu `(if pred then nil)`. Dabei sind `pred` und `then` lediglich
Symbole und nicht die von uns übergebenen Werte `(= 1 1)` und `"Hallo"`. Nun
aber existieren für die Symbole `pred` und `then` zur *Laufzeit* (vermutlich)
keine Bindungen, weshalb der Compiler eine Fehlermeldung wirft. Wir möchten
also, dass im Rumpf des Makros `pred` durch den übergebenen Wert ersetzt wird.
Dies ist in einem mit `'` versehenen Ausdruck nicht möglich. Dafür gibt es das
*Syntax-Quote* `` ` ``. Innerhalb eines mit `` ` `` versehenen Ausdrucks ist es
möglich, einzelne Ausdrücke ersetzen zu lassen. Dies macht der Befehl `unquote`,
für den es den syntaktischen Zucker `~` gibt. Hier ein Beispiel:

```clojure
;; Normal gequotet
'(1 2 (+ 1 2) (+ 2 2))
;; => (1 2 (+ 1 2) (+ 2 2))

;; syntax-gequotet mit unquote
`(1 2 ~(+ 1 2) (+ 2 2))
;; => '(1 2 3 (clojure.core/+ 2 2))
```

Wir sehen also, dass im Syntax-Quote-Beispiel der Ausdruck `(+ 1 2)` ausgewertet
wurde. Doch es ist noch mehr passiert: Syntax-Quote stellt die Bindung der
Symbole im aktuellen Kontext her und gibt ein voll-qualifiziertes Symbol zurück,
deshalb auch `clojure.core/+` statt einfach nur `+`. Mit Syntax-Quote können wir
nun das `my-when`-Makro wie gewünscht schreiben:

```clojure
(defmacro my-when
  [pred then]
  `(if ~pred
       ~then
       nil))
```

## Unquote-Splicing

Im Rumpf des in Clojure eingebauten `when` dürfen mehrere Ausdrücke
hintereinander stehen. Um diese Funktionalität wollen wir nun unser
`my-when`-Makro erweitern. Das heißt zunächst einmal, dass die Parameterliste
von `my-when` angepasst werden muss: Statt zwei fixen Parametern `pred` und
`then` darf `my-when` nun eine variable Anzahl an Parametern konsumieren: `[pred
& thens]`. Wie schon im vorherigen Blogpost erwähnt, ist es sinnvoll, sich zu
überlegen, welcher Quellcode vom Makro erzeugt werden soll. Da wir in `my-when`
unterliegend `if` benutzen, müssen wir die Elemente der `thens`-Liste in einen
`do`-Ausdruck platzieren. Der Code

```clojure
(my-when (= 1 1)
  (println "Ich will was ausgeben, bevor ich einen Wert zurückgebe")
  "Hallo")

```

soll also folgenden Code

```clojure
(if (= 1 1)
  (do (println "Ich will was ausgeben, bevor ich einen Wert zurückgebe")
      "Hallo")
  nil)
```

produzieren. Im `my-when`-Makro `~then` einfach durch `(do ~thens)` zu ersetzen
wäre falsch, denn daraus würde

```clojure
(do ((println "Ich will was ausgeben, bevor ich einen Wert zurückgebe")
      "Hallo"))
```

werden, da `thens` eine Liste ist. Der obige Ausdruck wirft einen Fehler, denn
das zusätzliche Klammernpaar verursacht einen Funktionsaufruf von `nil`, dem
Rückgabewert von `println`. Wir wollen die Elemente der `thens`-Liste direkt in
den `do`-Ausdruck einspleißen. Dies ermöglicht uns der
*Unquote-Splicing*-Operator `~@`. Zum besseren Verständnis von `~@` hier ein
Beispiel:

```clojure
(let [thens (list 3 4 5)]
  (println `(1 2 ~thens 6 7))   ; => (1 2 (3 4 5) 6 7)
  (println `(1 2 ~@thens 6 7))) ; => (1 2 3 4 5 6 7)
```

Unser erweitertes `my-when`-Makro sieht nun wie folgt aus:

```clojure
(defmacro my-when+ [pred & thens]
  `(if ~pred
     (do ~@thens)
     nil))
```

## Makro-Hygiene

Nachdem wir nun alle essenziellen Befehle über Makros kennengelernt haben,
kommen wir zu einem wichtigen, etwas technischen Thema: Makro-Hygiene.

Das Syntax-Quote ist dem einfachen Quote oft vorzuziehen, da es uns beim
Makro-Schreiben etwas mehr unterstützt, Fehler zu vermeiden. Hier zunächst eine
scheinbar harmlose Definition:

```clojure
(defmacro my-first [lis]
  (list 'first lis))

(my-first ["A" "B" "C"]) ;; => "A"
```

Doch nun benutzt ein anderer Ausdruck `my-first`:

```clojure
(let [first 1]
  (my-first ["A" "B" "C"]))
```

Dies liefert die Exception `java.lang.Long cannot be cast to clojure.lang.IFn`,
da im Rumpf von `let` nun `first` an `1` gebunden ist, und somit der Ausdruck
`(1 ["A" "B" "C"])` entsteht. Wir können den Fehler beheben, indem wir statt des
Apostrophs das Syntax-Quote benutzen:

```clojure
(defmacro my-first [lis]
  `(first ~lis))

(let [first 1]
  (my-first ["A" "B" "C"])) ;; => "A"
```

Warum evaluiert der `let`-Ausdruck nun zu "A"? Wie oben erwähnt, qualifiziert
das Syntax-Quote Symbole. Damit wird das Symbol `first` im Makro zur
Expansionszeit zu `clojure.core/first` und unterscheidet sich somit zur Laufzeit
von dem im `let` gebundenen `first`. Andere Programmiersprachen, wie z.B. Common
Lisp, haben aber auch beim Syntax-Quote das obige Problem (da hier Symbole nicht
vollqualifiziert werden).

Noch ein weiteres Hygiene-Problem sehen wir bei folgendem Code:

```clojure
(defmacro my-identity [a]
  (list 'let ['x 0] a))
```

Die meisten Aufrufe unserer vermeintlichen Identitätsfunktion `my-identity` tun
das gewünschte, nämlich den übergebenen Ausdruck zurückzugeben. Doch was ist mit

```clojure
(let [x "Hallo"]
  (my-macro x))
```

Genau, es wird `0` zurückgegeben und nicht `"Hallo"`, da das äußere `x` vom
inneren überschattet wird. Dieses Phänomen nennt man im Englischen auch
*accidental variable capture*. Auch hier hilft uns Clojure weiter, wenn wir das
Syntax-Quote benutzen:

```clojure
(defmacro my-identity [a]
  `(let [x 0] ~a))

(let [x "Hallo"]
  (my-identity x))
```

Auswerten des zweiten Ausdrucks resultiert in einer Exception:

```clojure
Can't let qualified name: namespace.main/x
```

Clojures `let`-Form akzeptiert in den Bindungen keine qualifizierten Symbole. Da
das Syntax-Quote alle Symbole vollqualifiziert, wird während der Expansion `[x
0]` zu `[namespace.core/x 0]`.

Doch was tun wir, wenn wir einen `let`-Ausdruck in unserem Makro benutzen
wollen? Eine erste Idee wäre, sich im Makro ungewöhnliche Bezeichner für diese
Bindungen auszudenken, also statt `x` `x12915` zu wählen. Doch zufällig könnte
es dennoch zu Namenskollisionen kommen. Clojure (und andere Lisp-Dialekte) lösen
das mit der Funktion `gensym`, die ein global eindeutiges Symbol zurückgibt:

```clojure
(gensym) ;; => G__33881
(gensym "hallo") ;; => hallo33885
```

Damit können wir *während* der Makro-Expansionszeit ein eindeutiges Symbol
erzeugen, und das in der `let`-Form benutzen:

```clojure
(defmacro my-identity-2 [a]
  (let [sym (gensym)]
    `(let [~sym 0] ~a)))
```

Da dieses Konstrukt doch häufig beim Makro-Schreiben vorkommt, hat Clojure eine
Funktionalität eingebaut, die das abkürzt. Wir können statt Obigem auch
schreiben:

```clojure
(defmacro my-identity-2 [a]
  `(let [sym# 0] ~a))
```

`#` weist hier den Clojure-Compiler an, das Symbol automatisch, eindeutig zu
generieren.

## Ungewollte mehrfache Auswertung

Warum wir unbedingt die `let`-Bindung innerhalb eines Makros benötigen, zeigt
die dritte Fehlerquelle beim Makros-Schreiben: *(ungewollte) mehrfache
Evaluation*.

Das folgende Makro ist nicht unbedingt sinnvoll, aber zur Illustration gut
geeignet:

```clojure
(defmacro berechne [x]
  `(if ~x
     (str "Die Berechnungen haben geklappt! Das Ergebnis ist" ~x)
     (str "Fehler bei der Berechnung.")))
```

Angenommen, wir haben einen Ausdruck, der einen Seiteneffekt ausführt (zum
Beispiel einen Datenbankschreibzugriff). Diesen Ausdruck dem `berechne`-Makro zu
übergeben würde dazu führen, dass der Seiteneffekt zweimal ausgeführt werden
würde! Um das zu vermeiden, müssen wir das Ergebnis von `~x` an ein Symbol
binden:

```clojure
(defmacro berechne [x]
  `(let [result# ~x]
     (if result#
       (str "Die Berechnungen haben geklappt! Das Ergebnis ist" result#)
       (str "Fehler bei der Berechnung."))))
```

Tatsächlich muss hier `result#` nicht mehr mit einer Tilde (`~`) versehen
werden, der Clojure-Compiler kümmert sich darum.


## Fazit

In diesem Blogpost haben wir weitere, wichtige Makro-Befehle kennengelernt. Das
Syntax-Quote erleichtert uns nicht nur die Schreibweise von Makros. Durch das
Benutzen des Syntax-Quotes vermeiden wir einige Stolpersteine beim
Makro-Schreiben.

Für die Zukunft dürfen wir uns auf einen weiteren Blogpost freuen, welcher
Anwendungsbeispiele von Makros in Clojure behandeln wird.

<!-- more end -->
