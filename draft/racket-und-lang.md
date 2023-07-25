---
layout: post
title: "Racket und #lang"
author: kaan-sahin
tags: ["racket", "lisp", "makro", "dsl"]
---

Racket steht -- wie allen Lisp-Dialekten -- das mächtige Werkzeug
*Makros* zur Verfügung. Mit ihnen können wir die Sprache erweitern und
an unsere Bedürfnisse anpassen. Dabei sind nicht nur einfache
syntaktische Konstrukte möglich, nein, ganze Typsysteme (wie zum
Beispiel "Typed Racket") oder Concurrency-Systeme ("core.async" aus
Clojure) können damit implementiert werden.

Ein weiterer Aspekt sind eingebettete domänenspezifische Sprachen
(eDSL): mithilfe von Makros kann eine präzise, den Fachbegriffen der
Domäne entsprechende Sprache mit angepasster Syntax, entwickelt
werden.

Racket geht den Weg der DSLs noch ein Stück weiter: Hier können wir
die DSLs ganz ohne Racket-Code drum herum verwenden; wie aus einer
eDSL eine DSL wird schauen wir uns heute an!

<!-- more start -->

# Racket

Zu Racket wurde auf diesem Blog schon einiges geschrieben, u. a.:

- [Eine kleine Einführung in die rein funktionale Programmierung](https://funktionale-programmierung.de/2013/03/12/rein-funktional.html)
- [Eine Monade für Wahrscheinlichkeitsverteilungen](https://funktionale-programmierung.de/2014/04/10/probability-monad.html)
- [Randomisierte Tests mit QuickCheck](https://funktionale-programmierung.de/2013/07/10/randomisierte-tests-mit-quickcheck.html)

Deshalb verzichten wir an dieser Stelle auf eine (syntaktische)
Einführung.

# Makros und `define-syntax`

Zu Makros wurde in diesem Blog auch schon einiges geschrieben (z. B.
[Makros in
Clojure](https://funktionale-programmierung.de/2019/01/30/clojure-macros.html)),
weshalb wir uns hier auch kurz fassen.

Mit Makros lässt sich der Quellcode vor der eigentlichen Evaluation
anpassen -- mit allen Sprachmitteln der eigentlichen Sprache. In Lisps
ist dies besonders einfach, da der Programmcode wie Lisp-Listen
aufgebaut ist und dementsprechend wie Listen bearbeitet werden kann.

In Racket erstellen wir Makros mit dem Spezialkonstrukt
`define-syntax`. Folgend schreiben wir das kleine Makro `infix`, das
uns die in Racket eigentlich ungültige Infixschreibweise `(2 + 3)`
ermöglicht, via `(infix (2 + 3))`:

```racket
(define-syntax (infix form)
   (syntax-parse form
     [(infix (zahl1 op zahl2))
     ...]
     ...))
```

`define-syntax` bekommt als erstes Argument den Namen des Makros und
 eine sogenannte Form. Diese Form ist der komplette Aufrufwert `(infix
 (2 + 3))`, also eine Liste mit zwei Elementen. Im Rumpf benutzen wir
 das Makro `syntax-parse`, das zahlreiche Erleichterungen für das
 Makroschreiben bereitstellt. Unter anderem kann die Form via
 Pattern-Mattching komfortabel zerlegt werden, sodass wir die
 einzelnen Elemente nicht umständlich mit `first` und `rest`
 herausholen müssen. `syntax-parse` erlaubt mehrere Patterns auf
 `form`, wir benötigen aber nur das eine, und schreiben nun noch, wie
 der Aufruf `(infix (2 + 3))` transformiert werden soll:
 
```racket
(define-syntax (infix form)
   (syntax-parse form
     [(infix (zahl1 op zahl2))
      #`(op zahl1 zahl2))))
```
 
Wir tauschen einfach die zwei ersten Elemente! Unbekannt ist noch
``#```. Mit ``#``` (sprich: hash quasiquote) können wir ein Syntax Object
erstellen. In Racket sind die Forms bei der Makroexpansionszeit nicht
bare Listen, sie sind in Syntax Objects gewrappt. Ein Syntax Object
hält Informationen zum Kontext des Makro-Aufrufes bereit, unter
anderem in welchem Modul und in welcher Zeile dieser Aufruf geschehen
ist. Beispielsweise erhalten wir hier

```racket
infix.rkt> #`"Hallo"
#<syntax:infix.rkt:3:2 "Hallo">
```

ein Syntax Object mit der Information, dass der Aufruf von `#"Hallo"`
(aus der REPL) im Namespace `infix.rkt` in Zeile 3 geschehen ist.

# Eine Datenbank-DSL

Wir wollen nun eine kleine Datenbank-DSL in Racket realisieren. Über
(eingebettete) DSLs wurde im Blog auch schon geschrieben:

- [DSLs ganz einfach mit Clojure](https://funktionale-programmierung.de/2021/05/03/clojure-dsl.html)
- [Systematisch eingebettete DSLs entwickeln in Clojure](https://funktionale-programmierung.de/2013/06/27/dsl-clojure.html)

An dieser Stelle gehen wir einen Schritt weiter und verlassen Racket
und dessen Syntax augenscheinlich komplett, und schreiben eine
"Stand-Alone DSL", die folgende Syntax erlaubt:

```
SHOW-DB
PUT "milk" 1.50
PUT "water" 1.00
x = GET "water"
PRINT x
SHOW-DB
```

`SHOW-DB` soll hier den aktuellen Datenbankstatus ausdrucken; `PUT`
ein Key-Value-Paar abspeichern; `GET` ein Value zu gegebenen Key aus
der Datenbank holen und an eine Variable binden; `PRINT` einen Wert
ausdrucken.

Vorbereitend einige Hilfsfunktionen für unsere Datenbank, die der
Einfachheit halber eine Hashmap ist:

```racket
(define the-db (make-hash))

(define (get-it key)
  (hash-ref the-db key))

(define (put-it key val)
  (hash-set! the-db key val))

(define (print-it x)
  (display (format "~v" x))
  (newline))
```

Zunächst schreiben wir eine eingebettete DSL, die nur einzelne Befehle
akzeptiert:

```racket
(db (PRINT "Hallo"))

(db (PUT "kaan" 35))

(db (x = GET "kaan))
```

Wie oben eingeführt, benutzen wir dafür `syntax-parse` und haben hier
vier Patterns:

```racket
(define-syntax (db form)
  (syntax-parse form
    [(db (SHOW-DB))
     ...]
    [(db (PRINT x))
     ...]
    [(db (PUT name val))
     ...]
    [(db (var = GET name))
     ...]))
```

Nun müssen nur noch die Ellipsen ausgefüllt werden, mit dem Code, der
das jeweilige Pattern in der Makroexpansionszeit ersetzen soll. Das
ist für die ersten drei Fälle denkbar einfach, wir benutzen unsere
obig definierten Hilfsfunktionen (und achten darauf, Syntax Objects
zurückzugeben):

```racket
(define-syntax (db form)
  (syntax-parse form
    [(db (SHOW-DB))
     #`(print-it the-db)]
    [(db (PRINT x))
     #`(print-it x)]
    [(db (PUT name val))
     #`(put-it name val)]
    [(db (var = GET name))
     #`(define var (get-it name))]))
```

Im vierten Fall erzeugen wir ein Syntax Object, das das übergebene
Symbol `var` an das Value bindet.

Jetzt können wir bereits Folgendes machen:

```racket
db.rkt> (db (SHOW-DB))
'#hash()
db.rkt> (db (PUT "kaan" 35))
db.rkt> (db (SHOW-DB))
'#hash(("kaan" . 35))
db.rkt> age
; age: undefined;
;  cannot reference an identifier before its definition
db.rkt> (db (age = GET "kaan"))
db.rkt> age
35
```

Nun wollen wir noch realisieren, dass das Makro mehrere dieser
"Statements" auf einmal verarbeiten kann.

Das fertige Makro sieht so aus:

```racket
(define-syntax (db form)
  (syntax-parse form
    [(db statement ...)
     #`(begin
         #,@(map (lambda (statement)
                   (syntax-parse (datum->syntax #`db statement)
                                 [(SHOW-DB)
                                  #`(begin
                                      (display "THE DB: ")
                                      (print-it the-db))]
                                 [(PRINT x)
                                  #`(print-it x)]
                                 [(PUT name val)
                                  #`(put-it name val)]
                                 [(var = GET name)
                                  #`(define var (get-it name))]))
                 (syntax->list #'(statement ...))))]))
```

Mit `...` im `syntax-parse`-Makro beim Pattern-Matching können wir
angeben, dass sich die letzte Variable beliebig oft wiederholen kann.
Im Weiteren nehmen wir uns diese beliebig lange `statement`-Liste her
und iterieren über sie mit unserem schon von oben bekannten Code.

Damit können wir nun folgenden Ausdruck in unserer eDSL schreiben:

```racket
(db (SHOW-DB)
    (PUT "milk" 1.50)
    (PUT "water" 1.00)
    (x = GET "water")
    (PRINT x)
    (SHOW-DB))
```

Dies ausgewertet druckt aus:

```racket
THE DB: '#hash()
1.0
THE DB: '#hash(("milk" . 1.5) ("water" . 1.0))
```

Wie können wir den obigen Code nun völlig ohne Klammern schreiben? Ein
Makro ist dazu nicht in der Lage, schließlich muss auch hier mit einer
öffnenden Klammer gestartet werden. Das muss also vorher passieren.

Racket gibt einem die Möglichkeit, den Parser (der sogenannte
*Reader*) zu beeinflussen. Genau das machen wir uns nun zu Nutze.

# Ein eigener Reader

Um den Text

```
SHOW-DB
PUT "milk" 1.50
PUT "water" 1.00
x = GET "water"
PRINT x
SHOW-DB
```

für Racket verständlich zu machen, müssen folgende drei Schritte
passieren:

1. der Text landet in einer Datei, die mit `#lang reader
   "db-reader.rkt"` beginnt
2. jede Zeile wird geklammert
3. alle Zeilen werden in einen `(db ...)`-Aufruf eingeschlossen

Schritt 1 weist Racket an, einen eigens definierten Reader, der in der
Datei `db-reader.rkt` definiert ist, zu benutzen.

Wir legen also die Datei `db-reader.rkt` an, die den Quellcode nach
dem Einlesen, vor Makroexpansion und Evaluation, verändern kann. Sie
beginnt so:

```racket
#lang racket
(provide (rename-out (db-read-syntax read-syntax)))

(define (db-read-syntax src in)
  (datum->syntax
   #f
   `(module db racket
      (require "db.rkt")
      (db
       ,@(parse-program src in)))))
```

Wir nennen unseren neuen Reader `db-read-syntax` und fangen mit
Schritt 3 an, setzen also um das gesamte geparste Programm (den Text
oben) erst einmal `(db ...)`. Darüber packen wir alles in ein Modul,
so sehen unter der Haube alle Racket-Quelltexte aus. Und zu guter
Letzt laden wir noch die Datei `db.rkt`, in der unser obiges Makro
definiert ist.

`parse-program` macht nun nicht viel anderes, als jede Zeile in
Klammern zu setzen:

```racket
(define (parse-program src in)
  (define line (parse-line src in))
  (if (eof-object? line)
      '()
      (cons line (parse-program src in))))

(define (parse-line src in)
  (regexp-try-match #px"^\\s+" in)
  (if (eof-object? (peek-char in))
      eof
      (read (open-input-string (string-append "(" (read-line in) ")")))))
```

In `parse-line` lesen wir eine Zeile via `(read-line in)`, setzen
Klammern und lesen dann mit dem "normalen" Reader (`read`) diese Zeile
ein.

Damit haben wir es geschafft! Unsere obigen Statements können als
"Stand-Alone DSL" ausgeführt werden!

# Fazit

Mit Racket steht uns ein mächtiges Werkzeug zur Erstellung von eDSLs
und DSLs zur Verfügung. Rackets Makrosystem, und die vielen
ausdruckstarken Hilfsfunktionen und -Makros, machen das Makroschreiben
zu einer leichten Fingerübung. Die Möglichkeit, den Reader anzupassen,
und damit via `#lang` eine eigenständige Sprache zu schreiben, sucht
seinesgleichen.
