---
layout: post
description: "Clojure Makros"
title: "Makros in Clojure - 2"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "syntaxquote", "backquote"]
---

Dieser Blogpost ist eine Fortführung von [Makros in
Clojure](https://funktionale-programmierung.de/2019/01/30/clojure-macros.html).
Wir werden weitere Makro-Begriffe, wie zum Beispiel das *Syntax-Quote*,
kennenlernen und unser eigenes Record-Makro erstellen. Es empfiehlt sich, den
vorherigen Beitrag gelesen zu haben.

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*


## Das Record-Makro

Clojure bietet uns mit dem Typkonstruktor `defrecord` die Möglichkeit,
sogenannte *zusammengesetzte Daten* strukturiert zu erstellen.
Nach `(defrecord Computer [cpu ram])` können wir
(mithilfe des dadurch zur Verfügung gestellten Record-Konstruktors `->Computer`)
Computer-Records erstellen, die zwei Felder besitzen, `cpu` und `ram`. Z.B.:

```clojure
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16))
```

Nun ist es noch möglich, via *Keywords* auf die einzelnen Felder
zuzugreifen, beispielsweise liefert `(:ram office-pc)` den Wert `2`.

Wir werden in diesem Blogpost unser eigenes `defrecord` mithilfe von Records
implementieren. Als unterliegende Struktur des Records verwenden wir eine
Clojure-*Hashmap*. Wir brauchen einen Typkonstruktor `def-my-record`, der uns zu
einem gegebenen Namen eine Funktion (oben war es `->Computer`, wir nehmen
`->>Computer`) erstellt, mit welcher man die tatsächlichen Records erzeugen
kann. (Zunächst wird unser Typkonstruktor nur Rekordtypen mit einem Feld
erlauben.)


<!-- more end -->
