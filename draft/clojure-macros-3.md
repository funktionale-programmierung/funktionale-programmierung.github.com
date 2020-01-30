---
layout: post
description: "Clojure Makros"
title: "Makros in Clojure - 3 - Das Record-Makro"
author: kaan-sahin
tags: ["Clojure", "Makro", "macro", "defmacro", "quote", "defrecord", "record"]
---

Nachdem wir in den beiden Blogposts [Makros in
Clojure](https://funktionale-programmierung.de/2019/01/30/clojure-macros.html)
und [Makros in Clojure -
2](https://funktionale-programmierung.de/2019/09/17/clojure-macros-2.html) die
für die Praxis relevanten Makro-Befehle kennengelernt haben, widmen wir uns in
diesem Blogpost einem umfangreichen Beispiel zu Makros in Clojure: Wir werden
unser eigenes Record-Makro erstellen! Es empfiehlt sich, die vorherigen Beiträge
gelesen zu haben.

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*

## Records für zusammengesetzte Daten

Immer dann, wenn ein Objekt aus mehreren Teilen besteht, haben wir es mit
zusammengesetzten Daten zu tun. Diese modellieren wir in Clojure mit sogenannten
*Records*. Der Kollege [Michael
Sperber](https://www.active-group.de/teamplayer/05sperber.html) hat dazu bereits
einen
[Blogpost](https://funktionale-programmierung.de/2015/04/27/clojure-records.html)
verfasst. 

Manche Dinge an den Clojure-Records stören jedoch bzw. verleiten zu Fehlern,
weshalb wir heute eine eigene Version schreiben wollen.

### Clojure-Records und die bereits bezahlte Rechnung

Wir modellieren eine Rechnung mit gewöhnlichen Clojure-Records: Eine Rechnung
besteh aus einer ID, einer IBAN, einem zu bezahlenden Betrag und einer
Markierung, die anzeigt, ob eine Rechnung schon beglichen wurde oder nicht:

```clojure
(defrecord Bill [id iban amount paid?])

(def restaurant-bill (->Bill 1 "DEXY XYXY XYXY ..." 48.00 true))
(def hospital-bill (->Bill 2 "DEYX YXYX YXYX ..." 10.00 false))
```

Mit `defrecord` lässt sich der neue Recordtyp `Bill` definieren. Ein
`Bill`-Record wird mit dem Konstruktor `->Bill` und den jeweiligen Werten für
die Felder erzeugt. Auf die einzelnen Bestandteile eines Records kann via
*Keywords* zugegriffen werden: `(:id restaurant-bill)` und `(:amount
hospital-bill)`liefern die Werte `1` und `10.00`.

Um die Unzulänglichkeiten der Clojure-eigenen Records zu demonstrieren,
betrachten wir eine Funktion `bills-to-pay`, die aus einer Liste von Rechnungen
diejenigen entfernt, die schon beglichen worden sind:

```clojure
(defn bills-to-pay
  [bills]
  (remove :paid bills))

(bills-to-pay [restaurant-bill hospital-bill])
;; => (#Bill{:id 1, :iban "DEXY XYXY XYXY ...", :amount 48.0, :paid? true}
;;     #Bill{:id 2, :iban "DEYX YXYX YXYX ...", :amount 10.0, :paid? false})
```

Huch! (oder auch "Ja, holla die Waldfee!!") Wieso wurde die Restaurant-Rechnung
nicht herausgefiltert? Die Rechnung wurde doch schon beglichen! Sie sehen es
bestimmt: Ein Schreibfehler hat sich eingeschlichen; statt `:paid?` wird `:paid`
benutzt. Ein Keywordzugriff auf eine Hashmap, in welcher das Keyword nicht
vorhanden ist, liefert `nil` (näheres dazu z. B.
[hier](https://stackoverflow.com/questions/6915531/why-does-using-keywords-or-symbols-as-functions-to-lookup-values-from-maps-work)).
Das ist ärgerlich; solche Fehler fallen unter Umständen für lange Zeit nicht
auf, da keine Fehlermeldung geworfen sondern ein valider, aber falscher Wert
geliefert wird.

Eine weitere Eigenheit von Clojure-Records kommt zutage, weil sie das
Hashmap-Interface implementieren. Einem Record kann man problemlos weitere Paare
mit `assoc` anfügen. Das allein ist noch nicht fragwürdig, aber dass das
resultierende Ergebnis trotzdem noch denselben Typ von zuvor hat schon:

```clojure
(def rb-2 (assoc restaurant-bill :velocity 100))

rb-2
;; => #...Bill{:id 1, :iban "DEXY XYXY XYXY ...", 
               :amount 48.0, :paid? true, :velocity 100}

(instance? Bill rb-2)       ; => true
```

`rb-2` ist *immer noch* ein `Bill`-Record! Wir wollen robustere Records und
bauen uns deshalb selbst welche!

## Das Record-Makro

Dazu benötigen wir zuallererst einen Recordtypkonstruktor. Clojure nennt seinen
`defrecord`, wir wählen `def-record-type`. Folgende Funktionen müssen von ihm
erzeugt werden:

- ein Recordkonstruktor
- Feld-Selektoren
- Typ-Prädikat

Als Argumente konsumiert er einen Recordtyp-Namen und Recordtypfelder-Namen. Das
Skelett des Makros sieht wie folgt aus:

```clojure
(defmacro def-record-type
  [type-name & field-names]
  ...)
```

Im Rumpf werden die oben beschriebenen Funktionen erzeugt.

### Recordkonstruktor

Wir fangen mit der Implementierung des Recordkonstruktors an. Dieser soll nach
Anwendung auf Feldwert-Argumente einen Record zurückgeben. Als unterliegende
Struktur eines Records verwenden wir der Einfachheit halber eine
Clojure-*Hashmap*. Statt `->type-name` wählen wir `make-type-name` als Namen.

Ein nicht automatisch generierter Konstruktor für Bill-Records könnte so
aussehen:

```clojure
(defn make-bill
  [id iban amount paid?]
  {:id id
   :iban iban
   :amount amount
   :paid? paid?})
```

Jetzt können wir die Restaurant-Rechnung via 

```clojure
(make-bill 1 "DEXY XYXY XYXY ..." 48.00 true)
```

erzeugen. 

Nun zum Makro: Wir müssen also Code schreiben, der uns obigen Ausdruck als
Clojure-Liste zurückgibt. Den Namen der Funktion erhalten wir durch
Konkatenation von `"make-"` und `type-name`. Der Parametervektor ist einfach der
übergebene Feldnamen-Vektor. Schlussendlich erzeugen wir die Hashmap, indem wir
über die Feldnamen-Liste iterieren und Tupel erzeugen:

```clojure
(defmacro def-record-type
  [type-name & field-names]
  `(defn ~(symbol (str "make-" type-name))
     ~(vec field-names)
     ~(into {}
            (map (fn [field-name]
                   [(keyword field-name) field-name])
                 field-names))))
```

Wir können mithilfe von `macroexpand-1` überprüfen, ob tatsächlich der
gewünschte Code erzeugt wird:

```clojure
(macroexpand-1 '(def-record-type-1 bill [id iban amount paid?]))

;; => (clojure.core/defn make-bill
;;                       [id iban amount paid?]
;;                       {:id id, :iban iban, :amount amount, :paid? paid?})
```

Da das Recordtypkonstruktor-Makro noch weitere Funktionen erzeugen wird, lagern
wir, bevor wir fortfahren, das Erzeugen des Recordkonstruktors noch in eine
eigene Funktion aus:

```clojure
(defn create-record-constructor
  [type-name field-names]
  `(defn ~(symbol (str "make-" type-name))
     ~field-names
     ~(into {}
            (map (fn [field-name]
                   [(keyword field-name) field-name])
                 field-names))))

(defmacro def-record-type
  [type-name field-names]
  `(do
     ~(create-record-constructor type-name field-names)))
```

Beim Testen der neuen Recordkonstruktor-Erzeugerfunktion müssen wir darauf
achten, dass diese Symbole konsumiert:

```clojure
(create-record-constructor 'bill ['id 'iban 'amount 'paid?])
```

### Selektoren

Wir können nun eigene Recordtypen und dazugehörige Records erzeugen. Auf die
einzelnen Feldwerte der Records ist es, wie bei den Clojure-`defrecord`s auch,
bereits möglich, mit Keywords zuzugreifen:

```clojure
(:amount (make-bill 1 "DEXY XYXY XYXY ..." 48.00 true))

;; => 48.00

```

Wie oben erläutert, ist der Keywordzugriff etwas problematisch und deshalb
erzeugen wir Selektorfunktionen.

Hier der Code für einen nicht automatisch generierten Bill-Selektor:

```clojure
(defn bill-amount
  [bill]
  (:amount bill))
```

Für jeden Feldnamen muss solch eine Selektorfunktion erzeugt werden. Dazu lagern
wir die Recordselektoren-Erzeugerfunktion wieder aus dem Makro aus:

```clojure
(defn create-record-accessors
  [type-name field-names]
  (map (fn [field-name]
         `(defn ~(symbol (str type-name "-" field-name))
            [~type-name]
            (~(keyword field-name) ~type-name)))
       field-names))
```

In den Recordtypkonstruktor übernommen ergibt das:

```clojure
(defmacro def-record-type
  [type-name field-names]
  `(do
     ~(create-record-constructor type-name field-names)
     ~@(create-record-accessors type-name field-names)))

(def-record-type-2 bill [id iban amount paid?])
(def the-bill (make-bill 1 "DEXY XYXY XYXY ..." 48.00 true))

(bill-paid? the-bill)
;; => true
```

### Typ-Prädikat

Um die Records auch sinnvoll benutzbar zu machen, benötigen wir noch eine
Möglichkeit zur Überprüfung, ob ein Record eine Instanz eines bestimmten Typs
ist. Clojure-Records machen das über `(instance? Bill my-bill)`. Unsere
Implementierung ermöglicht uns das bis jetzt nicht; wir erzeugen schließlich nur
eine einfache Hashmap ohne Typinformation. Deshalb fügen wir den Metadaten der
Hashmap nun das Paar `(:__type__ : type-name)` hinzu.

In `create-record-constructor` müssen wir dazu den `(into {} ...)`-Ausdruck um
einen `vary-meta`-Aufruf erweitern:

```clojure
(vary-meta (into {}
                 (map (fn [field-name]
                        [(keyword field-name) field-name])
                      field-names))
           (fn [m] (assoc m :__type__ `'~type-name)))
```

Das kryptisch anmutende `` `'~type-name `` wird weiter unten
erläutert.

Nun zum *Typ-Prädikat*. Das Typ-Prädikat für `bill` soll eine Funktion `bill?`
sein, sodass `(bill? thing)` im Falle eines Bill-Records `true` und ansonsten
`false` zurückgibt. Ein Objekt ist ein Bill-Record, wenn zwei Bedingungen
erfüllt sind:

1. der Typname ist in den Metainformationen vorhanden
2. Genau die (und nur die) Felder des Recordtypkonstruktors finden sich als
   Keywords in der unterliegenden Hashmap wieder

Punkt 2 zieht eine weitere Bedingung mit sich: Das Objekt muss eine Hashmap
sein. Ein nicht generische Prädikat für einen Bill-Record könnte so aussehen:

```clojure
(defn bill?
    [thing]
    (and
     (= 'bill (:__type__ (meta thing)))
     (map? thing)
     (= (set (map keyword field-names))
        (set (keys thing)))))
```

Wir schreiben also eine Erzeugerfunktion, die zu gegebenen Typnamen eine
Funktion wie oben zurückgibt:

```clojure
(defn create-predicate
  [type-name field-names]
  `(defn ~(symbol (str type-name "?"))
     [~'thing]
     (and
      (= '~type-name (:__type__ (meta ~'thing)))
      (map? ~'thing)
      (= ~(set (map keyword field-names))
         (set (keys ~'thing))))))
```

Etwas befremdlich könnten `~'thing` und `'~type-name` wirken. Warum nicht
einfach `thing` statt `~'thing`? Wie im [vorherigen
Blogpost](https://funktionale-programmierung.de/2019/09/17/clojure-macros-2.html)
bereits erläutert, qualifiziert das Syntax-Quote `` ` `` Symbole. `defn`
akzeptiert in der Parameterliste jedoch keine qualifizierten Symbole. `'~` sorgt
dafür, dass tatsächlich das übergebene Symbol (und nicht `type-name`) dasteht,
zur Laufzeit aber nicht weiter evaluiert wird. Gleiche Begründung gilt bei ``
`'~type-name `` aus `create-record-constructor` von oben. Hier ist noch
zusätzlich das `` ` `` nötig, da zuvor unquotet wurde.

Im Ausdruck `~(set (map keyword field-names))` könnte das Unquote `~` auch vor
`field-names` stehen. Dann aber würde das Mappen und Konvertieren in eine Menge
zur Laufzeit geschehen; so wie es jetzt ist, geschieht das bereits zur
Makro-Expansionszeit und ist damit etwas effizienter zur Laufzeit.

Nun haben wir alles beisammen, um unsere Records sinnvoll benutzen zu können!

```clojure
(def-record-type car [color])
(def-record-type bill [id iban amount paid?])
(def fire-truck (make-car "red"))

(car-color fire-truck)                ; => "red"
(car? fire-truck)                     ; => true
(bill? fire-truck)                    ; => false
(car? (assoc fire-truck :amount 23))  ; => false
```

## Fazit und Ausblick

Das Record-Makro nimmt der Entwicklerin nicht nur Schreibarbeit einzelner
Funktionen ab, sondern ermöglicht ein erweitertes, datenflussorientiertes
Programmieren. Zwar ist es erfreulich, dass Clojure überhaupt Records zur
Verfügung stellt, aber wie wir gesehen haben, stören einige
Implementierungsentscheidungen. Diese konnten wir erfolgreich ausmerzen. Das
eigentlich Hervorragende ist, dass unsere Implementierung auch ohne
Clojure-Records funktioniert!

Natürlich sind die hier entworfenen Records nur beispielhaft zu sehen: Um den
Rahmen des Blogposts nicht zu sprengen, sind ein paar fragwürdige Entscheidungen
(z. B. für die unterliegende Struktur des Records eine Hashmap zu wählen)
getroffen worden. Eine wirklich gute Alternative entwickelt und benutzt die
[Active Group GmbH](www.active-group.de) in der Entwicklung von
Clojure-Programmen:
[active-clojure-Records](https://github.com/active-group/active-clojure).

Neben zusammengesetzten Daten kommen *gemischte Daten* in der Modellierung von
Programmen vor. In einem nächsten Blogpost werden wir mit Makros Summentypen in
Clojure entwickeln und damit die Sprache noch weiter erweitern und an unsere
Vorstellungen anpassen.

<!-- more end -->
