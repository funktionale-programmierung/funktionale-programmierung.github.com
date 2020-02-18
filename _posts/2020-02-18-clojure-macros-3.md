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
diesem Blogpost zwei Beispielen zu Makros in Clojure. Insbesondere werden wir
unser eigenes Record-Makro erstellen. Es empfiehlt sich, die vorherigen Beiträge
gelesen zu haben.

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*

## Warmup

Bisher haben wir nur Makros, die es schon in `clojure.core` gibt, nachgebaut.
Heute erweitern wir Clojure zum ersten Mal um ein neues, sinnvolles Makro! Das
Konstrukt `when-let` ist ähnlich zu `when`, lässt jedoch zusätzlich den
Testausdruck an ein Symbol binden:

```clojure
(when-let [x (:name {:name "Bruno" :age 42})]
  (str "Hallo " x))       ; => "Hallo Bruno"

(when-let [x (:last-name {:name "Bruno" :age 42})]
  (str "Dein Nachname ist " x))   ; => nil

```

Wir hätten gerne eine `when-let`-Version, die mehrere Bindungen zulässt, aber
abbricht, sobald ein Wert davon `nil` bzw. `false` ist:

```clojure
(when-let* [m {:person {:name "Kim" :age "42"}}
            person (:person m)
            name (:name person)]
  (str "Die Person-Map " person " enthält den Namen " name))
```

Bisher ist das nur so möglich:

```clojure
(when-let [m {:person {:name "Kim" :age "42"}}]
  (when-let [person (:person m)]
    (when-let [name (:name person)]
      (str "Die Person-Map " person " enthält den Namen " name))))
```

Anhand dieses Codeschnipsels können wir direkt ableiten, was im Makro zu tun
ist: Wir müssen rekursiv über die Bindungen in `bindings` iterieren und pro
Bindung einen `when-let`-Ausdruck erzeugen:

```clojure
(defmacro when-let*
  [bindings & body]
  (assert (vector? bindings) "bindings must be a vector")
  (assert (= 0 (mod (count bindings) 2))
          "when-let* requires an even number of forms in bindings")
  (if (empty? bindings)
    `(do
       ~@body)
    (let [sym (first bindings)
          value (second bindings)]
      `(when-let [~sym ~value]
         (when-let* ~(vec (drop 2 bindings))
           ~@body)))))
```

Zuvor testen wir zur Makro-Expansionszeit, ob nicht grober Unfug bei der
Benutzung von `when-let*` gemacht wurde. Dann überprüfen wir, ebenfalls zur
Expansionszeit, ob der `bindings`-Vektor leer ist, es also keine Bindungen mehr
zu erzeugen gilt. Als Konsequente wird `body` zurückgeliefert, ansonsten bauen
wir uns die Bindung für das erste `when-let` zusammen und rekursieren mit den
restlichen Bindungen.

Schön, dieses Makro ist eine echte Erleichterung sowohl für das Schreiben, als
auch für die Lesbarkeit. Nun widmen wir uns einem weitaus umfangreicheren
Makro-Beispiel, das in ähnlicher Form tagtäglich von uns Active
Group-EntwicklerInnen benutzt wird.

## Das Record-Makro

Clojure bietet uns mit dem Typkonstruktor `defrecord` die Möglichkeit,
sogenannte *zusammengesetzte Daten* zu erstellen. Dazu hat Kollege
[Sperber](https://www.active-group.de/teamplayer/05sperber.html) bereits einen
[Blogpost](https://funktionale-programmierung.de/2015/04/27/clojure-records.html)
verfasst.

Deshalb gehen wir hier nicht weiter auf zusammengesetzte Daten ein, sondern
erläutern nur noch kurz die Funktionsweise von Clojure-Records. Wir übernehmen
das Computer-Beispiel aus obigem Beitrag. Nach Auswertung von

```clojure
(defrecord Computer [cpu ram hard-drive])
```

können wir, mithilfe des dadurch zur Verfügung gestellten Recordkonstruktors
`->Computer`, Computer-Records erstellen, die drei Felder besitzen, `cpu`,
`ram` und `hard-drive`:

```clojure
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2 250))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16 1000))
```

Weiter ist es möglich, via *Keywords* auf die einzelnen Felder zuzugreifen,
beispielsweise liefern `(:ram office-pc)` und `(:hard-drive gaming-pc)` die
Werte `2` und `1000`.

Wir schreiben nun unseren eigenen Recordtypkonstruktor, `def-my-record`. Dieser
muss folgende Funktionen liefern:

- einen Recordkonstruktor
- Feld-Selektoren
- Typ-Prädikat

Als Argumente konsumiert er einen Recordtyp-Namen und Recordtypfelder-Namen. Das
Skelett des Makros sieht dann so aus:

```clojure
(defmacro def-my-record
  [type-name & field-names]
  ...)
```

Im Rumpf werden die oben beschriebenen Funktionen erzeugt.

### Recordkonstruktor

Wir fangen mit der Implementierung des Recordkonstruktors an. Dieser soll nach
Anwendung auf Feldwert-Argumente einen Record zurückgeben. Als unterliegende
Struktur eines Records verwenden wir eine Clojure-*Hashmap*.

Ein nicht automatisch generierter Konstruktor für Computer-Records lautet:

```clojure
(defn ->>computer
  [cpu ram hard-drive]
  {:cpu cpu
   :ram ram
   :hard-drive hard-drive}))
```

und wir können den Gaming-Computer via 

```clojure
(->>computer "Intel I5 6600 3600 MHz" 16 1000)
```

erzeugen. 

Nun zum Makro: Wir müssen also Code schreiben, der uns obigen Ausdruck als
Clojure-Liste zurückgibt. Den Namen der Funktion erhalten wir durch
Konkatenation von `"->>"` und `type-name`. Der Parametervektor ist einfach der
übergebene Feldnamen-Vektor. Schlussendlich erzeugen wir die Hashmap, indem wir
über die Feldnamen-Liste iterieren und Tupel erzeugen:

```clojure
(defmacro def-my-record
  [type-name & field-names]
  `(defn ~(symbol (str "->>" type-name))
     ~(vec field-names)
     ~(into {}
            (map (fn [field-name]
                   [(keyword field-name) field-name])
                 field-names))))
```

Wir können mithilfe von `macroexpand-1` überprüfen, ob tatsächlich der
gewünschte Code erzeugt wird:

```clojure
(macroexpand-1 '(def-my-record computer [cpu ram hard-drive]))

;; => (clojure.core/defn ->>computer 
;;                       [cpu ram hard-drive] 
;;                       {:cpu cpu, :ram ram, :hard-drive hard-drive})
```

Da das Recordtypkonstruktor-Makro noch weitere Funktionen erzeugen wird, lagern
wir, bevor wir fortfahren, das Recordkonstruktor-Erzeugen noch in eine eigene
Funktion aus:

```clojure
(defn create-record-constructor
  [type-name field-names]
  `(defn ~(symbol (str "->>" type-name))
     ~field-names
     ~(into {}
            (map (fn [field-name]
                   [(keyword field-name) field-name])
                 field-names))))

(defmacro def-my-record
  [type-name field-names]
  `(do
     ~(create-record-constructor type-name field-names)))
```

Beim Testen der neuen Recordkonstruktor-Erzeugerfunktion müssen wir darauf
achten, dass diese Symbole konsumiert:

```clojure
(create-record-constructor 'computer ['cpu 'ram 'hard-drive])
```

### Selektoren

Wir können nun eigene Recordtypen und dazugehörige Records erzeugen. Auf die
einzelnen Feldwerte der Records ist es, wie bei den Clojure-`defrecord`s auch,
bereits möglich, mit Keywords zuzugreifen:

```clojure
(:ram (->>computer "Intel I5 6600 3600 MHz" 16 1000))

;; => 16

```

Der Keywordzugriff ist in manchen Situationen jedoch problematisch: Ob ein
Feldwert tatsächlich `nil` ist oder sich ein Vertipper (zum Beispiel `:ran`
statt `:ram`) eingeschlichen hat, ist hier nicht unterscheidbar! Deshalb
erzeugen wir eigene Selektorfunktionen.

Hier der Code für einen nicht automatisch generierten Computer-Selektor:

```clojure
(defn computer-cpu
  [computer]
  (:cpu computer))
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
(defmacro def-my-record
  [type-name field-names]
  `(do
     ~(create-record-constructor type-name field-names)
     ~@(create-record-accessors type-name field-names)))

(def-my-record computer [cpu ram hard-drive])
(def office-pc (->>computer "INTEL 3000 Mhz" 4 500))
(computer-hard-drive office-pc)

;; => 500
```

### Typ-Prädikat

Um die Records auch sinnvoll benutzbar zu machen, benötigen wir noch eine
Möglichkeit zur Überprüfung, ob ein Record eine Instanz eines bestimmten Typs
ist. Clojure-Records machen das über `(instance? Computer my-computer)`. Unsere
Implementierung ermöglicht uns das bis jetzt noch nicht; wir erzeugen
schließlich nur eine einfache Hashmap ohne Typinformation. Deshalb fügen wir den
Metadaten der Hashmap nun ein (Key:Val)-Paar `(:__type__ : type-name)` hinzu.

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

Nun zum *Typ-Prädikat*. Das Typ-Prädikat für `computer` soll eine Funktion
`computer?` sein, sodass `(computer? thing)` im Falle eines Computer-Records
`true` und ansonsten `false` zurückgibt. Als nicht generische Funktion würden
wir sie so schreiben:

```clojure
(defn computer? 
  [thing] 
  (= 'computer (:__type__ (meta thing))))
```

Wir schreiben also eine Erzeugerfunktion, die zu gegebenen Typnamen eine
Funktion wie oben zurückgibt:

```clojure
(defn create-predicate
  [type-name]
  `(defn ~(symbol (str type-name "?"))
     [~'thing]
     (= '~type-name (:__type__ (meta ~'thing)))))
```

Etwas befremdlich könnten `~'thing` und `'~type-name` wirken. Warum nicht
einfach `thing` statt `~'thing`? Wie im [vorherigen
Blogpost](https://funktionale-programmierung.de/2019/09/17/clojure-macros-2.html)
bereits erläutert, qualifiziert das Syntax-Quote `` ` `` Symbole. `defn`
akzeptiert in der Parameterliste jedoch keine qualifizierten Symbole. `'~` sorgt
dafür, dass tatsächlich das übergebene Symbol (und nicht `type-name`) dasteht,
zur Laufzeit aber nicht weiter evaluiert wird. Gleiche Begründung gilt bei ``
`'~type-name `` aus `create-record-constructor` von oben, hier nur noch
zusätzlich `` ` ``, da zuvor unquotet wurde.

Nun haben wir alles beisammen, um unsere Records sinnvoll benutzen zu können!

```clojure
(def-my-record car [color])
(def fire-truck (->>car "red"))
(car-color fire-truck)     ; => "red"
(car? fire-truck)          ; => true
(computer? fire-truck)     ; => false
```

## Fazit und Ausblick

Heute wurden zwei nützliche Makros vorgestellt. Das `when-let*`-Makro bringt
bessere Lesbarkeit und Schreiberleichterung mit sich. Das Record-Makro nimmt der
Entwicklerin nicht nur Schreibarbeit einzelner Funktionen ab, sondern ermöglicht
ein erweitertes, datengesteuertes Programmieren. Die [Active Group
GmbH](www.active-group.de) benutzt ein ähnliches
[Makro](https://github.com/active-group/active-clojure) in der Entwicklung von
Clojure-Programmen. 

Neben zusammengesetzten Daten kommen *gemischte Daten* in der Modellierung von
Programmen vor. In einem nächsten Blogpost werden wir mit Makros Summentypen in
Clojure entwickeln und damit die Sprache noch weiter erweitern und an unsere
Vorstellungen anpassen.

<!-- more end -->
