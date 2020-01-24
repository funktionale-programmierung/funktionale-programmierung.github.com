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
diesem Blogpost einigen Beispielen zu Makros in Clojure. Insbesondere werden wir
unser eigenes Record-Makro erstellen. Es empfiehlt sich, die vorherigen Beiträge
gelesen zu haben.

<!-- more start -->

*Hinweis: Der komplette Code ist auf
[Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*

## Warmup

Bisher haben wir nur Makros, die es schon in`clojure.core` gibt, nachgebaut.
Heute erweitern wir Clojure zum ersten Mal um ein neues, sinnvolles Makro! Das
Konstrukt `when-let` ist ähnlich zu `when`, lässt jedoch zusätzlich den
Testausdruck an ein Symbol binden:

```clojure
(when-let [x (:name {:name "Bruno" :age 42})]
  (str "Hello " x)) ;; => "Hello Bruno"

(when-let [x (:last-name {:name "Bruno" :age 42})]
  (str "Hello Mr. " x)) ;; => nil

```

Wir hätten gerne eine `when-let`-Version, die mehrere Bindungen zulässt, aber
abbricht, sobald ein Wert davon `nil` bzw. `false` ist:

```clojure
(when-let* [r {:person {:name "Kim" :age "42"}}
            person (:person r)
            name (:name person)]
  (str "Der Person-Record " person " enthält den Namen " name))
```

Bisher ist das nur so möglich:

```clojure
(when-let [r {:person {:name "Kim" :age "42"}}]
  (when-let [person (:person r)]
    (when-let [name (:name person)]
      (str "Der Person-Record " person " enthält den Namen " name))))
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
zu erzeugen gilt. AlsKonsequente wird `body` zurückgeliefert, ansonsten bauen
wir uns die Bindung für das erste `when-let` zusammen und rekursieren mit den
restlichen Bindungen.

Schön, dieses Makro ist eine echte Erleichterung sowohl für das Schreiben, als
auch für die Lesbarkeit. Nun widmen wir uns einem weitaus umfangreicheren
Makro-Beispiel, das so ähnlich tagtäglich von der Active Group benutzt wird.

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

können wir, mithilfe des dadurch zur Verfügung gestellten Record-Konstruktors
`->Computer`, Computer-Records erstellen, die drei Felder besitzen, `cpu`,
`ram` und `hard-drive`:

```clojure
(def office-pc (->Computer "AMD Athlon XP 1700 MHz" 2 250))
(def gaming-pc (->Computer "Intel I5 6600 3600 MHz" 16 1000))
```

Weiter ist es möglich, via *Keywords* auf die einzelnen Felder zuzugreifen,
beispielsweise liefert `(:ram office-pc)` den Wert `2` und `(:hard-drive
gaming-pc)` den Wert `1000`.

Wir schreiben nun unseren eigenen Recordtypkonstruktor! Dieser muss folgende
Funktionen liefern: 

- einen Recordkonstruktor
- Feld-Selektoren
- Typ-Prädikat

Der Recordtypkonstruktor, wir werden ihn `def-my-record` nennen, muss mindestens
folgende Argumente konsumieren:

- Recordtyp-Name
- Recordtypfelder-Namen

Das Skelett des Recordtypkonstruktors sieht damit so aus:

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
(defn ->>Computer
  [cpu ram hard-drive]
  {:cpu cpu
   :ram ram
   :hard-drive hard-drive}))
```

und wir können den Office-Computer via 

```clojure
(->>Computer "Intel I5 6600 3600 MHz" 16 1000)
```

erzeugen. 

Nun zum Makro: Wir müssen also Code schreiben, der uns obigen Ausdruck als
Clojure-Liste zurückgibt. Den Namen der Funktion erhalten wir durch
Konkatenation von `"->>"` und `type-name`. De Parametervektor ist einfach der
übergebene Feldnamen-Vektor. Schlussendlich erzeugen wir die Hashmap indem wir
über die Feldnamen-Liste iterieren und Tupel erzeugen:

```clojure
(defmacro def-my-record1
  [type-name & field-names]
  `(do
     (defn ~(symbol (str "->>" type-name))
       ~(vec field-names)
       ~(into {}
              (map (fn [field-name]
                     [(keyword field-name) field-name])
                   field-names)))))
```

Wir können mithilfe von `macroexpand-1` überprüfen, ob tatsächlich der
gewünschte Code erzeugt wird:

```clojure
(macroexpand-1 '(def-my-record1 Computer [cpu ram hard-drive]))

--> (do 
      (clojure.core/defn ->>Computer 
                         [cpu ram hard-drive] 
                         {:cpu cpu, :ram ram, :hard-drive hard-drive}))
```

Der `do`-Befehl ist bis jetzt eigentlichnoch unnötig, wir werden ihn gleich,
sobald der Recordtypkonstrukor mehrere Funktionen erzeugen soll, brauchen.

Bevor wir dazu aber übergehen, wollen wir das Recordkonstruktor-Erzeugen noch
auslagern:

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
(create-record-constructor 'Computer ['cpu 'ram 'hard-drive])
```

### Selektoren

Wir können nun eigene Recordtypen und dazugehörige Records erzeugen. Auf die
einzelnen Feldwerte der Records ist es, wie bei den Clojure-`defrecord`s auch,
bereits möglich, mit Keywords zugreifen:

```clojure
(:ram (->>Computer "Intel I5 6600 3600 MHz" 16 1000))

--> 16

```

Der Keywordzugriff ist in manchen Situationen jedoch problematisch: Ob ein
Feldwert tatsächlich `nil` ist oder sich ein Vertipper (z. B. `:ran` statt
`:ram`) eingeschlichen hat, ist hier nicht unterscheidbar! Deshalb erzeugen wir
eigene Selektorfunktionen.

Hier der Code für einen nicht automatisch generierten Computer-Selektor:

```clojure
(defn Computer-cpu
  [Computer]
  (:cpu Computer))
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

(def-my-record Computer [cpu ram hard-drive])
(def office-pc (->>Computer "INTEL 3000 Mhz" 4 500))
(Computer-hard-drive office-pc)

--> 500
```

### Typ-Prädikat

Um die Records auch sinnvoll benutzbar zu machen, benötigen wir noch eine
Möglichkeit zur Überprüfung, ob ein Record eine Instanz eines bestimmten Typs
ist. Clojure-Records machen das über `(instance? Computer my-computer)`. Bis
jetzt gibt unsere Implementierung zu wenig her, um Records verschiedener Typen
zu unterscheiden. Deshalb fügen wir den Metadaten der erzeugten Hashmap nun ein
(Key:Val)-Paar `(:__type__ : type-name)` hinzu.

In `create-record-constructor` müssen wir dazu den `(into {} ...)`-Asudruck um
einen `vary-meta`-Aufruf

```clojure
(vary-meta (into {}
                 (map (fn [field-name]
                        [(keyword field-name) field-name])
                      field-names))
           (fn [m] (assoc m :__type__ `'~type-name)))
```

erweitern. Das kryptisch anmutende `` `'~type-name `` wird weiter unten
erläutert.

Nun zum *Typ-Prädikat*. Das Typ-Prädikat für `Computer` soll eine Funktion
`Computer?` sein, sodass `(Computer? thing)` im Falle eines Computer-Records
`true` und ansonsten `false` zurückgibt. Als nicht generische Funktion würden
wir sie so schreiben:

```clojure
(defn Computer? 
  [thing] 
  (= 'Computer (:__type__ (meta thing))))
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
bereits erläutert, qualifiziert das Syntax-Quote (`` ` ``) Symbole. `defn`
akzeptiert in der Parameterliste jedoch keine qualifizierten Symbole. `'~` sorgt
dafür, dass tatsächlich das übergebene Symbol (und nicht `type-name`) dasteht,
zur Laufzeit aber nicht weiter evaluiert wird. Gleiche Begründung gilt bei ``
`'~type-name `` aus dem `create-record-constructor`, hier nur das zusätzliche ``
` ``, da zuvor unquotet wurde.


## TODO das hier oben einfügen
Wir benötigen das zusätzliche `do` um die beiden Funktionsdefinitionen herum,
weil das Makro ansonsten zur Makro-Expansionszeit zwar beide Ausdrücke
berechnen, aber nur den Letzten zurückgeben würde.
## TODO 

Nun haben wir alles beisammen, um unsere Records sinnvoll benutzen zu können!


```clojure
(def-my-record Car color)
(def fire-truck (->>Car "red"))
(:color fire-truck)        ; --> "red"
(Car? fire-truck)          ; --> true
```

Durch `(macroexpand-1 '(def-my-record Car color))` können wir uns wieder zeigen
lassen, ob unser Makro auch die gewünschten Funktionen erstellt.



## TODO
Warum eigene Records?
- Keywordzugriff schlecht vs Accessor
- instance? doof, besser eigenes Prädikat
## TODO


## Fazit und Ausblick

Mit Makros können wir auf elegante Art und Weise unsere Programmiersprache
individuell an gegebene Problemstellungen anpassen. Die Notwendigkeit und
Wichtigkeit von Makros wurde in diesem ersten Blogpost anhand eines Bedürfnisses
(Spracherweiterung durch Record-Typen) dargestellt und die Handhabung von Makros
über die stückweise Entwicklung des Record-Typen nähergebracht.

<!-- more end -->
