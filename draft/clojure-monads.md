---
layout: post
title: "Freie Monaden in Clojure"
author: marcus-crestani
tags: ["Praxis", "freie", "Monade", "Clojure", "mocks", "mock-tests"]
---

Monaden sind ein wichtiges Konzept in der funktionalen Programmierung mit
immensem praktischen Nutzen, auch hier im Blog haben wir schon viel darüber
geschrieben.  Zum Beispiel zeigt der Artikel ["Freie Monaden oder: Wie ich
lernte, die Unabhängigkeit zu
lieben"](https://funktionale-programmierung.de/2018/01/22/freie-monade.html),
wie freie Monaden in Scala eingesetzt werden können, um die Ausführung eines
Programmes von dessen Beschreibung zu entkoppeln.

Hier werden wir die Fallstudie aus diesem Artikel aufgreifen und zeigen, wie wir
die Ausführung eines Programms mit Hilfe von Monaden in Clojure von der
Beschreibung entkoppeln.  Dabei zeigen wir, wie wir das nutzen können, um
Testfälle zu schreiben; und wir werden zeigen, wie wir ganz einfach Mock-Tests
für unsere monadischen Programme angeben können.

<!-- more start -->
## Clojure

Clojure ist eine funktionale Programmiersprache, deren Syntax aus den aus
Lisp bekannten geklammerten Ausdrücken besteht.  Clojure kompiliert nach
Java-Bytecode und läuft auf der Java Virtual Machine.  Wir verwenden Clojure in
der Praxis in sehr vielen unserer Projekte und haben über die letzten Jahre eine
umfangreiche und frei verfügbare Clojure-Bibliothek namens [Active
Clojure](https://github.com/active-group/active-clojure) entwickelt, die wir in
unseren Projekten verwenden.  Die Funktionalität aus dieser Bibliothek benutzen
wir in diesem Artikel.

## Fallstudie Adressbuch

In dem Beispiel geht es um die Implementierung eines einfachen Adressbuchs, das
Adressen enthält.

### Adressen

Eine Adresse besteht der Einfachheit halber nur aus ID[^1], Name und Stadt, auf
Details wie Straße und PLZ oder andere Einschränkungen verzichten wir, damit das
Beispiel übersichtlich bleibt.  Wir implementieren Adressen als zusammengesetzte
Daten mit Hilfe eines [Records aus unserer
Active-Clojure-Bibliothek](https://github.com/active-group/active-clojure#records):

```clojure
(define-record-type
  ^{:doc "An address"}
  Address
  (make-address id name town)
  address?
  [^{:doc "Id of address"}
   id address-id
   ^{:doc "Name of address"}
   name address-name
   ^{:doc "Town of address"}
   town address-town])
```

Diese Record-Definition liefert uns Funktionen, mit denen wir Adressen
konstruieren können, zum Beispiel erzeeugt

```clojure
(make-address 23 "Marcus" "Tübingen")
```

eine Adresse.  Dazu liefert die Record-Definition Selektoren, mit denen wir auf
die Bestandteile einer Adresse zugreifen können.  Zum Beispiel liefert

```clojure
(address-town (make-address 23 "Marcus" "Tübingen"))
```

die Zeichenkette `"Tübingen"`, die Stadt unserer Tübinger Beispieladresse.

[^1]: Dass unsere Adressen eine eindeutige ID benötigen, ergibt sich aus
      praktischen Gesichtspunkten: Es kann sein, dass wir aus Versehen doppelte
      Adressen in unserem Adressbuch abgespeichert haben.  Wenn wir dann diese
      doppelten Adressen aufräumen wollen, können wir nicht einfach alle
      Adressen mit einem bestimmten Namen und einem bestimmten Ort löschen, weil
      wir dann ja alle diese Adressen löschen würden (die Adressen sind gleich
      im Sinne von struktureller beziehungsweise extensionaler Gleicheit weil
      sie den gleichen Wert repräsentieren).  Stattdessen wollen wir ja nur die
      Doppelten löschen, die können wir nur mit einer eindeutigen ID
      identifizieren (intensionale Gleichheit würde das Problem auch lösen,
      macht aber nur Sinn bei mutierbaren Objekten, die wir aus vielen anderen
      Gründen aber immer vermeiden wollen).

### Adressbuch-Funktionen

Es soll Funktionen geben, die es erlauben, Adressen im Adressbuch abzulegen und
wieder aus dem Adressbuch zu löschen:

```clojure
(put-address <address>) ; eine Adresse im Adressbuch speichern
(delete-address <id>)   ; eine Adresse aus dem Adressbuch löschen
```

Beide Funktionen haben keinen nützlichen Rückgabewert, sie geben einfach `nil`
zurück.

Die Funktion `get-address` gibt eine Adresse anhand ihrer ID aus dem Adressbuch
zurück:

```clojure
(get-address <id>)      ; liefert Adresse aus Adressbuch
```

Außerdem soll es eine Funktion geben, die alle Adressen heraussucht, die auf ein
bestimmtes Prädikat passen, also einen Filter für Adressen:

```clojure
(filter-addresses <predicate?>) ; filtert Adressen aus dem Adressbuch heraus
```

Diese Funktion liefert eine Liste von Adressen, die auf das Pädikat
`<predicate?>` passen.  Ein Prädikat ist eine einstellige Funktion, die `true`
oder `false` zurück gibt.  Zum Beispiel ist dies ein Prädikat, das `true`
ergibt, wenn die Adresse in Tübingen ist, ansonsten liefert es `false`:

```clojure
(defn in-tübingen?
  [address]
  (= "Tübingen" (address-town address)))
```

Der Aufruf

```
(filter-addresses in-tübingen?)
```

liefert dann eine Liste aller Adressen aus unserem Adressbuch, die in Tübingen
sind.

<!--
Damit können wir auch eine Funktion `get` implementieren, die eine bestimmte
Adresse im Adressbuch findet und zurückgibt.  Die Funktion filtert alle Adressen
auf die gegebene `id` und da `filter` immer eine Liste von Adressen zurück gibt,
wählt `get` noch die erste Adresse dieser Liste mit `first` aus:

```clojure
(defn get
  [id]
  (first (filter (fn [address] (= id (address-id address))))))
```
-->

### Adressbuch-Datenbank

Wir können die aufgeführten Funktionen jetzt so implementieren, dass sie über
einen Datenbank-Treiber direkt in eine Datenbanktabelle schreiben.  Für die
Kommunikation mit der Datenbank verwenden wir
[Clojure-JDBC](https://github.com/clojure/java.jdbc).  Das sieht dann so aus:

```clojure
(defn put-address
  [db-connection address]
  (jdbc/insert! db-connection "adresses" (into {} address)))

(defn delete-address
  [db-connection id]
  (jdbc/delete! db-connection
                ["SELECT * FROM addresses where id = ?" id]
                {:row-fn map->Address}))

(defn get-address
  [db-connection id]
  (first (jdbc/query db-connection ["SELECT * FROM addresses where id = ?" id]
                     {:row-fn map->Address})))

(defn filter-addresses
  [db-connection predicate?]
  (filter predicate?
          (jdbc/query db-connection ["SELECT * FROM addresses"]
                      {:row-fn map->Address})))
```

Clojure-JDBC verwendet Maps als Datenrepräsentation, unsere Adressen-Records
können wir mit `(into {} address)` in Maps mit den Feldnamen als Keys umwandeln
und mit `map->Address` von so einer Map zurück in einen Record.  Die Funktion
`map->Address` ist eine Funktion, die unsere Record-Definition liefert.  Wenn
wir die Spalten unserer Tabelle also so nennen, wie die Felder im Record heißen,
dann reicht diese Datenkonvertierung aus.  Das Datenbanktabellen-Schema sollte
also so aussehen:

```sql
CREATE TABLE addresses (
 ID INT,
 NAME VARCHAR,
 TOWN VARCHAR
);
```

Die Funktion `put-address` fügt also via `jdbc/insert!` eine Adresse in die
Datenbanktabelle `adresses` ein, `delete-address` löscht sie via `jdbc/delete!`
wieder, `get-address` gibt die Adresse, die zur übergebenen ID passt, via 
`jdbc/query` zurück und `filter-addresses` selektiert alle Adressen via
`jdbc/query` und filtert sie mit der in Clojure eingebauen `filter`-Funktion
anhand des übergebenen Prädikats.

### So nicht!

Diese Implementierung funktioniert zwar, enthält aber mehrere Probleme, die
ausführlicher in [diesem
Artikel](https://funktionale-programmierung.de/2018/01/22/freie-monade.html)
beschrieben sind:

- Das Programm ist schwer testbar, da man keine Unit-Tests schreiben kann und
  auch für einfache Integrationstests ein Datenbank-Backend braucht.

- Die Seiteneffekte werden sofort ausgeführt und Mehrfachausführungen liefern
  unterschiedliche Ergebnisse.

- Die Schnittstelle ist zu speziell, da das Datenbank-Verbindungs-Objekt
  existieren und immer mitgegeben werden muss.

Die Kopplung zur Datenbank behindert die Entwicklung und zukünftige Anpassungen
und muss weg.

## Sprache für Adressbuch-Funktionen

Der erste Schritt zur Entfernung der Kopplung ist, dass wir unsere Operationen
als Daten repräsentieren anstatt als Funktionen mit Seiteneffekten.  Wir können
in Clojure unsere Operationen als Records definieren:

```clojure
(define-record-type
  ^{:doc "Put an address into the address book"}
  PutAddress
  (put-address address)
  put-address?
  [^{:doc "Address to put into the address book"}
   address put-address-address])

(define-record-type
  ^{:doc "Get an address from the address book"}
  GetAddress
  (get-address id)
  get-address?
  [^{:doc "ID of the address to get from the address book"}
   id get-address-id])

(define-record-type
  ^{:doc "Delete an address from the address book"}
  DeleteAddress
  (delete-address id)
  delete-address?
  [^{:doc "ID of the Address to delete from the address book"}
   id delete-address-id])

(define-record-type
  ^{:doc "Filter the address book"}
  FilterAddresses
  (filter-addresses predicate?)
  filter-addresses?
  [^{:doc "Predicate to filter the address book"}
   predicate? filter-addresses-predicate?])
```

Mit Listen aus diesen Operationen können wir jetzt schon mal einfache Programme
beschreiben, zum Beispiel eine Adresse einfügen und gleich wieder löschen:

```clojure
(put-address (make-address 23 "Marcus" "Tübingen"))
(delete-address 23)
```

Um sinnvollere Programme zu beschreiben, die auch komplexe Zusammenhänge
abbilden, brauchen wir aber noch die Möglichkeit, Zwischenergebnisse zu binden,
zum Beispiel um eine Prozedur zu schreiben, die alle Tübinger Adressen löscht.

## Monadic

Monadische Operationen können wir mit `bind` (oft auch `flatMap` genannt)
verknüpfen und die Zwischenergebnisse zur nächsten Operation weitergeben.  Das
händische Verknüpfen der Operationen mit `bind` ist allerdings mühsam und schwer
leserlich, daher benutzen wir mit `monadic` spezielle Syntax aus unserer
[Active-Clojure-Bibliothek für
Monaden](https://github.com/active-group/active-clojure#monad)
`active.clojure.monad`, mit der wir monadische Programme kompakter aufschreiben
können.

Damit können wir ein Programm schreiben, das alle Tübinger Adressen löscht:

```clojure
(monad/monadic
 [tübinger (filter-addresses in-tübingen?)]
 (monad/sequ (map (fn [address]
                    (delete-address (address-id address)))
                  tübinger)))
```

Das Programm bindet zunächst das Ergebnis der monadischen Berechnung von
`(filter-addresses in-tübingen?)` an die Variable `tübinger`, `tübinger` enthält
also eine Liste aller Tübinger Adressen.  Die letzte Zeile iteriert dann mit
`map` über die Liste aller Tübinger Adressen und nutzt `delete-address`, um die
Adressen zu löschen.  Da `delete-adress` selbst wieder eine monadische Operation
ist, ist das Ergebnis eine Liste von monadischen Operationen, die wir mit dem
eingebauten Kommando `monad/sequ` auswerten können.

Dieses Programm können wir in eine Funktion einbauen:

```clojure
(defn remove-addresses-in-tübingen
  "Remove all addresses that are located in Tübingen."
  []
  (monad/monadic
   [tübinger (filter-addresses in-tübingen?)]
   (monad/sequ_ (map (fn [address]
                       (delete-address (address-id address)))
                     tübinger))))
```

Wir haben nun also eine Repräsentation für Operationen, Zwischenergebnisse und
Abstraktionen.  Was noch fehlt, ist die Ausführung.  Dazu benötigen wir einen
Interpreter, der ein monadisches Programm entgegennimmt und alle Operationen
des Programms ausführt.  Oben haben wir ja bereits eine direkte
Datenbank-Implementierung geschrieben.  Diese Implementierung nutzen wir nun im
Interpreter für unsere Adressbuch-Monade:

```clojure
(defn database-run-command
  "Run a monadic address book program."
  [run env state m]
  (cond
    (lang/put-address? m)
    [(db/put-address (::db-spec env) (lang/put-address-address m))
     state]

    (lang/delete-address? m)
    [(db/delete-address (::db-spec env) (lang/delete-address-id m))
     state]

    (lang/get-address? m)
    [(db/get-address (::db-spec env) (lang/get-address-id m))
     state]

    (lang/filter-addresses? m)
    [(db/filter-addresses (::db-spec env) (lang/filter-addresses-predicate? m))
     state]

    :else
    monad/unknown-command))
```

Funktionen, die Interpreter implementieren, heißen in
`active.clojure.monad`-Konvention in der Regel `run-command`.  Diese Interpreter
akzeptieren vier Argumente, wobei das letzte Argument `m` das monadische
Programm ist.  Im Rumpf des Interpreters findet eine Fallunterscheidung auf das
monadische Programm `m` statt, je nach Kommando (per Konvention meist im
`lang`-Namespace) leitet der Interpreter auf die entsprechende Funktion der
Datenbank-Implementierung im Namespace `db` weiter.

Weitere Argumente sind eine Funktion `run` (um möglicherweise verschachtelte
monadische Kommands zu interpretieren), eine initiale Umgebung `env` und einen
initialen Zustand `state`; Umgebung und Zustand sind als Maps repräsentiert.

Der Rückgabewert des Interpreters ist ein Tupel aus dem Rückgabewert der
Datenbank-Implementierung und dem Zustand, der sich in diesem Interpreter aber
nicht verändert, `state` wird also in allen Fällen unverändert zurückgegeben.
Wir sehen gleich noch einen anderen Interpreter, der den Zustand verändert.
Außerdem gibt der Interpreter den Spezialwert `monad/unknown-command` zurück,
wenn er die übergebene Operation nicht kennt.

Dieser Interpreter erwartet das Datenbank-Verbindungs-Objekt in der Umgebung
`env` und gibt es an die Datenbank-Implementierung weiter.  Dieses
Datenbank-Verbindungs-Objekt müssen wir in der initialen Umgebung zur Verfügung
stellen, dann kümmert sich der Interpreter aber selbst darum, dass es an alle
Datenbank-Funktionen weitergegeben wird -- unser monadisches Programm muss sich
nicht darum kümmern.

In der Active-Clojure-Monade ist eine *Monaden-Kommando-Konfiguration* eine
Abstraktion für eine Interpreter-Funktion[^2] zusammen mit der zugehörigen
initialen Umgebung und dem zugehörigen initialen Zustand.
`monad/make-monad-command-config` erzeugt eine solche Abstraktion, bei uns sieht
das so aus:

```clojure
(defn database-monad-command-config
  [db]
  (monad/make-monad-command-config database-run-command
                                   {::db-spec db}
                                   {}))
```

[^2]: Die monadischen Kommandos, die von dieser Monaden-Kommando-Konfiguration
    abgedeckt sind, sind in dieser Repräsentation implizit durch die Fälle der
    Interpreter-Funktion beschrieben.

Die Interpreter-Funktion heißt bei uns `database-run-command`, in der initialen
Umgebung legen wir unter dem Schlüsselwort `::db-spec` das
Datenbank-Verbindungs-Objekt ab; der initiale Zustand ist die leere Map `{}`.

Wirklich laufen lassen können wir ein monadisches Programm mit
`monad/execute-monadic`, das eine Monaden-Kommando-Konfiguration und das
Programm akzeptiert.  Hier ist eine Beispielbenutzung:

```clojure
(monad/execute-monadic (database-monad-command-config db)
                       (put-address (make-address 23 "Marcus" "Tübingen")))
```

Offen lassen wir hier absichtlich die Definition des
Datenbank-Verbindungs-Objekt `db`.  Der vollständige, funktionierende Code aus
diesem Blog-Posting haben wir in diesem
[Github-Repo](https://github.com/active-group/active-clojure-monad-example)
veröffentlicht.  Darin ist auch eine H2-in-memory-Datenbank mit zugehöriger
Initalisierung in der Monaden-Kommando-Konfiguration abstrahiert, worauf wir in
diesem Blog-Posting nicht weiter eingehen werden.

## Testen

Die monadischen Kommandos als Indirektion vor der komplizierten
Datenbank-Implementierung bieten uns jetzt verschiedene Möglichkeiten, unsere
Programme -- also unsere Businesslogik -- zu testen, ohne dass wir die
Datenbank -- oder andere integrierte Umgebungen -- aufwändig dafür
berücksichtigen müssen.  Zwei Möglichkeiten schauen wir uns an:

1. monadische Kommandos mocken

2. Test-Interpreter, der keine Datenbank benutzt

### monadische Kommandos mocken

Die Active-Clojure-Bibliothek stellt zum Testen von monadischen Programmen einen
einfachen Testinterpreter bereit, der die ausgeführten Operationen lediglich
aufzeichnet und mit einer Liste von erwarteten Operationen abgleicht -- und die
Ergebnisse der Operationen mocken kann.

`mock/mock-run-command` bekommt als erstes Argument eine Liste der erwarteten
Operationen -- repräsentiert als Mock-Ergebnisse -- und als zweites Argument
das monadische Programm.  Ein Mock-Ergebnis besteht aus dem erwarteten
monadischen Kommando und dem Rückgabewert, mit dem das Programm weitermachen
soll.  Hier ist ein Beispiel dafür:

```clojure
(deftest t-state-put-get
  (is (= (make-address 2 "Marcus" "Tübingen")
         (mock/mock-run-monad
          [(mock/mock-result (put-address (make-address 2 "Marcus" "Tübingen"))
                             true)
           (mock/mock-result (get-address 2)
                             (make-address 2 "Marcus" "Tübingen"))]
          (monad/monadic
           (put-address (make-address 2 "Marcus" "Tübingen"))
           (get-address 2))))))
```

Hier sieht man, dass man so zwar die Reihenfolge der erwarteten Kommandos testen
kann, aber hierzu doch auch oft das Verhalten in den Mock-Ergebnissen
implementiert werden muss, vor allem wenn wie hier Ergebnisse von vorherigen
Kommandos abhängen.  Durch die flexible Kombinierbarkeit von
Monaden-Kommando-Konfigurationen ist es so aber zum Beispiel möglich, bestimmte
Kommandos tatsächlich interpretieren zu lassen und andere zu mocken.

### Test-Interpreter

Anstatt die Adressen in eine externe Datenbank zu schreiben, können wir auch
andere Backends benutzen.  Und für Tests der Business-Logik würde ein einfacher
Interpreter ausreichen, der die Adressen zum Beispiel im Monadenzustand
speichert.  So ein Interpreter könnte so aussehen:

```clojure
(defn state-run-command
  "Run a monadic address book program --- with state as simple backend storage."
  [run env state m]
  (cond
    (lang/put-address? m)
    (let [address (lang/put-address-address m)]
      [true
       (assoc state (address-id address) address)])

    (lang/get-address? m)
    [(get state (lang/get-address-id m))
     state]

    (lang/delete-address? m)
    [true
     (dissoc state (lang/delete-address-id m))]

    (lang/filter-addresses? m)
    [(filter (lang/filter-addresses-predicate? m) (vals state))
     state]

    :else
    monad/unknown-command))
```

Hier repräsentieren wir unsere Adressen-Datenbank als Clojure-Map im
Monadenzustand -- und die Monadenimplementierung kümmert sich darum, dass der
Zustand von Auswertungsschritt zu Auswertungsschritt weitergegeben wird; darum
müssen wir uns nicht selbst kümmern.

Damit haben unsere Testfälle überhaupt keine Abhängigkeiten in die
Infrastruktur.  Allerdings besteht die Gefahr, dass der Test-Interpreter und der
Datenbank-Interpreter sich abweichend verhalten -- aber das könnten wir mit
Tests auf anderer Ebene abdecken.  Und unsere eigentliche Business-Logik
ausgiebig ohne die Datenbank testen.

## Fazit

Monaden erlauben uns, Programme zu schreiben, die unabhängig von der späteren
Ausführung sind.  Kopplung an komplexe Infrastruktur vermeiden wir so.  Dadurch
gewinnen wir eine kompaktere Notation, höhere Verständlichkeit und bessere
Testbarkeit für unsere Business-Logik.

<!-- more end -->
