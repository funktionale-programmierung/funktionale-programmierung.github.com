---
layout: post
title: "Monitoring: mit Riemann Graphite-Events für die InfluxDB mappen"
author: stephan-guenther
tags: ["riemann", "InfluxDB", "Graphite", "Monitoring", "Clojure"]
---

Im Folgenden geht es darum zu zeigen, wie man Riemann in einer bestehenden Montoring-Infrastruktur konfiguriert, damit eingehende Events im Graphite-Format in einer Influx-DB gespeichert und effizient durchsucht werden können.


## Riemann ##

Wenn man eine möglichst flexibele Lösungen für seine Monitoring-Infrastruktur braucht, ist
[Riemann](http://riemann.io/) ein vielseitigs Tool, um diverse Event-Preprocessing funktionalitäten abzudecken.

Dazu werden Events durch in [Clojure](https://clojure.org/) konfigurierte Pipelines gestreamt und and verschiedene Backends weitergeleitet.


## InfluxDB ##

Die [InfluxDB](https://docs.influxdata.com/influxdb/) ist ein passendes Backend um Time-Based-Events persistet, kompakt und durchsuchbar zu speichern. Die Query-Language ([InfluxQL](https://docs.influxdata.com/influxdb/v1.7/query_language/data_download/)) ähnelt nicht nur vom Namen SQL.

Das Datenmodell kurz zusammengefasst: es werden pro Datenbank mehrere *Measurements* (Tabellen) gespeichert und jedes Measurement kann dabei beliebig viele *Fields* (Spalten) besitzen, mindestends jedoch die Felder `timestamp` und `value`.

Jedes Measurement verhält sich dabei wie ein Key-Value-Store - beim Einfügen werden die Fields automatisch angelegt. Soll später über das Field in Queries gefiltert werden, muss beim Einfügen explizit definiert weden, dass dafür ein Index angelegt werden soll.


## Problemstellung ##

Riemann ist bereits installiert/vorhanden und erhält Events aus einer Quelle im Graphite-Format. Diese sollen an die InfluxDB geschrieben werden, so dass diese sich in der Auswertung mit InfluxQL performant abfragen lassen.


### Graphite ###

Events im Graphite-Format sind Textbasiert, flach strukturiert und besitzen diese Attribute.

    <metric-path> <value> <timestamp>

Die Komponenten im "metric-path" sind separiert durch Punkt.
Beispiele sind:

    servers.localhost.cpu.loadavg.1 0.64 1556273291
    servers.localhost.cpu.loadavg.5 0.31 1556273291
    servers.localhost.cpu.loadavg.10 0.12 1556273291
    servers.localhost.cpu.loadavg.1 0.64 1556273291
    servers.localhost.cpu.loadavg.5 0.31 1556273291
    servers.localhost.cpu.loadavg.10 0.12 1556273291


### Riemann und Beispiel-Config ###

Füttert man Riemann mit diesen Graphite-Events, dann sollten diese in folgendem Input-Event-Format in Riemann ankommen:

```clojure
{
    :service "<metric-path>"
    :time    <timestamp>
    :metric  <value>
    ...
}
```

Eine einfache Beispiel-Konfiguration für Riemann (`riemann.config`) sieht dann so aus:

```clojure
; Listen on all interfaces over TCP (5555)
(let [host "0.0.0.0"]
    (tcp-server {:host host}))

(def influx-config {
    :version :0.9
    :host "influxdb.example.com"
    :username "riemann"
    :password "secret"})

; Creates InfluxDB sinks for an specific database and optional tags
(defn influx-sink [database & tags]
    (let [cfg (merge influx-config {:db database})
          withtags (if (empty? tags) cfg (assoc cfg :tag-fields (set tags)))]
        (influxdb withtags)))

(let [index (index)]
    ; Inbound events will be passed to these streams:
    (streams
        (default :ttl 60
            ; Index all events immediately
            index

            ; Push all events to InfluxDB database "my-db" ...
            (where (not (state "expired"))
                (influx-sink "my-db"))

            ; Log expired events
            (expired
                (fn [event] (info "expired" event))))))
```

Das InfluxDB backend mappt dabei automatisch die Event-Keys

- `:service` auf `:measurement`
- `:metric` auf `:value`
- `:time` auf `:timestamp`

Alle weiteren Keys (die für Riemann keine Bedeutung haben) werden beibehalten und in InfluxDB als *Fields* übertragen.


## Splitting und Mapping ##

Mit der Beispiel-Config werden nun alle Metric-Paths aus den Graphite-Events als Measurements in der InfluxDB gespeichert. Das ist aber nicht was man haben will, denn man kann so in InfluxQL keine Queries auf Teilen der Metric-Paths stellen.

Das Attribute `:service` muss dazu aufgetrennt werden und wie Eingangs erläutert einige davon als *Tags* markiert werden, damit ein Index dafür angelegt wird.

Dazu legen wir uns ein paar Hilfs-Funktionen an:

```clojure
; Splits input string on period (if not quoted) and skip's empty parts
(defn part-split [s]
    (let [matches (re-seq #"([^\"][^\.]*)\.*|\"([^\"]+)\"\.*" s)]
        (mapv #(first (remove nil? (rest %))) matches)))

; Creates a event-matcher
; (that splits key `k` by periods and matches if part at index `pos` is `value`
(defn part-eq [k pos value]
    (fn [event]
        (let [parts (part-split (k event))]
            (and (< pos (count parts)) (= value (parts pos))))))

; Relocates a value in dictionary `m` from key `ksrc` to `kdest`
(defn remap [m ksrc kdest]
    (let [v (ksrc m)]
        (dissoc (assoc m kdest v) ksrc)))
```

Damit kann man schon mal beliebige Event-Keys auftrennen. Dann brauchen wir noch eine Funktion, die Events so tranformiert, dass bestimmte Teilstücke aus einem `:service` Key in andere Keys übertragen werden.

Um das flexibel zu halten, will man zusätzlich zum
- Mapping der *Parts*
- auch *Parts* verwerfen und nicht übertragen
- oder noch nicht referenzierte *Parts* zu einem neuen Field zusammenfügen
- einen Fallback definieren, falls das Splitting nicht erfolgreich war

Dazu folgende Hilfsfunktion:

```clojure
; Modifies the event's :service by splitting it by "." and mapping these parts as
; value to other fields or dropping some of them by index. All parts that are not
; touched this way are joined by "." and will build a new field name. The value
; of :metric will be stored in that field.
; In case anything could be parsed, this modified event is passed to the 'good'
; function, otherwise it will be passed to the 'bad' function.
(defn graphite-map
    ([mapped dropped good bad]
        (graphite-map mapped dropped good bad nil))
    ([mapped dropped good bad restkeyword]
        (graphite-map mapped dropped good bad restkeyword :metric))
    ([mapped dropped good bad restkeyword restfield]
        (fn [event]
            (let [parts (part-split (:service event))
                  valid-map (if (empty? mapped) true (< (apply max (vals mapped)) (count parts)))
                  valid-drop (if (empty? dropped) true (< (apply max dropped) (count parts)))]
                (if (and valid-map valid-drop)
                    (let [fields (reduce-kv (fn [m k v]
                              (assoc m k (parts v))) {} mapped)
                          others (let [used (set (apply conj (vals mapped) dropped))]
                              (keep-indexed (fn [i p]
                                  (if (not (contains? used i)) p)) parts))
                          conv (if (empty? others) identity (fn [e]
                              (let [restval (clojure.string/join "." others)]
                                  (if (nil? restkeyword)
                                      (remap e restfield (keyword restval))
                                      (assoc e restkeyword restval)))))]
                        (call-rescue (conv (merge event fields)) (list good)))
                    (call-rescue event (list bad)))))))
```

Nun braucht man nur das ganze für seinen eigenen Anwendungszweck zusammenzustecken. Dazu als Beispiel folgende Hilfsfunktion, die dann noch in den input-stream eingebunden werden muss:

```clojure
; Detects known events in Graphite format (long :service) and converts them to
; the InlfuxDB 0.9 format with added tags and pushes that into the InfluxDB sink.
(defn graphite-split [database]
    (let [unparsable (graphite-bad "_unparsable" (influx-sink database :host :meta))
          unknown (graphite-bad "_unmapped" (influx-sink database :host :meta))]
        (split*
            (part-eq :service 1 "filesystem")
                (let [influx (influx-sink database :host :mountpoint)
                      mapping {:host 0, :mountpoint 2}
                      dropping (list 1)]
                    (graphite-map mapping dropping influx unparsable))
            (part-eq :service 1 "postgres")
                (let [influx (influx-sink database :host :database :table)
                      mapping {:service 1, :host 0, :database 2, :table 3}
                      dropping ()]
                    (graphite-map mapping dropping influx unparsable))
            (part-eq :service 1 "management")
                (let [influx (influx-sink database :host :program :variable)
                      mapping {:service 1, :host 0, :program 2}
                      dropping ()]
                    (graphite-map mapping dropping influx unparsable :variable))
            unknown)))
```
```clojure
(let [index (index)]
    ; Inbound events will be passed to these streams:
    (streams
        (default :ttl 60
            ; Index all events immediately.
            index

            ; Push split all events according to its rules and push them
            ; to InfluxDB database "my-db" ...
            (where (not (state "expired"))
                (graphite-split "my-db"))

            ; Log expired events.
            (expired
                (fn [event] (info "expired" event))))))
```

Damit sollten dann diese Graphite-Events ...

    myhost.filesystem./boot.iavail 124628 1557387008
    myhost.postgres.SyncDb.world_graph_edges.seq_scan 234 1474461001
    myhost.management.uigen.DbCache1.CountVar 16 1557387906

```clojure
{
    :service myhost.filesystem./boot.iavail
    :metric 124628
    :time 1557387008
}
```
```clojure
{
    :service myhost.postgres.SyncDb.world_graph_edges.seq_scan
    :metric 234
    :time 1474461001
}
```
```clojure
{
    :service myhost.management.uigen.DbCache1.CountVar
    :metric 16
    :time 1557387906
}
```

... so transformiert und in die "my-db" geschrieben werden:

```clojure
{
    :service filesystem   ; measurement
    :host myhost          ; tag
    :mountpoint /boot     ; tag
    :iavail 124628        ; field
    :time 1557387008
}
```
```clojure
{
    :service postgres          ; measurement
    :host myhost               ; tag
    :database SyncDb           ; tag
    :table world_graph_edges   ; tag
    :seq_scan 234              ; field
    :time 1474461001
}
```
```clojure
{
    :service management           ; measurement
    :host myhost                  ; tag
    :program uigen                ; tag
    :variable DbCache1.CountVar   ; tag
    :metric 16                    ; field
    :time 1557387906
}
```

## Tests ##

Funktioniert die konfiguration so wie erwartet? Am besten definiert man dazu direkt einige Tests in der `riemann.config`. Der Test-Runner ist dafür direkt in Riemann [integiert](http://riemann.io/howto.html#writing-tests). Für die Hilfsfunktionen könnte das so aussehen:

```clojure
(tests

    (deftest remap
        (let [in {:a 42}
              out {:b 42}]
            (is (= out (riemann.config/remap in :a :b)))))

    (deftest part-split
        (let [in0 "first.second.third.\"fourth.with.data\".fifth"
              out0 ["first", "second", "third", "fourth.with.data", "fifth"]
              in1 "myhost.filesystem./.avail"
              out1 ["myhost", "filesystem", "/", "avail"]]
            (is (= out0 (riemann.config/part-split in0)))
            (is (= out1 (riemann.config/part-split in1)))))

    (deftest graphite-map
        (let [in {:service "myhost.sync.eventlog.client-datasize.\"1ec32d51.e73afc36.76d9a7d1.1930d444\""
                  :metric 42}
              out (atom {})
              outset #(swap! out (fn [prev] %1))
              tester (fn [mapping dropping result]
                  ((riemann.config/graphite-map mapping dropping #(outset %) #(outset %)) in)
                  (is (= @out result)))]
            (tester {:host 0
                     :service 1
                     :source 2
                     :type 3} ;store non-mapped service-part as field
                ()
                {:host "myhost"
                 :service "sync"
                 :source "eventlog"
                 :type "client-datasize"
                 :1ec32d51.e73afc36.76d9a7d1.1930d444 42})
            (tester {:host 0
                     :service 1
                     :source 2
                     :type 3
                     :device 4} ;do not touch metric if all parts are mapped
                ()
                {:host "myhost"
                 :service "sync"
                 :source "eventlog"
                 :type "client-datasize"
                 :device "1ec32d51.e73afc36.76d9a7d1.1930d444"
                 :metric 42})
            (tester {:host 0
                     :service 1
                     :device 4} ;join non-mapped parts by dots
                ()
                {:host "myhost"
                 :service "sync"
                 :device "1ec32d51.e73afc36.76d9a7d1.1930d444"
                 :eventlog.client-datasize 42})
            (tester {:host 0
                     :service 1
                     :device 4}
                (list 2 3) ;drop unused parts
                {:host "myhost"
                 :service "sync"
                 :device "1ec32d51.e73afc36.76d9a7d1.1930d444"
                 :metric 42}))))
```

So kann man erstmal nur einzelne Funtkionen testen. Möchte man Tests für das Event-Processing schreiben, müssen zusätzlich *tap's* in der Pipeline definiert werden, die es erlauben die Events an dieser Stelle abzugreifen.

So gehts:

```clojure
; Creates InfluxDB sinks for an specific database and optional tags (and a tap)
(defn influx-sink [database & tags]
    (tap :influx
        (let [cfg (merge influx-config {:db database})
            withtags (if (empty? tags) cfg (assoc cfg :tag-fields (set tags)))]
            (influxdb withtags))))
```

Und jetzt noch Tests für unsere drei beispiele oben:
```clojure
(tests

    ; ...
    ; weitere Tests

    (deftest filesystem
        (let [in {:service "filesystem./boot.iavail"
                  :time 1474461001
                  :metric 124628}
              out {:host "myhost"
                   :service "filesystem"
                   :mountpoint "/boot"
                   :iavail 124628
                   :ttl 60
                   :time 1474461001}]
            (is (= (inject! [in]) {:influx [out]}))))

    (deftest postgres
        (let [in {:service "myhost.postgres.SyncDb.world_graph_edges.seq_scan"
                  :time 1474461001
                  :metric 234}
              out {:host "myhost"
                   :service "postgres"
                   :database "SyncDb"
                   :table "world_graph_edges"
                   :seq_scan 234
                   :ttl 60
                   :time 1474461001}]
            (is (= (inject! [in]) {:influx [out]}))))

    (deftest management
        (let [in {:service "myhost.management.uigen.DbCache1.CountVar"
                  :time 1557387906
                  :metric 16}
              out {:host "b-charite-70"
                   :service "management"
                   :program "uigen"
                   :variable "DbCache1.CountVar"
                   :metric 16
                   :ttl 60
                   :time 1557387906}]
            (is (= (inject! [in]) {:influx [out]})))))
```

