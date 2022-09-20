---
layout: post
title: "Eventverarbeitung mit Riemann"
author: marcus-crestani
tags: ["Riemann", "InfluxDB", "Elasticsearch", "Logs", "Metriken", "Monitoring", "Clojure"]
---

Riemann ist ein Stream-Processing-System, das sich hervorragend zum Sammeln und
Verarbeiten von Events und Logs von Servern und Systemen eignet.  Wir verwenden
Riemann erfolgreich produktiv in [sehr großen IT-Systemen als Kernstück für die
Log- und Metrikverarbeitung und für das
Monitoring](https://codetalks.de/speakers#talk-1345?event=7).

Dabei nutzen wir Riemann zur Aufbereitung von Events und zum Weiterleiten an
Langzeitspeichersysteme wie
[Elasticsearch](https://www.elastic.co/elasticsearch/), in denen diese Events
wiederum durch Benutzeroberflächen wie [Kibana](https://www.elastic.co/kibana/)
komfortabel durchforstet werden können.  Oder Riemann kann Metriken in
Zeitreihendatenbanken wie [InfluxDB](https://www.influxdata.com/) schreiben;
diese Zeitreihen können dann durch Benutzeroberflächen wie
[Grafana](https://grafana.com/) visualisiert werden oder können auch benutzt
werden, um bei Fehlern und Problemen zu alarmieren.

Wir zeigen heute an einem Anwendungsbeispiel, wie man aus Logs mit Hilfe von
Riemann Metriken extrahieren kann.

<!-- more start -->

## Riemann

Riemann selbst ist in Clojure geschrieben und kann auch in Clojure konfiguriert
und programmiert werden.  Tatsächlich ist die Konfiguration einer
Riemann-Instanz ein Clojure-Programm, das mit Hilfe einer mächtigen und
effizienten Stream-Processing-Sprache die Eventverarbeitung steuert.

Riemann ermöglicht es, Events durch Streams zu schicken, die diese Events
filtern, verändern, kombinieren, aggregieren und projizieren können.  Es gibt
dutzende eingebaute Streams und es ist sehr einfach, eigene Streams zu
schreiben, da ein Stream lediglich eine Funktion ist, die ein Event akzeptiert.

## Events

Events sind in Riemann als Maps repräsentiert, also als eine Menge von
Schlüssel-Wert-Paaren, wie zum Beispiel dieses Event, das eine Logzeile einer
Anfrage an einen Webserver repräsentiert:

```clojure
(def request-event
   {:timestamp   1663577804126
    :method      "GET"
    :request     "/index.html"
    :requestor   "192.168.1.23"
    :transaction "uid-82a9dda829"
    :service     "webserver-request"
    :host        "192.168.1.1"
    :time        1})
```

Wir binden das Event an den Namen `request-event`, um es im Folgenden für unsere
Tests verwenden zu können.  Riemann definiert einige besondere Felder wie zum
Beispiel `:host` und `:service`, die in allen Events vorkommen; ansonsten gibt
es keine Einschränkung, welche Felder und Werte ein Event enthalten kann.

## Streams

Ein einfacher Stream, der zum Beispiel Logevents von Metriken trennt und
Logevents zur Langzeitspeicherung an einen Elasticsearch-Server und Metriken an
eine InfluxDB-Zeitreihendatenbank weiterleitet, sieht so aus:

```clojure
(streams
  (where
    (metric nil)
    elasticsearch-stream
    (else
      influxdb-stream)))
```

Ein `where`-Stream überprüft, ob durchfließende Events auf bestimmte Prädikate
passen, hier bedeutet das Prädikat `(metric nil)`, dass ein Event kein Feld
`:metric` enthält, was in unserem Beispiel wiederum bedeutet, dass ein Event
keine Metrik ist (die Event-Taxonomie von Riemann nimmt an, dass sich Metriken
durch die Existenz des Felds `:metric` von anderen Eventtypen unterscheiden).
In diesem Fall fließt das Event an einen `elasticsearch-stream` weiter, der es
schließlich an den Elasticsearch-Server schickt.  Im `else`-Zweig finden sich
nur noch Metriken, die Riemann dann über den `influxdb-stream` an die InfluxDB
weiterleitet.

Die Definitionen von `elasticsearch-stream` und `influxdb-stream` sehen so aus:

```clojure
(def elasticsearch-stream
  (tap :elasticsearch-stream elasticsearch))

(def influxdb-stream
  (tap :influxdb-stream influxdb))
```

Das `tap` in jeder dieser Definitionen ist ein Konstrukt, um Events abzugreifen,
um sie für die Testfälle zu inspizieren.  Davon werden wir gleich Gebrauch
machen.

Die tatsächlichen Definitionen von `elasticsearch` und `influxdb` würden diesen
Blogartikel sprengen, daher verlassen wir uns einfach auf Wunschdenken und gehen
davon aus, dass sie das machen, was sie sollen: die ankommenden Events nach
jeweils Elasticsearch beziehungsweise InfluxDB weiterleiten.  Für besonders
Interessierte sei gesagt, dass wir für diese Streams die Implementierung aus
unserer Riemann-Bibliothek
[`active-riemann`](https://github.com/active-group/active-riemann) benutzen.

## Tests

Unit-Tests sind unverzichtbar; auch unsere Stream-Definition müssen wir testen.
Riemann bringt Unterstützung für Unit-Tests mit, wir können Tests mit `riemann
test` auf der Kommandozeile laufen lassen.  Die Tests müssen wir aber erstmal
schreiben.  Der erste Test stellt sicher, dass Events, die keine Metriken sind,
nur im Elasticsearch landen:

```clojure
(deftest event-test
  (let [actual (inject! [request-event])]
    (is (= [request-event] (:elasticsearch-stream actual)))
    (is (empty? (:influxdb-stream actual)))))
```

Die Funktion `inject!` gibt es nur, um die Streams im Rahmen von Unit-Tests zu
füttern.  Sie schickt eine Liste von Events in die definierten Streams.  Hier
ist in der Liste nur ein Event, das weiter oben definierte `request-event`.  Der
Rückgabewert von `inject!` enthält alle Events, die an den Stellen, die wir mit
`tap` markiert haben, vorbeigekommen sind.  Da wir unseren Stream nach
Elasticsearch mit `:elasticsearch-stream` belauschen, erwarten wir, dass wir
genau dieses eine Event dort finden, weil unsere Streamdefinition es dort
vorbeigeleitet hat.  Und da wir keine Metrik injiziert haben, erwarten wir, dass
an `:influxdb-stream` keine Events vorbeigekommen sind, daher muss diese Liste
leer sein, also auf das Prädikat `empty?` passen.

Für den nächsten Test wollen wir eine Metrik injizieren, also erstellen wir eine
Beispielmetrik.  Dafür bauen wir uns zuerst eine Hilfsfunktion, in der wir das
Aussehen unserer Metrik abstrahieren.  Die Metrik hat ein Feld `:label` für den
Bezeichner und ein Feld `:metric` für den metrischen Wert, den wir der
Hilfsfunktion übergeben können:


```clojure
(defn make-webserver-transaction-metric
  [duration]
  {:label  "webserver-transaction-duration-milliseconds"
   :metric duration})
```

Dann erzeugen wir unsere Beispielmetrik mit dem Wert 116:

```clojure
(def example-metric (make-webserver-transaction-metric 116))
```

Für den nächsten Testfall injizieren wir diese Beispielmetrik:

```clojure
(deftest metric-test
  (let [actual (inject! [example-metric])]
    (is (empty? (:elasticsearch-stream actual)))
    (is (= [example-metric] (:influxdb-stream actual)))))
```

Hier erwarten wir, dass an `:elasticsearch-stream` kein Event vorbeigekommen
ist, dafür aber die Beispielmetrik bei `:influxdb-stream` auftaucht.  Alle Tests
laufen durch.

## Webserver-Logs

Für unser Anwendungsbeispiel nehmen wir an, dass auf einem ausgedachten
Webserver Logs anfallen.  Die Anfragen schaffen es in dem oben beschriebenen
Format in unser Riemann-System.[^1] Außer den Anfragen loggt der Webserver auch
noch die Auslieferung der Antwort.  Ein Event dafür sieht so aus:

```clojure
(def reply-event
  {:timestamp   1663577804242
   :transaction "uid-82a9dda829"
   :service     "webserver-reply"
   :host        "192.168.1.1"})
```

Der Webserver verknüpft Anfrage und Antwort mit einer eindeutigen
Transaktions-ID im Feld `:transaction`.

[^1]: Zum Beispiel eignet sich [Logstash](https://www.elastic.co/logstash/), um
      Logdateien einzulesen, aufzubereiten und an Riemann weiterzuleiten.

## Transaktionszeit bestimmen

Für unseren Anwendungsfall interessiert uns, wie lange der Webserver braucht, um
eine Anfrage zu beantworten.  Diese Metrik wollen wir messen und festhalten, um
die Performance des Webservers überwachen zu können.  Das bedeutet, dass wir die
verstrichene Zeit zwischen den Events der Anfrage und der Antwort einer
Transaktion ermitteln müssen und zur Aufbewahrung, späteren Visualisierung oder
gegebenenfalls auch für Alarme in unsere InfluxDB schreiben.  Wenn wir die
Zeitstempel aus den `:timestamp`-Feldern voneinander abziehen, erhalten wir die
Transaktionszeit in Millisekunden.  Für unser Beispiel ergibt sich eine
Transaktionszeit von 116 Millisekunden.

Entscheidend dabei ist, dass wir nur den Abstand der zusammengehörenden
Logeinträge berechnen, also die Logeinträge finden müssen, die dieselbe
Transaktions-ID haben.  Konkret bedeutet das, dass wir uns die Anfragen so lange
merken müssen, bis wir die passende Antwort sehen, sprich die Antwort mit
derselben Transaktions-ID wie die Anfrage.  Sich etwas zu merken bedeutet
Zustand.  Riemann bringt in Form eines *Index* Unterstützung für das Speichern
von Zuständen mit.

## Index

Der Index ist ein in Riemann eingebauter Speicher für Events.  Riemann speichert
für jedes Tupel aus `:host` und `:service` das jeweils letzte gesehene Event.
Diese Events können wir vom Index abfragen, außerdem haben die Events im Index
ein Verfallsdatum: Das Feld `:ttl` legt die "time-to-live" für ein Event im
Index in Sekunden fest.  Und wir können festlegen, in welchen Abständen
gestorbene Events gelöscht werden.  Riemann löscht diese Events nicht nur,
sondern informiert auch noch über diese abgelaufenen Events -- diese Information
können wir für unser Vorhaben auch sinnvoll nutzen.

Wir initialisieren unseren Index wie folgt:

```clojure
(def default-ttl 60)

(def index
  (do
    (periodically-expire (/ default-ttl 10))
    (index)))

(def index-stream
  (sdo
   (tap :index identity)
   (default :ttl default-ttl index)))
```

Zunächst definieren wir unsere Standard-Verfallszeit `default-ttl` auf 60
Sekunden; einen neuen Index binden wir an den Namen `index` und definieren mit
`periodically-expire`, dass Riemann alle sechs Sekunden nach abgelaufenen Events
schauen soll.  Den neuen Index verpacken wir in einen Stream mit dem Namen
`index-stream`.  Genaugenommen ist unser `index-stream` eine `sdo`-Komposition
von zwei Streams: `sdo` ist `do` für Streams, also fließen Events in alle
Streams im Rumpf.  Hier durchläuft ein ankommendes Event die Streams `(tap
:index identity)` und `(default :ttl default-ttl index)`:

- Der Stream `(tap :index identity)` ist nur dazu da, um unseren Index-Stream
  für unsere Unit-Tests belauschen zu können.

- Der Stream `(default :ttl default-ttl index)` versieht jedes Event, das noch
  kein Feld `:ttl` enthält, mit einem Standardwert für das Feld `:ttl`, und zwar
  mit dem Wert unserer definierten Standard-Lebenszeit.  Anschließend werden
  diese Events dann an den Index weitergereicht, um sie dort abzuspeichern.

## In den Index schreiben

Wir brauchen einen Stream, der ein Anfrage-Event in den Index schreibt.  Wir
nennen den Stream `store-requests-stream`:

```
(def store-requests-stream
  (smap (fn [request]
          (clojure.set/rename-keys request {:transaction :service}))
        index-stream))
```

Der Stream benutzt einen `smap`-Stream, um die angegebene Funktion auf jedes
Event -- sprich auf jede Anfrage -- anzuwenden und das Ergebnis dann in den
`index-stream` weiterzuschicken.  Die Funktion, die `smap` anwendet, baut eine
Anfrage so um, dass der Index sie auch richtig speichern kann: Wie oben erwähnt,
speichert der Index ein Event pro Tupel aus `:host` und `:service`.  Damit wir
wirklich alle Events speichern können, benennen wir in der Anfrage einfach das
Feld `:transaction` in `:service` um und haben so die eindeutige Transaktions-ID
mit als Teil des Schlüssels im Index.  Der ursprüngliche Wert von `:service`
interessiert uns nicht weiter.  Ein indiziertes Event sieht zum Beispiel so aus:

```clojure
(def request-event
  {:timestamp   1663577804126
   :method      "GET"
   :request     "/index.html"
   :requestor   "192.168.1.23"
   :service     "uid-82a9dda829"
   :host        "192.168.1.1"
   :time        1})
```

## Aus dem Index lesen

Kommen Antwort-Events an, müssen wir zugehörige Anfragen aus dem Index lesen.
Diesen Stream nennen wir `lookup-requests-and-calculate-metric-stream`; der
Stream erwartet Antwort-Events:

```
(def lookup-requests-and-calculate-metric-stream
  (smap (fn [reply]
          (when-let [request (riemann.index/lookup index
                                                   (:host reply)
                                                   (:transaction reply))]
            (riemann.index/delete index request)
            (make-webserver-transaction-metric (- (:timestamp reply)
                                                  (:timestamp request)))))
        reinject))
```

Dieser Stream benutzt ebenfalls einen `smap`-Stream, um eine Funktion auf jedes
Antwort-Event aufzurufen und dann mittels des `reinject`-Streams das Ergebnis
davon wieder in unsere Streams zu reinjizieren, also wieder vorne in unsere
Streams hineinzuschieben.

Im Rumpf dieser Funktion passiert einiges: Eine möglicherweise gespeicherte
Anfrage versucht `riemann.index/lookup` anhand der Felder `:host` und
`:transaction` der Antwort zu finden.  Wenn das glückt, entfernt die Funktion
das gefundene Event aus dem Index, damit es später nicht ablaufen kann -- es ist
ja schließlich schon verarbeitet.  Das Ergebnis dieser Funktion ist eine Metrik,
die wir mit `make-webserver-transaction-metric` erzeugen und die als Wert die
Differenz der Zeitstempel zwischen Anfrage und Antwort berechnet.  Ist eine
passende Anfrage nicht im Index, zum Beispiel weil wir unser Programm gestartet
haben, nachdem diese Anfrage gekommen wäre, gibt die Funktion `nil` zurück.  Das
hat dann den beabsichtigten Effekt, dass dieses `nil` eben nicht reinjiziert
wird, da `smap` den Wert `nil` nicht an die Streams weitergibt.

## Den Index sauberhalten

Damit sich keine Anfragen ansammeln, zu denen der Webserver -- aus welchen
Gründen auch immer -- nie eine Antwort rausgeschickt hat, haben wir jede Anfrage
im Index mit einer maximalen Lebenszeit versehen.  Wenn Riemann solche
abgelaufenen Anfragen aus dem Index entfernt, reinjiziert es eine Information
darüber in die Streams, in Form der Anfrage, aber zusätzlich mit einem Feld
`:state` mit dem Wert `expired` markiert.  Mit dem Prädikat `(state "expired")`
kann Riemann diese Events filtern -- und wir können diese Information nutzen.

Im Falle eines solchen Timeouts wollen wir eine Metrik ausgeben, deren Wert sehr
groß sein soll, sprich also `Double/MAX_VALUE` in unserem Zahlenraum.  Die
Metrik nennen wir `timeout-metric`:

```clojure
(def timeout-metric
  (make-webserver-transaction-metric Double/MAX_VALUE))
```

Den Stream dafür nennen wir `emit-timeout-metric-stream`:

```clojure
(def emit-timeout-metric-stream
  (smap (constantly timeout-metric) reinject))
```

Der `smap`-Stream reinjiziert für jeden eingehenden Wert, unabhängig von der Art
des Werts, eine Metrik mit `Double/MAX_VALUE` in die Streams.

## Nicht die Ströme kreuzen

Damit haben wir alle Bestandteile zusammen; nun können wir unseren
ursprünglichen Stream um ein paar Weichen ergänzen, um auf unsere gewünschte
Funktionalität zu kommen:

```clojure
(streams
 (where
  (metric nil)
  (sdo
   (where (not (state "expired")) elasticsearch-stream)
   (split
    (service "webserver-request") store-requests-stream
    (service "webserver-reply") lookup-replies-and-calculate-metric-stream
    (state "expired") emit-timeout-metric-stream))
  (else
   influxdb-stream)))
```

Der Fall für Metriken bleibt unverändert.  Der Fall von Events, die keine Metrik
sind, wird komplizierter: Das `sdo` markiert, dass die Events an mehrere Streams
weitergegeben werden sollen.  Zum einen an den Stream, der ans Elasticsearch
weiterschickt, allerdings wollen wir die Informationen über die abgelaufenen
Anfragen nicht dort sehen, daher filtern wir die mit `(not (state "expired"))`
aus.  Ansonsten sollen alle Anfragen und alle Antworten im Elasticsearch landen.

Der nächste Stream implementiert die Berechnung der Transaktions-Metriken.  Wir
spalten in einem `split`-Stream den Eventstrom in Anfragen mit dem Prädikat
`(service "webserver-request")`, in Antworten mit dem Prädikat `(service
"webserver-reply")` und in abgelaufene Events mit dem Prädikat `(state
"expired")`auf:

- Die Anfragen landen über den oben definierten `store-requests-stream` im
  Index.

- Die Antworten führen via `lookup-replies-and-calculate-metric-stream` und die
  zuvor im Index gespeicherten Anfragen zu Metriken.

- Die abgelaufenen Anfragen führen zu Metriken, die den Timeout markieren.

### "Wieso nicht?" -- "Das wäre schlecht!"

Zwei Unit-Tests sollen unsere Implementierung absichern.  Der Erste testet eine
komplette Transaktion bestehend aus Anfrage und Antwort:

```clojure
(deftest transaction-test
  (let [actual (inject! [request-event reply-event])]
    (is (= [request-event reply-event]
           (:elasticsearch-stream actual)))
    (is (= [indexed-event]
           (:index actual)))
    (is (= [example-metric]
           (:influxdb-stream actual)))))
```

Der Test injiziert unsere Beispielevents für Anfrage `request-event` und Antwort
`reply-event`.  Unsere Erwartungen:

- Beide Events werden an Elasticsearch geschickt.
- Die Anfrage landet in abgewandelter Form als indiziertes Event `indexed-event`
  im Index.
- Die berechnete Metrik `example-metric` erreicht die InfluxDB.

Der zweite Test überprüft das Verhalten im Timeout-Fall:

```clojure
(deftest transaction-expired-test
  (let [actual (do
                 (inject! [request-event])
                 (riemann.time.controlled/advance! (* 2 default-ttl))
                 (inject! [reply-event]))]
    (is (= [request-event reply-event]
           (:elasticsearch-stream actual)))
    (is (= [indexed-event]
           (:index actual)))
    (is (= [timeout-metric]
           (:influxdb-stream actual)))))
```

Hier injizieren wir zunächst nur die Anfrage `request-event` und stellen dann
für diesen Test die Uhr mit `riemann.time.controlled/advance!` so weit vor, dass
die Anfrage im Index auf jeden Fall abgelaufen ist.  Erst dann schicken wir die
Antwort `reply-event`, die dann aber keine passende Anfrage mehr findet.  Daher
sehen wir zwar im Elasticsearch sowohl Anfrage- als auch Antwort-Event, und auch
im Index kam mal das indizierte Antwort-Event vorbei, aber die einzige Metrik,
die es in die InfluxDB geschafft hat, ist die Timeout-Metrik `timeout-metric`.
Zusätzlich zum Verhalten im Timeout-Fall deckt dieser Unit-Test sogar das
Verhalten in dem Fall ab, dass für ein Antwort-Event keine Anfrage im Index
vorhanden ist.

Damit haben wir alle Zweige unserer Streams abgetestet und das Ziel erreicht:
Wir haben mit Riemann Metriken aus eingehenden Events abgeleitet.

## Fazit

Riemann ist performant und effizient in der Ausführung -- unsere produktiv
laufenden Systeme verarbeiten trotz großer und komplizierter Stream-Logik
mehrere tausend Events pro Sekunde.

Und Riemann ist elegant zu programmieren: Mit der umfangreichen
Stream-Processing-Sprache lassen sich die typischen Aufgaben in der
Eventverarbeitung wie Filtern, Anreichern, Kombinieren, Aggregieren und
Projizieren einfach erledigen.  Und für alle weiteren Aufgaben können wir die
Stream-Processing-Sprache flexibel erweitern.

### "Totale Protonenumkehr!"

<!-- more end -->
