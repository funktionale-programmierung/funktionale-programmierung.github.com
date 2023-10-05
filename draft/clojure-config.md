---
layout: post
title: "Bibliothek für Konfigurationen"
author: marcus-crestani
tags: ["Konfiguration", "Clojure", "ClojureScript"]
---

Software muss konfigurierbar sein, um flexibel zu sein.  Aber wie stellen wir
sicher, dass eine Konfiguration vollständig und gültig ist?  Also dass alle
Aspekte, die konfiguriert werden müssen, auch konfiguriert sind?  Dass es
sinnvolle Voreinstellungen gibt für nicht explizit konfigurierte Werte?  Und
dass die Werte, die in der Konfiguration eingetragen sind, auch sinnvolle Werte
sind?

Um diese Fragen nicht für jedes Projekt neu zu beantworten, haben wir für
[Clojure](http://clojure.org/) und [ClojureScript](http://clojurescript.org/),
eine [Bibliothek für
Konfigurationen](https://github.com/active-group/active-clojure#configuration)
entwickelt, die wir seit vielen Jahren in der Praxis erfolgreich einsetzen --
und in diesem Artikel vorstellen.

<!-- more start -->

## Konfiguration

Nach typischer Clojure-Art sind Konfigurationen für uns verschachtelte
Key-Value-Maps.[^1] Solche Konfigurations-Maps bestehen aus Einstellungen
*Settings* und Abschnitten *Sections*.  Hier ein einfaches Beispiel für eine
Konfiguration einer Applikation mit einem Webserver:

[^1]: Key-Value-Maps, und somit auch unsere Konfigurationen, sind gültiges
    [EDN-Format](https://github.com/edn-format/edn), ein gängiges
    Datentransferformat im Clojure-Umfeld; das Speichern und Laden von
    Konfigurationsdateien in diesem Format geht daher mit Clojure-Bordmitteln
    `spit`, `slurp` und `read-string` -- darauf gehen wir in diesem Artikel
    nicht näher ein.

```clojure
{:log-level :info
 :webserver {:host "0.0.0.0"
             :port 80}}
```

Die Konfiguration enthält das Setting `:log-level` zur Konfiguration des
minimalen Logging-Levels der Applikation, das auf den Wert `:info` gesetzt ist;
außerdem die Section `:webserver` für die Konfiguration der Eigenschaften für
den Webserver.  Die Section `:webserver` ist wieder eine Konfigurations-Map mit
Settings `:host` für den Listen-Host (hier ist der Webserver durch die Angabe
von `"0.0.0.0"` von außerhalb erreichbar) und den Listen-Port, hier der
Standard-HTTP-Port `80`.

Hier kann man jetzt schon einen möglichen Fallstrick mit Konfigurationen
erahnen: Eine Applikation erwartet in der Regel ganz bestimmte konfigurierte
Werte, die sie dann interpretiert und passend darauf reagiert.  In unserem
Beispiel geht die Applikation also davon aus, dass der Wert für das
Logging-Levels tatsächlich ein Keyword wie zum Beispiel `:info` ist und nicht
zum Beispiel eine Zeichenkette `"info"` oder `"INFO"`.  Und der Webserver-Port
soll eine Zahl `80` sein und nicht eine Zeichenkette `"80"` und so weiter.

Und genau solche Einschränkungen können Programmiery mit unserer Bibliothek
festlegen und Konfigurationen dann auf diese Einschränkungen überprüfen und so
sicherstellen, dass die Applikation eine gültige und für sie verständliche
Konfiguration erhält.

## Konfigurationsschema

Die Applikation definiert dafür ein Konfiugrationsschema, welches sie erwartet.
So ein Konfigurationsschema heisst *Schema* in unserer Bibliothek.  Jetzt
definieren wir mal der Reihe nach die Settings, Sections und dann das Schema für
die obige Beispielkonfiguration.  Wir gehen in den Codebeispielen davon aus,
dass der Namespace `active.clojure.config` als `config` importiert ist.

Starten wir mit der Definition des Logging-Levels-Settings:

```clojure
(def log-level-setting
  (config/setting
   :log-level
   "Minimal log level, defaults to :error."
   (config/one-of-range #{:trace :debug :info :warn :error :fatal}
                        :error)))
```

Der Aufruf von `config/setting` erwartet als erstes Argument das Keyword der
Einstellung, hier ist das `:log-level`.  Dann folgt verpflichtend eine
Zeichenkette zur Dokumentation der Einstellung -- verständliche Beschreibungen
hier sind sehr hilfreich wenn die Konfiguration Fehler verursacht.  Und als
drittes Argument legen wir den gültigen Wertebereich der Einstellung fest -- in
unserer Bibliothek heißt ein Wertebereich *Range*.  Der Wertebereich für das
Loglevel ist eine `one-of-range`, also ist ein gültiger Wert dafür eines der
angegebenen Schlüsselwörter `:trace`, `:debug`, `:info`, `:warn`, `:error`, oder
`:fatal`.  Zusätzlich erlaubt die Range die Angabe einer Voreinstellung, wenn
das Setting nicht ausdrücklich konfiguriert ist.  Hier ist das `:error` -- in
der Dokumentation ist diese Tatsache auch hilfreich beschrieben.

Nach demselben Muster schreiben wir jetzt die Einstellungen, die der Webserver
braucht.  Zuerst für den Host:

```clojure
(def webserver-host-setting
  (config/setting
   :host
   "The address the webserver listens on, defaults to 0.0.0.0."
   (config/default-string-range "0.0.0.0")))
```

Die Einstellung für den Webserver-Host soll eine Zeichenkette sein, also eine
/String-Range/, und hier eine, die zusätzlich die Angabe einer Voreinstellung
`"0.0.0.0"` erlaubt, daher benutzen wir `default-string-range` um das
festzulegen.  Unsere Bibliothek enthält bereits eine vielzahl von Ranges für
häufig gebraucht Wertebereiche und es ist einfach, eigene Ranges für speziellere
Wertebereiche zu definieren.  Eine weitere eingebaute Range sehen wir bei der
nächsten Konfigurationseinstellung.

Die Definition der Konfigurationseinstellung für den Port sieht so aus:

```clojure
(def webserver-port-setting
  (config/setting
   :port
   "The port the webserver listens on, defaults to 3000."
   (config/integer-between-range 0 65535 3000)))
```

Den Port erwartet die Konfiguration als ganze Zahl, also eine /Integer-Range/;
aber es gibt in TCP-Netzwerken nur eine beschränkte Anzahl von Ports, nämlich
von 0 bis 65535, daher schränken wir den möglichen Wertebereich gleich darauf
mit `integer-between-range` ein.  Das dritte Argument für
`integer-between-range` ist die Voreinstellung, falls die Einstellung nicht
explizit gemacht wurde.  Hier benutzt unsere Applikation in diesem Fall den Port
3000.

Diese zwei Einstellungen machen nun den Webserver-Abschnitt aus -- daher bündeln
wir sie in einem Schema zusammen mit einer passenden Beschreibung:

```clojure
(def webserver-schema
 (config/schema
  "Configuration settings for the web server"
  webserver-host-setting
  webserver-port-setting))
```

Und mit diesem Schema können wir nun die Webserver-Section definieren:

```clojure
(def webserver-section
  (config/section
   :webserver
   webserver-schema))
```

Hier ist keine Beschreibung nötig, da die Beschreibung ja schon im Schema
steckt.

Jetzt haben wir alles beisammen, was wir für unser gesamtes Konfigurationsschema
brauchen:

```clojure
(def schema
 (config/schema
  "Configuration for our application"
  log-level-setting
  webserver-section))
```

## Validierung und Normalisierung

Damit können wir Konfigurationen anhand unseres Schemas nun auf Gültigkeit
überprüfen:

```clojure
(config/normalize&check-config-object
  schema
  {:log-level :info
   :webserver {:host "0.0.0.0"
               :port 80}})
```

Der Aufruf gibt die übergebene Konfiguration zurück, was bedeutet, dass die
übergebene Konfiguration gültig und vollständig ist.

Unvollständige Konfigurationen werden mit Voreinstellungen komplettiert: Der
Aufruf

```clojure
(config/normalize&check-config-object
  schema
  {:webserver {:host "0.0.0.0"}})
```

liefert die vervollständigte Konfiguration

```clojure
{:log-level :error
 :webserver {:host "0.0.0.0"
             :port 3000}}
```

Und eine fehlerhafte Konfiguration

```clojure
(config/normalize&check-config-object
  schema
  {:webserver {:port "80"}})
```

liefert eine Datenstruktur namens `RangeError`, die den Fehler in der
Konfiguration mittels Pfad zur fehlerhaften Konfigurationseinstellung, den
falschen Wert und den eigentlich erwarteten Wertebereich beschreibt.  Und so dem
Benutzy die Möglichkeit gibt, das Problem zu verstehen und zu beheben.

## Zugriff auf Konfigurationseinstellungen

Jetzt könnten wir die Einstellungen mit Hilfe der Settings-Keywords aus der
verschachtelten Konfigurations-Map auslesen.  Aber das ist keine besonders
robuste Vorgehensweise, da dem Clojure-Kompiler mögliche Tippfehler in den
Keywords gar nicht auffallen -- im Zweifel liefert so ein Zugriff auf einen
nicht-existenten Key einfach `nil` zurück.  Und das wiederrum könnte unsere
Applikation missverstehen oder durcheinander bringen -- was wir durch robuste
Konfiguration ja verhindern wollen.

Daher wollen wir gar nicht mit Keywords auf die Konfigurations-Map direkt
zugreifen, sondern wir wollen sicherere Mechanismen dafür nutzen, die es in
unserer Bibliothek gibt:

- Wir hantieren nicht mit der Map direkt, sondern mit einem speziellen Datentyp
  namens /Configuration/.

- Wir benutzen die an Variablen gebundenen Settings und Sections, um auf die
  Werte zuzugreifen -- damit bemerkt schon der Kompiler mögliche Tippfehler.

- Wir benutzen Funktionen für den Zugriff, die eine validierte Konfiguration
  sicherstellen.

Praktisch sieht das wie folgt aus, zunächst binden wir unsere
Beispiel-Configuration an einen Namen für unsere nächsten Versuche und
Erklärungen:

```clojure
(def c
  (config/make-configuration
    schema
    {:log-level :info
   :webserver {:host "0.0.0.0"
               :port 80}}))
```

Auf diese Configuration können wir nun mit Hilfe von `access` Werte zu
Einstellungen auslesen:

```clojure
(config/access c log-level-setting)
```

liefert `:info`.

Und aus verschachtelte Konfigurationen auslesen geht so:

```clojure
(config/access c webserver-port-setting webserver-section)
```

Dieser Aufruf liefert `80`.

Es gibt auch die Möglichkeit, eine ganze Section herauszulösen und damit die
Werte auszulesen:

```clojure
(let [webserver-config (config/section-subconfig c webserver-section)]
  (config/access webserver-config webserver-port-setting))
```

Das ermöglicht Konfiguration von verschiedenen Bausteinen der Applikation ohne
zu große Kopplung.

### Linsen auf Konfigurationseinstellungen

[Linsen](https://funktionale-programmierung.de/2014/10/15/funktionale-linsen.html)
spiele in unserer täglichen Arbeit -- und daher auch in diesem Blog -- eine
große Rolle, weil sie einen großen praktischen Nutzen haben.  Daher weiß
natürlich auch unsere Konfigurations-Bibliothek mit Linsen umzugehen, zum
Beispiel für den Zugriff auf Einstellungen:

```clojure
(let [log-level-lens (config/access-lens log-level-setting)]
  (log-level-lens c))
```

liefert wie oben der direkte Zugriff auch `:info`.  Die Linse können wir ohne
die Konfiguration konstruieren -- eine sehr elegante Art, Selektoren zu
schreiben.

Analog geht das auch für verschachtelte Konfigurationen:

```clojure
(let [webserver-port-lens (config/access-lens webserver-port-setting webserver-section)]
  (webserver-port-lens c))
```

Das liefert wie zu erwarten `80`.

### Projektion von Konfigurationseinstellungen

Kopplung vermeiden oder zumindest verringern ist das wichtigste Mantra für gute
Software.  Eine Methode zur Entkopplung sind verschiedene Datenstrukturen für
verschiedene Bereiche der Applikation, auch wenn sie sehr ähnlich sind.[^2]

[^2]: Aber da Software wächst und Anforderungen sich ändern, bleiben
    Ähnlichkeiten oft nicht erhalten.

Daher lohnt es sich in einer Software, die /Einstellungen/ von der
/Konfiguration/ zu trennen -- in dem man zum Beispiel eine Datenstruktur
einführt, die die Einstellungen repräsentiert, die aber nicht das
/Configuration/-Objekt ist.  Unsere Bibliothek macht uns das einfach durch ein
Zusammenspiel von
[Records](https://funktionale-programmierung.de/2015/04/27/clojure-records.html)
und
[Projektionslinsen](https://funktionale-programmierung.de/2023/02/28/projection-lenses.html).

Wir können nämlich die Einstellungs-Datenstruktur als Record definieren:

```clojure
(define-record-type Settings
  {:projection-lens settings-projection-lens}
  make-settings
  settings?
  [log-level settings-log-level?
   webserver-host settings-webserver-host
   webserver-port settings-webserver-port])
```

Und zwischen diesem Record und der Konfiguration eine Projektionslinse definieren:

```clojure
(def configuration->settings
  (settings-projection-lens
    (config/access-lens log-level-setting)
    (config/access-lens webserver-host-setting webserver-section)
    (config/access-lens webserver-port-setting webserver-section)))
```

Mit dieser kompakten Definition können wir nun eine Konfiguration in Settings
übersetzen[^3]:

```clojure
(configuration->settings c)
```

[^3]: Das geht sogar in die andere Richtung, wenn zum Beispiel die Applikation
    den Benutzys Änderungen an den Settings ermöglicht und diese wieder als
    Konfiguration in der Konfigurationsdatei persistieren will.

## Profile

Ein weiteres nützliches Feature ist die Unterstützung von Profilen.  Oft gibt es
Varianten in einer Konfiguration, die einer bestimmten Umgebung oder einem
bestimmten Deployment geschuldet sind.  Dabei hilft unsere Bibliothek.  Zum
Beispiel können wir ein Profil für Testumgebungen konfigurieren, das einige
Aspekte im Vergleich zur Produktivumgebung anpasst.  Dazu fügen wir in unserer
Beispielkonfiguration unter `:profiles` ein Profil namens `:test` hinzu.  Die
vollständige Konfiguration sieht dann so aus:

```clojure
{:log-level :info
 :webserver {:host "0.0.0.0"
             :port 80}
 :profiles
 {:test {:log-level :debug}}}
```

Wir wollen auch Debugging-Logs in der Testumgebung sehen.

Beim Einlesen der Konfiguration können wir dann die Einstellungen für dieses
Profil "reinmischen", die Profil-spezifischen Einstellungen überschreiben dann
die allgemeinen Einstellungen.  Wir rufen `normalize&check-config-object` noch
mit einer Liste von zu berücksichtigenden Profilen auf:

```clojure
(config/normalize&check-config-object
  schema
  [:test]
  {:log-level :info
   :webserver {:host "0.0.0.0"
               :port 80}
   :profiles
   {:test {:log-level :debug}}})
```

Das liefert dann die vervollständigte Konfiguration für die Testumgebung:

```clojure
{:log-level :debug
 :webserver {:host "0.0.0.0"
             :port 80}}
```

## Fazit

Eine wichtige Grundlage von flexiblen Applikationen sind robuste
Konfigurationen.  Die vorgestellte Bibliothek kümmert sich um den robusten
Umgang mit Konfigurationen.  Sie ist bei uns in allen unseren produktiven
Clojure-Projekten seit vielen Jahren erfolgreich im Einsatz.

<!-- more end -->

