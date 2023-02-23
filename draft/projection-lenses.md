---
layout: post
title: "Datenkonvertierung mit Linsen"
author: marcus-crestani
tags: ["Praxis", "Linsen", "Records", "EDN", "Transformation", "Konvertierung", "bidirektional"]

---

Linsen sind ein wichtiges Konzept in der funktionalen Programmierung mit großem
praktischen Nutzen.  Vor einigen Jahren haben wir [funktionale
Linsen](https://funktionale-programmierung.de/2014/10/15/funktionale-linsen.html)
hier im Blog bereits vorgestellt.  Seitdem haben wir die Benutzung von Linsen in
unserer täglichen Arbeit stets ausgebaut.  Heute wollen wir zeigen, wie wir
Linsen als bidirektionale Transformationen nutzen und wie sie uns dadurch beim
Umwandeln von Datenrepräsentationen unterstützen.

Das Beispiel für diesen Artikel implementieren wir mit unserer umfangreichen und
frei verfügbaren Clojure-Bibliothek namens [Active
Clojure](https://github.com/active-group/active-clojure), die wir in allen
unseren Clojure-Projekten benutzen.  In dieser Bibliothek gibt es eine
Implementierung für Linsen und für
[Records](https://funktionale-programmierung.de/2015/04/27/clojure-records.html),
mit denen wir zusammengesetzte Daten in Clojure modellieren können.  Die
Kombination von Linsen und Records ist besonders hilfreich.

<!-- more start -->

## Datenstrukturen

Jedes Programm verarbeitet Daten, die Strukturierung dieser Daten und das Finden
von geeigneten Datenmodellen gehört zu den wichtigsten Aufgaben guter und
erfolgreicher Softwareentwicklung.  Datenstrukturen möchten wir in unseren
Programmen zu Typen machen, für zusammengesetzte Daten eignen sich Records.
Betrachten wir als Beispiel eine Datendefinition für eine Lesezeichen-Verwaltung
eines Webbrowsers, die Beschreibungen mit URLs verknüpft.  Eine URL besteht aus
einem Protokoll, einem Hostnamen, einer optionalen Portnummer und einem Pfad:

```clojure
(define-record-type URL
  make-url
  url?
  [protocol url-protocol
   host url-host
   port url-port
   path url-path])
```

Obige Recorddefinition liefert einen Konstruktor mit namen `make-url`, der als
Argumente Werte für die vier Felder `protocol`, `host`, `port`, und `path`
erwartet.  Außerdem liefert die Recorddefinition ein Prädikat `url?`, das
überprüft, ob ein Argument vom Typ `URL` ist und Selektoren für die vier Felder
`url-protocol`, `url-host`, `url-port` und `url-path`.  So bindet der Ausdruck

```clojure
(def url-1 (make-url "https" "funktionale-programmierung.de" 443 "/"))
```

eine Instanz einer URL für die Internetadresse unseres Blogs an den Namen
`url-1`.  Zugriff auf das Feld `host` geht mit dem Selektor

```clojure
(url-host url-1)
```

und gibt

```clojure
"funktionale-programmierung.de"
```

zurück.

### Records und Linsen

Tatsächlich sind die Selektoren aber nicht nur einfache Funktionen, welche die
Werte der Felder liefern, sie sind Linsen, die diese Werte fokussieren.  Aus
Linsen kann man mit `yank` den fokussierten Wert auslesen, also kann man für den 
obigen Zugriff auch

```clojure
(lens/yank url-1 url-host)
```

schreiben.  Da jede Linse ein implizites `lens/yank` macht, wenn man sie auf ein
Argument anwendet (nämlich der Datenstruktur), werden wir im Folgenden auch für
alle anderen Linsen `lens/yank` weglassen und die kompaktere,  klarere
Selektor-Schreibweise benutzen.

Mit Linsen kann man die fokussierten Werte in Datenstrukturen auch mit `shove`
verändern:

```clojure
(lens/shove url-1 url-host "active-group.de")
```

Obiger Ausdruck liefert einen URL-Record zurück, der auf die Homepage der Active
Group zeigt. (das Objekt/der Wert `url-1` wird dabei nicht verändert).

Analog zu `yank` gibt es auch für `shove` eine kompaktere Schreibweise für
Linsen.  Dafür wendet man eine Linse auf zwei Argumente an: Die Datenstruktur und
den neuen Wert:

```clojure
(url-host url-1 "active-group.de")
```

ist synonym zum obigen `shove`-Ausdruck.

### Lesezeichen

Weiter geht es nun mit der Datendefinition unserer Lesezeichenverwaltung.  Ein
Lesezeichen ist ein Link, der aus einer Beschreibung und einer URL besteht:

```clojure
(define-record-type Link
  make-link
  link?
  [description link-description
   url link-url])
```

Und unsere Lesezeichenverwaltung besteht aus einer Liste aller unserer
Lesezeichen:

```clojure
(define-record-type Bookmarks
  make-bookmarks
  bookmarks?
  [bookmarks bookmarks-bookmarks])
```

Damit können wir uns folgende Lesezeichen definieren:

```clojure
(make-bookmarks
 [(make-link "Größter deutschsprachiger FP-Blog"
             (make-url "https" "funktionale-programmierung.de" 443 "/"))
  (make-link "Entwicklung funktionaler Software"
             (make-url "https" "active-group.de" 443 "/"))])
```

## Datenkonvertierung

Datenkonvertierung nennt man das Umwandeln von strukturierten Daten in ein
anderes Datenformat, üblicherweise in ein einfacher serialisierbares Format, das
sich besser zur Übertragung oder zur Speicherung eignet.  Das braucht man beim
Schreiben von Programmen sehr häufig.  Bei der heutzutage weit verbreiteten
Client-Server-Architektur ist ständige Kommunikation und Datenübertragung
zwischen Client und Server nötig, daher ist man als Softwareentwickly in der
täglichen Arbeit ständig damit beschäftigt, Datenrepräsentationen umzuwandeln.
Das ist oft nervig, bedeutet viel Tipparbeit und ist fehleranfällig.

Für unsere Lesezeichenverwaltung eignet sich zum Beispiel [Extensible Data
Notation (kurz: EDN)](https://github.com/edn-format/edn) als geeignetes
Datenformat für das Speichern oder Übertragen der Informationen.  Das könnte so
aussehen:

```clojure
{:bookmarks [{:description "Größter deutschsprachiger FP-Blog"
              :url         {:protocol "https"
                            :host     "funktionale-programmierung.de"
                            :port     443
                            :path     "/"}}
             {:description "Entwicklung funktionaler Software"
              :url         {:protocol "https"
                            :host     "active-group.de"
                            :port     443
                            :path     "/"}}]}
```

## Projektionslinsen

Viel einfacher wird die Implementierung von Datenkonvertierungen mit
*Projektionslinsen*.  Projektionslinsen sind Linsen, die zwei Linsen miteinander
verbinden und so Werte in verschiedenen Datenformaten miteinander verbinden
können.  Ein einfaches Beispiel hilft für das Verständnis dieser Idee:

Nehmen wir zwei einfache EDN-Datenstrukturen:

- eine Map

```clojure
{:a 23
 :b 42}
```

- und einen Vektor

```clojure
[23 42]
```

Die Keywords `:a` und `:b` sind gleichzeitig Linsen, die in der Beispielmap die
jeweiligen Werte fokussieren; Linsen auf die Elemente des Vektors gehen über den
Index im Vektor, also hier `(lens/at-index 0)` für das erste und `(lens/at-index
1)` für das zweite Element.

Eine Projektionslinse, die den Wert der Map für den Schlüssel `:a` an die erste
Stelle des Vektors und den Wert für `:b` an die zweite Stelle des Vektors
projiziert verbindet die entsprechenden Linsen und sieht so aus:

```clojure
(def map->vector
  (lens/projection [] {(lens/at-index 0) :a
                       (lens/at-index 1) :b}))
```

Die Zuordnung, welche Linsen aufeinander abbilden, übergeben wir als Map.  Hier
sieht das aufmerksame Lesy, dass Projektionslinsen eine Richtung haben und ein
neutrales Element für die Ziel-Datenstruktur brauchen.  Hier ist die Richtung
von `yank` von der Map zum Vektor, daher der leere Vektor als neutrales Element.
Durch das Umdrehen der Zuordnungen und das Anpassen des neutralen Elements
können wir die Richtung im `lens/projection`-Aufruf ändern.  Alternativ gibt es
eine Linse `invert`, die eine Linse umdreht.  Eine umgedrehte Linse braucht auch
ein neutrales Element für das Ziel-Datenformat:

```clojure
(def vector->map (lens/invert map->vector {}))
```

Die Konvertierung der Map in den Vektor entspricht dann also einem `yank` dieser
Linse auf der Map:

```clojure
(map->vector {:a 23 :b 42})
```

liefert `[23 42]`.

Und `shove` auf einer leeren Map und dem Vektor oder ein `yank` auf der
invertierten Linse konvertiert wieder zurück in die Map-Repräsentation `{:a 23
:b 42}`:

```clojure
(map->vector {} [23 42])
(vector->map [23 42])
```

### Projektionslinsen für Records

Records können passende Projektionslinsen automatisch generieren, damit die
Benutzung noch einfacher wird.  Das geht mit dem optionalen Argument
`:projection-lens` bei einer Record-Definition, das den Namen der Bindung für
die generierte Projektionslinse festlegt:

```clojure
(define-record-type URL
  {:projection-lens into-url-projection-lens}
  make-url
  url?
  [protocol url-protocol
   host url-host
   port url-port
   path url-path])
```

Die Projektionslinse `into-url-projection-lens` kennt bereits die Linsen der
zugehörigen Record-Felder.  Diese Felder können wir mit den Linsen unserer
EDN-Datenstruktur verbinden, in dem wir sie in der richtigen Reihenfolge --
passend zu der Reihenfolge der Felder und der Reihenfolge im Konstruktor-Aufruf
-- an `into-url-projection-lens` übergeben:

```clojure
(def edn->url (into-url-projection-lens :protocol :host :port :path))
```

Diese Linse können wir jetzt für die Konvertierung zwischen
Record-Repräsentation und EDN-Repräsentation benutzen; die
Record-Projektionslinsen sind mit dem Record als Ziel definiert, gehen also hier
in unserem Fall von der EDN-Datenstruktur aus, also liefert ein `yank` auf die
Linse

```clojure
(edn->url {:protocol "https"
           :host     "funktionale-programmierung.de"
           :port     443
           :path     "/"})
```

die Record-Datenrepräsentation:

```clojure
(make-url "https" "funktionale-programmierung.de" 443 "/")
```

und `shove` in die leere Map konvertiert den Record zurück in das EDN-Format:

```clojure
(edn->url {} (make-record "https" "funktionale-programmierung.de" 443 "/"))
```

Das alles geht natürlich auch für verschachtelte Datenstrukturen.  Hier sind die
noch fehlenden Definitionen für den Rest des Lesezeichen-Beispiels, zunächst für
ein Lesezeichen:

```clojure
(define-record-type Link
  {:projection-lens into-link-projection-lens}
  make-link
  link?
  [description link-description
   url link-url])

(def edn->link
  (into-link-projection-lens :description
                             (lens/>> :url edn->url)))
```

Die richtige Konvertierung verschachtelter Datenstrukturen erreichen wir durch
das hintereinanderschalten von Linsen mit Hilfe des `lens/>>`-Operators.  Hier
verknüpfen wir `:url` mit der oben definierten Projektionslinse für URLs
`edn->url`.

Jetzt fehlt noch die Liste der Lesezeichen:

```clojure
(define-record-type Bookmarks
  {:projection-lens into-bookmarks-projection-lens}
  make-bookmarks
  bookmarks?
  [bookmarks bookmarks-bookmarks])

(def edn->bookmarks
  (into-bookmarks-projection-lens (lens/>> :bookmarks (lens/mapl edn->link))))
```

Hier erreichen wir die korrekte Konvertierung der verschachtelten
Datenstrukturen mit einer weiteren Kombination von Linsen: Zunächst schalten wir
mit `lens/>>` hintereinander und dann benutzen wir `lens/mapl`, einen
Linsen-Kombinator der die übergebene Linse auf alle Elemente einer Liste
anwendet.[^2]

[^2]: Durch die Möglichkeit, beliebige Linsenkombinatoren zu benutzen, ist der
    tatsächlichen Konvertierung keine Grenze gesetzt.  Ein weiterer in der
    Praxis nützlicher Linsenkombinator `lens/xmap`, damit ist es beispielsweise
    möglich, Werte in andere Repräsentationen und zurück zu transformieren, zum
    Beispiel verschiedene Datums- und Uhrzeit-Formate.

Damit haben wir alle Bestandteile zusammen und können mit der elegant
definierten Linse `edn->bookmarks` unsere komplette Datendefinition zwischen
Records und EDN umwandeln.

## Gemischte Daten

Unsere Lösung ist erweiterbar: Als neues Feature unserer Lesezeichenverwaltung
wollen wir Ordner zur besseren Strukturierung einführen.  Wir wollen Ordner
überall dort erlauben, wo wir bisher einzelne Lesezeichen erlauben.  Wir haben
nun also gemischte Daten: Elemente einer Lesezeichenliste können sowohl
Links als auch Ordner sein.

Ein Ordner besteht aus einem Namen und einer Liste von Lesezeichen:

```clojure
(define-record-type Folder
  {:projection-lens into-folder-projection-lens}
  make-folder
  folder?
  [name folder-name
   bookmarks folder-bookmarks])
```

Auch innerhalb eines Ordners können alle Lesezeichen entweder Links und
wiederrum Ordner sein, also haben wir es hier mit einer gemischten und
verschränkt rekursiven Datendefinition zu tun.  Für den Umgang mit gemischten
Daten gibt es einen eingebauten Linsenkombinator `alt->edn`.  Und um die
verschränkte Rekursion müssen wir uns bei der Umwandlung in EDN nicht anders
kümmern, als wir es in Clojure sowieso tun müssen.  In unserer `bookmark->edn`-Linse
nehmen wir Bezug auf einen noch zu definierenden Wert `edn->folder`, daher
müssen wir den Namen `edn->folder` vorher deklarieren und die Auswertung des
Werts mit der Linse `defer` verzögern, `edn->folder` übergeben wir dann das
Clojure-Variablenobjekt:

```clojure
(declare edn->folder)

(def bookmark->edn
  (lens/alt->edn [link? (lens/invert edn->link)]
                 [folder? (lens/invert (lens/defer #'edn->folder))]))
```

Als Argumente bekommt `lens/alt->edn` eine Liste von Alternativen, wobei eine
Alternative ein Paar aus einem Prädikat und einer Linse ist, die zu dem
Datenformat passt, auf welches das Prädikat passt.  Die angegebene Linse benutzt
also eine Projektionslinse `link->edn` für Links, wenn das Prädikat `link?`
passt; und eine Projektionslinse `folder->edn`, wenn es auf das Prädikat
`folder?`  passt.  Da wir ausgehend von den Records projizieren, müssen wir die
Projektionslinsen invertieren:

```clojure
(def link->edn
  (lens/invert edn->link))

(def folder->edn
  (lens/invert edn->folder))
```

Die Linse `edn->link` bleibt unverändert zu oben, die Linse `edn->folder` fehlt
noch.  Dafür brauchen wir noch eine weitere Konvertierung, da in einem Ordner ja
wiederrum Lesezeichen aus Links und Ordnern sein können.  Deswegen definieren
wir zuerst eine Projektion dafür:

```clojure
(def edn->bookmarks-with-folders
  (into-bookmarks-projection-lens (lens/>> :bookmarks
                                           (lens/mapl (lens/invert bookmark->edn)))))
```

Und damit haben wir alle Teile zusammen, um die Projektionslinse des
`Folder`-Records für die Definition von `edn->folder` nutzen zu können:

```clojure
(def edn->folder
  (into-folder-projection-lens :name
                               (lens/>> :bookmarks
                                        (lens/mapl edn->bookmarks-with-folders))))
```

Das können wir nun ausprobieren: umwandeln in EDN und zurück in die
Record-Datenrepräsentation liefert den Ausgangswert:

```clojure
(edn->bookmarks-with-folders
 (bookmarks-with-folders->edn
  (make-bookmarks [(make-folder
                    "Blogs"
                    (make-bookmarks
                     [(make-link "Größter deutschsprachiger FP-Blog"
                                 (make-url "https" "funktionale-programmierung.de"
                                           443 "/"))]))
                   (make-link "Entwicklung funktionaler Software"
                              (make-url "https" "active-group.de" 443 "/"))])))
```

Wir haben die Erweiterung implementiert ohne vorher geschriebenen Code ändern zu
müssen.  Eine super Sache!

## Fazit

Projektionslinsen sind eine passende Abstraktion für Konvertierungen zwischen
verschiedenen Datenformaten.  Die Benutzung ist einfach, wenig fehleranfällig,
erweiterbar und gut wartbar und erleichtert somit die Softwareentwicklung.

### Tellerrand

Diese Art, Linsen miteinander zu kombinieren, um Daten zu tranformieren, ist eng
verwandt mit den [Pickler Combinators von Andrew
J. Kennedy](https://www.microsoft.com/en-us/research/wp-content/uploads/2004/01/picklercombinators.pdf).

Die vorgestellten Record-Projektionslinsen eignen sich auch hervorragend für die
Interaktion mit den Linsen in unserer Bibliothek für Konfigurationen
`active.clojure.config`.  Das werden wir in einem anderen Blogeintrag
vorstellen.

<!-- more end -->

