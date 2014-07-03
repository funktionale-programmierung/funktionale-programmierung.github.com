---
layout: post
description: Web-Apps mit React/Reacl programmieren
title: "Web-Apps mit Reacl programmieren"
author: michael-sperber
tags: ["ClojureScript"]
---

Bei der [Active Group](http://www.active-group.de/) arbeiten wir an
mehreren Projekten mit Web-Frontends.  Dies ist inzwischen auch für
"normale" Applikationen eine echte Alternative zu den oft
umständlichen und eingeschränkten GUI-Toolkits von Java, .NET & Co, da
fast alle mit einem "Web-Widget" geliefert werden, in dem
JavaScript/HTML5-Anwendungen laufen können.  HTML5 und die reichlich
vorhandenen Frameworks für die Web-Programmierung bieten größeren
Gestaltungsspielraum als die traditionellen GUI-Toolkits und sind
außerdem leichter zu portieren.

Trotzdem macht die DOM-Programmierung mit Javascript (und auch mit
vielen Frameworks, die das eigentlich vereinfachen sollen) oft nicht
so richtig Freude.  Geändert hat sich das für uns mit dem
Open-Source-Release von Facebooks Framework
[React](http://facebook.github.io/react/), bei dem Ideen aus der
funktionalen Programmierung deutlich zu erkennen sind.  Um die
Entwicklung noch weiter zu vereinfachen, setzen wir für die
React-Programmierung
[ClojureScript](https://github.com/clojure/clojurescript) ein, und
zwar mit einem eigenen Wrapper für React namens
[Reacl](https://github.com/active-group/reacl), der ebenfalls als Open
Source verfügbar ist.  Um Reacl geht es in diesem Posting, das etwas
länger ausgefallen ist.

<!-- more start -->

# Das React/Reacl-Modell #

In einem
[vorherigen Posting]({% post_url 2014-02-14-clojurescript-react %})
haben wir bereits in ClojureScript und React eingeführt, aber es lohnt
sich, das React und Reacl zugrundeliegende Modell noch einmal Revue
passieren zu lassen.  Wer traditionelle Javascript-MVC-Frameworks
kennt, muss erstmal etwas umdenken -
[hier](http://www.reddit.com/r/programming/comments/1fak87/react_facebooks_latest_javascript_client_library/)
ist zu sehen, dass bei vielen Javascript-Entwicklern, die React das
erste mal sahen, Skepsis und teils offene Feindseligkeit vorherrscht.
Das täuscht hoffentlich nicht darüber hinweg, das React großartig ist.

In normalen MVC-Frameworks sind sowohl das Modell als auch der View
mutierbar:

<img src="/files/reacl/mvc.png"/>

Wenn also ein Benutzer durch eine Interaktion eine Änderung des
Modells veranlasst, dann muss der Controller die *entsprechende*
Veränderung im View veranlassen.  Das bringt zwei Probleme mit sich:

* Da Änderungen im Modell direkt Änderungen im View triggern,
  entstehen in der Anwendung komplexe Callback-Netzwerke, bei denen
  jede Änderung des Modells explizit an alle Views kommuniziert werden
  muss.  Das ist schwer zu verstehen, schwer korrekt hinzubekommen,
  schwer zu debuggen und schwer zu ändern.
* Da Änderungen im View ihrerseits Änderungen im Modell triggern
  können, entstehen oft Callback-Zykeln, die schwer zu brechen sind.
  
React vermeidet diese Probleme mit folgendem Bild:

<img src="/files/reacl/reacl.png"/>

Der View entsteht also durch die Anwendung einer Funktion auf das
*gesamte* Modell.  Die Anwendung muss also nicht mehr selbst
sicherstellen, dass die Änderung im View der Änderung im Modell
entspricht: Das macht React hinter den Kulissen.  Außerdem wird
der View nur neu generiert, wenn eine Runde Änderungen am Modell
abgeschlossen sind.  So können keine Zykeln entstehen.

Eine ausführlichere Beschreibung dieses Modells und der Vorteile in
der Entwicklung gibt es auch [auf
Video](http://www.youtube.com/watch?v=nYkdrAPrdcw&list=PLb0IAmt7-GS188xDYE-u1ShQmFFGbrk0v).
(Am besten gleich über das Marketing-Sprech am Anfang auf Minute 7 vorspulen.)

# Noch funktionaler mit ClojureScript und Reacl #

Während React trotz funktionaler Ideen noch viele imperative Aspekte
durchscheinen lässt, haben wir uns mit Reacl zum Ziel gesetzt, das
oben skizzierte Modell konsequent umzusetzen.

Dazu gehören zwei zentrale Ideen, die in Clojure-Programmen aufgrund
des Fokus auf [rein funktionaler Programmierung]({% post_url 2014-02-14-clojurescript-react %}) und
[Message-Passing](http://clojure.com/blog/2013/06/28/clojure-core-async-channels.html)
öfter anzutreffen sind:

* Der Zustand der gesamten Applikation sitzt *in einem einzigen
  Objekt*, das ausschließlich rein funktional manipuliert wird.
  Ändert sich der Zustand der Applikation, wird ein neues Objekt
  substituiert, ohne das alte Objekt zu mutieren.
* Zustandsänderungen werden durch das Schicken von *expliziten
  Nachrichten* veranlasst.

Wir benutzen Reacl zwar schon in Kundenprojekten, trotzdem haben wir
mit der Entwicklung erst vor kurzem angefangen, es sind also noch
Änderungen zu erwarten.  Dieses Posting bezieht sich auf Version
0.4.0.

# Reacl am konkreten Beispiel #

Dieser Abschnitt führt in Reacl anhand einer winzigen Todo-Applikation
ein: Neue Todos können in eine Liste eingetragen und dann abhakt
werden.   Das sieht so aus:

<img src="/files/reacl/todo.png"/>

Hier ist die Namespace-Deklaration, die für eine Reacl-Anwendung
typisch ist:

{% highlight clojure %}
(ns examples.todo.core
  (:require [reacl.core :as reacl :include-macros true]
            [reacl.dom :as dom :include-macros true]
            [reacl.lens :as lens]))
{% endhighlight %}

Das Beispiel landet mit dieser Deklaration in einem Namespace namens
`examples.todo.core`.  Außerdem importiert es Funktionen und Makros
aus dem Namespace `reacl.core` mit Präfix `reacl/`
(ClojureScript-Makros werden in Clojure gesondert programmiert, darum
muss man sie mit `:include-macros` auch extra einbinden), aus
`reacl.dom` mit Präfix `dom/` und `reacl.lens` mit Präfix `lens/`.

Der Namespace `reacl.core` definiert die zentralen, oben beschriebenen
Konzepte und `reacl.dom` Funktionen für die DOM-Manipulation.  Der
Namespace `reacl.lens` ist eine kleine Library für *Linsen*, welche es
erleichtern, den Applikationszustand zu manipulieren.  Eine Linse ist
eine Art Zeiger auf einen Teil einer größeren Struktur, der es
erlaubt, auf diesen Teil zuzugreifen und diesen auszutauschen.
(Linsen sind in Haskell [ein echter Hit
sind](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html).
Darüber werden wir auch noch ein Posting schreiben.)  Sie ersparen
uns, mit "Ids" o.ä. hantieren zu müssen, um bestimmte Teile des
Applikationszustands zu identifizieren.

## Einzelne Todos ##

Die Todo-Applikation verwaltet den Applikationszustand - also die
Liste von Todos - als Liste von Objekten des Typs `Todo`:

{% highlight clojure %}
(defrecord Todo [text done?])
{% endhighlight %}

Eine Reacl-Applikation definiert *Klassen* von *Komponenten*, die in
das DOM einer Web-Anwendung eingehängt werden können.  Entsprechend
gibt es eine Klasse `to-do-item` für einzelne Todos, und jedes Todo
ist eine Instanz der Klasse - eine Komponente.  Hier ist der Anfang
der Klassendefinition für `to-do-item`:

{% highlight clojure %}
(reacl/defclass to-do-item
  this todos [lens]
  ...)
{% endhighlight %}

In der Klassendefinition können drei lokale Variablen verwendet
werden: Die Bedeutung von `this` und `todos` ist in Reacl
vordefiniert, `lens` ist hingegen ein *Parameter* der Klasse:

* `this` ist die Komponente.
* `todos` ist der *Applikationszustand*, also die Liste aller Todos.
* `lens` ist ein Zeiger innerhalb der Todo-Liste auf *dieses* Todo.

Jede Reacl-Klasse muss eine *Render-Methode* definieren, die aus dem
Applikationszustand und den Parametern den View generiert:

{% highlight clojure %}
(reacl/defclass to-do-item
  this todos [lens]
  render
  (let [todo (lens/yank todos lens)]
    (dom/letdom
     [checkbox (dom/input
                {:type "checkbox"
                 :value (:done? todo)
                 :onChange #(reacl/send-message! this
                                                 (.-checked (dom/dom-node this checkbox)))})]
     (dom/div checkbox
              (:text todo))))
  ...)
{% endhighlight %}

Der Ausdruck nach `render` muss ein virtuelles DOM-Objekt liefern.
Dazu benutzt er `lens/yank`, um aus der Todo-Liste "dieses" Todo
herauszuholen.  Der `dom/div`-Ausdruck macht ein DOM-Objekt (ein
`div`-Element) aus einer Checkbox (zum Abhaken des Todos) und dem Text
des Todos.  Die Checkbox wird mit dem `dom/input`-Ausdruck erzeugt,
der ein entsprechendes `input`-Element liefert.
Die Map mit den Schlüsseln `:type`, `:value` und
`:onChange` steht für die HTML-Attribute `type`, `value` und
`onChange`.  (Die Doppelpunkte kennzeichnen sogenannte *Keywords* in
ClojureScript, als effiziente Schlüssel in die Map fungieren.)

Wichtig am `input`-Element ist die Callback-Funktion am
`onChange`-Attribut: Diese schickt eine Nachricht an die Komponente
(also an `this`): `true`, wenn der Haken gesetzt ist, sonst `false`.
Dieser boolesche Wert muss aus dem "echten" DOM extrahiert werden, den
`dom/dom-node` liefert - dort ist er der Wert des `checked`-Felds.
(Der Punkt in `.-checked` steht für den Zugriff auf ein Feld, das `-`
besagt, dass ein Feld ausgelesen wird und nicht eine Methode aufgerufen.)
Damit `dom/dom-node` weiß, für *welchen* virtuellen DOM-Knoten sie den
echten DOM-Knoten liefern soll (nämlich das `input`-Element), muss das
`input`-DOM einen Namen bekommen - das passiert mit `dom/letdom`, das
ähnlich wie `let` funktioniert, aber speziell für diesen Zweck gemacht
ist.

Damit kann die Komponente schon einmal ein Todo darstellen.  Es fehlt
noch der interaktive Aspekt.  Dazu kommt zur `defclass`-Form noch eine
`handle-message`-Klausel dazu, die den Wert als Argument bekommt, der
mit `reacl/send-message!` verschickt wurde:

{% highlight clojure %}
(reacl/defclass to-do-item
  this todos [lens]
  render ...
  handle-message
  (fn [checked?]
    (reacl/return :app-state
                  (lens/shove todos
                              (lens/in lens :done?)
                              checked?))))
{% endhighlight %}

In diesem Fall führt das Abhaken der Checkbox dazu, dass der
Applikationszustand durch einen neuen ersetzt werden muss: Der Wert
des `done?`-Felds des aktullen Todos soll ersetzt werden.  Dies macht
der `lens/shove`-Ausdruck oben, der eine neue Liste von Todos
liefert.  (Mehr zu Linsen - wie gesagt - in einem späteren Posting.)
Der Aufruf von `(reacl/return :app-state ...)` signalisiert Reacl,
dass der Applikationszustand ausgetauscht werden soll.

Fertig ist die `to-do-item`-Klasse!  Die Klasse `to-do-item` kann
jetzt als Funktion benutzt werden, die zwei Parameter hat: Die
Komponente, in die das Todo eingebaut wird sowie die Linse `lens`.

## Die Todo-Liste ##

Die gesamte Todo-Applikation besteht aus einer Liste von `to-do-item`s
sowie einem Textfeld für neue Todos.  Die Todo-Applikations-Komponente
kennt also zwei Arten der Benutzer-Interaktion: Text wird eingetippt
und schließlich der `Add`-Knopf (oder Return) gedrückt.  Um beide
Aktionen zu repräsentieren, benutzen wir zwei Record-Definitionen:

{% highlight clojure %}
(defrecord New-text [text])
(defrecord Submit [])
{% endhighlight %}

Der bisher eingetippte Text ist "irgendwie Zustand", aber kein
Applikationszustand: Er geht erst in den Applikationszustand ein, wenn
`Add`/Return gedruckt wird.  Davor ist er reiner lokaler
"GUI-Zustand", der lokal zur Komponente gehört.  
Diese Sorte Zustand
unterscheidet Reacl vom Applikationszustand, und er kann bei der
Klassendeklaration als zusätzlicher Parameter angemeldet
werden:

{% highlight clojure %}
(reacl/defclass to-do-app
  this todos local-state []
  initial-state ""
  ...)
{% endhighlight %}

Es reicht übrigens nicht, den Text erst bei Bedarf aus dem DOM-Knoten
auszulesen, da der Message-Handler keinen Zugriff auf das DOM hat.
Dies entkoppelt außerdem den GUI-View von der Reaktion auf
Benutzereingaben, was die Softwarearchitektur verbessert.

Die `initial-state`-Klausel legt den Anfangszustand bei der Erzeugung
der Komponente fest - noch kein Text da.  Der `render`-Ausdruck kann
`local-state` als Wert des Textfelds in das DOM einbauen:

{% highlight clojure %}
(reacl/defclass to-do-app
  this todos local-state []

  initial-state ""

  render
  (dom/div
   (dom/h3 "TODO")
   (dom/div (map-indexed (fn [i todo]
                           (dom/keyed (str i) (to-do-item this (lens/at-index i))))
                         todos))
   (dom/form
    {:onSubmit (fn [e _]
                 (.preventDefault e)
                 (reacl/send-message! this (Submit.)))}
    (dom/input {:onChange (fn [e]
                            (reacl/send-message! this
                                                 (New-text. (.. e -target -value))))
                :value local-state})
    (dom/button
     (str "Add #" (+ (count todos) 1)))))
  ...)
{% endhighlight %}

Zu beachten ist hier, dass es zwei Callbacks gibt, die jeweils eine
unterschiedliche Nachricht mit `reacl/send-message!` verschicken.  (Die
`dom/keyed` Funktion nummeriert die Todos durch, damit React sie
intern bei Änderungen zuordnen kann.)

Schließlich fehlt noch die `handle-message`-Klausel:

{% highlight clojure %}
(reacl/defclass to-do-app
  this todos local-state []
  ...
  handle-message
  (fn [msg]
    (cond
     (instance? New-text msg)
     (reacl/return :local-state (:text msg))

     (instance? Submit msg)
     (reacl/return :local-state ""
                   :app-state (concat todos [(Todo. local-state false)])))))
{% endhighlight %}

Diese unterscheidet zwischen den zwei Nachrichten-Typen und
transformiert den Zustand entsprechend.  Die `:local-state`-Klausel
von `reacl/return` sorgt dafür, dass der lokale Zustand ersetzt wird.

# Fazit #

Das Reacl-Modell unterscheidet sich deutlich von anderen
MVC-Frameworks für Javascript: Programmierer müssen sich nicht darum
kümmern, die DOM bei Modell-Änderungen gerade passend zu ändern.
Stattdessen generiert das Programm den View einfach neu und dieser ist
damit immer konsistent.  Das vereinfacht die Programmierung radikal.
React kümmert sich darum, das DOM effizient dem Browser zu vermitteln.
Dabei ist React überraschend performant - React-Programme sind oft
schneller als traditionelle MVC-Programme.

Reacl-Klassen zentralisieren die Transformation von Zustand in den
`handle-message`-Klauseln der Klassen.  Dies macht es einfach,
die möglichen Zustandsänderungen im Überblick zu behalten.

Das gesamte Beispiel ist im Verzeichnis `examples/todo` im
[Reacl-Projekt](https://github.com/active-group/reacl) zu finden.
Dort finden sich noch weitere Beispiele - hoffentlich viel Spaß dabei!

<!-- more end -->

