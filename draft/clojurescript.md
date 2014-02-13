---
layout: post
description: Erste Schritte in ClojureScript
title: "Erste Schritte in ClojureScript"
author: michael-sperber
tags: ["ClojureScript", "Clojure", "JavaScript"]
---

Funktionale Programmierer möchten auch in der Web-Entwicklung auf dem
Browser ihre bevorzugte funktionale Sprache nutzen.  Zwar hat
JavaScript
<a href="{% post_url 2013-07-25-curry-buch %}">Wurzeln in der funktionalen Programmiererung</a>,
aber die Sprache hat eben auch noch
eine objektorientierte und andere hässliche Seiten.  Aus diesem Grund
gibt es inzwischen eine kleine Industrie von Compilern von anderen
Programmiersprachen nach JavaScript, zum Beispiel für
[OCaml](http://ocsigen.org/js_of_ocaml/manual/),
[Scala](http://www.scala-js.org/),
[Haskell](http://www.haskell.org/haskellwiki/The_JavaScript_Problem)
und [Racket](http://hashcollision.org/whalesong/).

Heute beschäftigen wir uns mit einer besonders populären funktionalen
Sprache mit JavaScript als Ziel: [ClojureScript](https://github.com/clojure/clojurescript).

<!-- more start -->

ClojureScript ist ein Dialekt von [Clojure](http://clojure.org/), und
damit ein dynamisch getypter Lisp-Dialekt mit besonderer Betonung von
rein funktionalen Datenstrukturen.  ClojureScript ist nicht identisch
mit Clojure - es gibt eine Reihe [subtiler
Unterschiede](https://github.com/clojure/clojurescript/wiki/Differences-from-Clojure),
z.B. bei der Repräsentation von Zahlen, beim Feld-/Methodenzugriff
oder bei Makros.  In diesem Beitrag interessiert uns vor allem aber
ein schneller Einstieg, um ein Gefühl für die Sprache, ihr Ökosystem
und die Anbindung von JavaScript-Frameworks betrifft.  Programmieren
mit ClojureScript macht nämlich richtig Laune, vor allem für Leute die
wie mich, die kaum JavaScript können..

# Projektstart

Wie bei Clojure sonst auch ist bei der ClojureScript-Entwicklung
[Leiningen](https://github.com/technomancy/leiningen) unerlässlich,
das Standard-Build-Tool für Clojure-Projekte.  Leiningen erlaubt das
Anlegen eines Projektskeletts mit Hilfe im Internet hinterlegter
Schablonen.  Wir brauchen ein minimales ClojureScript-Projekt, das ist
mit einem Befehl gemacht:

    lein new mies cljs-hello
	
`mies` ist der Name der Schablone - der Befehl macht ein Verzeichnis
namens `cljs-hello`, in dem sich das Projekt befindet.  Dies kann mit
einem weiteren Leiningen-Befehl in Betrieb genommen werden:

    lein cljsbuild auto
	
Dieser Befehl sorgt dafür, dass der ClojureScript-Code im Projekt nach
JavaScript compiliert wird und hält danach Ausschau nach Änderungen.
Wenn er welche feststellt, wird recompiliert.

Wenn `lein cljsbuild` einmal durchgelaufen ist, ist die Datei
`index.html` im Projekt-Wurzelverzeichnis fertig zum Aufmachen mit dem
Browser.  (Konfiguriert ist das ganze in der Leiningen-Konfiguration
`project.clj`.  Für die Zwecke dieses Postings reicht allerdings die
generierte Konfiguration unverändert.

# Programmieren mit ClojureScript

In der Datei `src/cljs_hello/core.cljs` ist die eigentliche
Webanwendung.  Vorgefertigt iist da nur das folgende kümmerliche
Programm:

{% highlight clojure %}
(ns cljs-hello.core)

(enable-console-print!)

(println "Hello world!")
{% endhighlight %}

Die erste Zeile ist die aus Clojure bekannte Namespace-Deklaration.
`(enable-console-print!)` sorgt dafür, dass `println` und Konsorten in
die Konsole des Browsers drucken.  Es empfiehlt sich also, die
JavaScript-Konsole im Browser aufzumachen.  (Im Firefox über `Tools ->
Web Developer -> Web Console` zum Beispiel, im Chrome über `View ->
Developer -> Developer Tools`.)

Der Code wird von `index.html` aus geladen:

{% highlight html %}
<html>
    <body>
        <script src="out/goog/base.js" type="text/javascript"></script>
        <script src="cljs_hello.js" type="text/javascript"></script>
        <script type="text/javascript">goog.require("cljs_hello.core");</script>
    </body>
</html>
{% endhighlight %}

Von `cljs_hello.js` schließlich wird der ganze generierte
JavaScript-Code geladen, zusammen mit den benötigten Libraries.

# Programmieren in ClojureScript

Da die direkte Manipulation des DOM von JavaScript aus wenig Spaß
macht, empfiehlt sich auch in ClojureScript die Verwendung eines
GUI-Frameworks.  Besonders gut passt zum Beispiel kürzlich von
Facebook und LinkedIn veröffentliche
[React](http://facebook.github.io/react/), das insbesondere den
Konzepten aus der funktionalen Programmierung sehr nah ist:

React-*Komponenten* sind Objekte mit beliebigem inneren Zustand, den
sie jederzeit in eine GUI rendern können, indem sie eine
`render`-Methode spezifizieren.  Diese liefert ein *virtuelles*
DOM-Objekt, das React dann in die reale DOM überträgt.  Dabei
überträgt React nur die Teile, die sich geändert haben, was das
Framework [extrem schnell
macht](http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/).

Als Beispiel machen wir eine ganz einfache Kommentar-Applikation, in
der eine Liste von Kommentaren - jeweils Autor und Text - angezeigt
werden und neue Kommentare hinzugefügt werden können.  Als
Vorbereitung müssen wir erstmal in `index.html` eine Zeile hinzufügen,
die React einbindet:

{% highlight html %}
        <script src="http://fb.me/react-0.8.0.js"></script>
{% endhighlight %}

Hier ist die Definition für eine React-Komponenten-Klasse für einen
einzelnen Kommentar in ClojureScript:

{% highlight clojure %}
(def Comment
  (js/React.createClass
   #js {:render (fn []
                  (this-as this
                           (let [props (.-props this)]
                             (js/React.DOM.div
                              nil
                              (js/React.DOM.h2 nil  (.-author props))
                              (js/React.DOM.span nil (.-text props))))))}))
{% endhighlight %}

An Objekte aus dem JavaScript-Namensraum kommen Programme mit dem
Präfix `js/` heran.  Entsprechend gehört die Funktion `createClass` zu
React - sie erwartet eine JavaScript-Hashmap, die verschiedene Aspekte
einer Komponente definiert.  Im einfachsten Fall wie hier ist das nur
eine einzelne Funktion `render`.  Allerdings ist zu beachten, dass
ClojureScript-Maps nicht direkt als JavaScript-Hashmaps verwendet
werden können.  (In ClojureScript ist eine Map wie in Clojure auch
eine persistente Datenstruktur.)  Der Präfix `#js` sorgt aber dafür, dass
die darauffolgende Map als JavaScript-Hashmap angelegt wird.  Dabei
wird dabei aus dem Keyword-Schlüssel `:render` der Schlüssel `render`
in der Hashmap.

React schreibt vor, dass es mindestens den `render`-Eintrag geben
soll, und der sollte eine Funktion sein, die ein virtuelles DOM-Objekt
zurückliefert.  Dazu muss das Kommentar-Objekt wissen, wer Autor und
was der Text der Kommentars ist.  In React sind dafür die *Properties*
einer Komponente zuständig, die bei der Erzeugung der Komponente
mitgeliefert werden können, ebenfalls in Form einer Hashmap, zum
Beispiel in folgender Form:

{% highlight clojure %}
(Comment #js {:author "Mike Sperber" :text "Alles Mist."})
{% endhighlight %}

Innerhalb der `render`-Funktion stecken die Properties im Feld `props`
von "This".  "This" muss  in ClojureScript explizit an einen
Bezeichner gebunden werden: Die Form `(this-as this ...)` bindet ihn
an `this`.  Dann extrahiert `(.-props this)` den Inhalt des
`props`-Feldes.  (Das `-` unterscheidet Feldzugriffe von
Methodenaufrufen, die kein `-` aufweisen.)

Die `js/React.DOM`-Aufrufe konstruieren das virtuelle DOM-Objekt.
Die `nil`s sagen jeweils, dass bei den Elementen keine Attribute
stehen.  In HTML ausgedrückt sähe der Rumpf einer
`Comment`-Komponente so aus:

{% highlight html %}
<div><h2>AUTHOR</h2><span>TEXT</span></div>
{% endhighlight %}

Ganz ähnlich funktioniert die `CommentList`-Klasse für eine ganze
Liste von Kommentaren:

{% highlight clojure %}
(def CommentList 
  (js/React.createClass
   #js {:render (fn []
                  (this-as this
                           (js/React.DOM.div
                            nil
                            (into-array
                             (map (fn [c] (Comment #js {:author (:author c) :text (:text c)}))
                                  (.-comments (.-props this)))))))}))
{% endhighlight %}

Gedacht ist, dass so eine `CommentList`-Komponente zum Beispiel so
instanziert wird:

{% highlight clojure %}
(CommentList #js {:comments [{:author "Mike Sperber" :text "Mikes Kommentar."}
                             {:author "David Frese" :text "Davids Kommentar."}]})
{% endhighlight %}

Auch hier ist zu beachten, dass ein ClojureScript-Vektor kein
JavaScript-Array ist - die React-DOM-Konstruktoren wollen aber Arrays
sehen.  Darum die Konversion mit `into-array`.

Als nächstes machen wir eine Komponente, die das Eingeben einer neuen
Kommentare erlaubt.  Dafür legen wir ein Formular mit zwei Textfeldern
an - für Autor und Text des Formulars:

{% highlight clojure %}
(def NewComment
  (js/React.createClass
   #js {:render
        (fn []
          (this-as this
                   (js/React.DOM.form
                    #js {:onSubmit (.-handleSubmit this)}
                    (js/React.DOM.input
                     #js {:type "text" :ref "author"})
                    (js/React.DOM.input
                     #js {:type "text" :ref "text"})
                    (js/React.DOM.button nil "Submit"))))
        :handleSubmit
        (fn [e]
          (this-as this
                   (let [props (.-props this)
                         refs (.-refs this)
                         author-dom (.getDOMNode (.-author refs))
                         text-dom (.getDOMNode (.-text refs))]
                     (.newComment props {:author (.-value author-dom) :text (.-value text-dom)})))
          false)}))
{% endhighlight %}

Die beiden `input`-Elemente in der `render`-Methode weisen jeweils
`ref`-Attribute mit Namen für die Elemente auf: diese sind dazu da,
damit der Callback des Formulars auf die Inhalte der Textfelder
zugreifen kann.  Der Callback ist in der `handleSubmit`-Funktion, die
bei der Konstruktion der `NewComment`-Komponente ebenfalls in der
Hashmap ist, in der auch `render` steht.  React steckt sie unter dem
gleichen Namen in das Komponenten-Objekt, so dass `render` mit
`(.-handleSubmit this)` darauf zugreifen kann.

Die `handleSubmit`-Funktion kann dann auf das `refs`-Feld von
`this`-Zugreifen, in dem alle DOM-Elemente stehen, die ein
`ref`-Attribut in `render` abbekommen haben.  
Das Feld `author` ist dasElement mit `ref`-Wert `author`, entsprechend
für `text`.  Die Methode `getDOMNode` liefert dann das entsprechende
echte DOM-Element, in dem das `value`-Feld dann den entsprechenden
Text holt.

Aber was tun mit dem neuen Kommentar?  Er sollte natürlich zu einer
Liste aller Kommentare hinzugefügt werden, die sich aber außerhalb der
`Newcomment`-Komponente befindet.  Die `handleSubmit`-Funktion geht
deshalb davon aus, dass sich unter den Properties eine Funktion namens
`newComment` befindet, die diese Aufgabe erledigt.  Die muss dann bei
der Erzeugung der `NewComment`-Komponente mitgeliefert werden.  Diese
ruft `handleSubmit` mit dem neuen Kommentar auf. 

Wichtig: `handleSubmit` muss `false` liefern, damit der Browser das
Submit-Event als fertig behandelt ansieht und nicht das Formular
versucht an die Webseite auszuliefern.

Schließlich schreiben wir noch eine Komponentenklasse, um alles
zusamenzusetzen.  Diese Klasse namens `CommentBox` hat nun (als
einzige) *Zustand*, nämlich die aktuelle Liste aller Kommentare.  Für
die Verwaltung des Zustands will React eine Methode namens
`getInitialState` sehen, die den Anfangszustand setzt.  Wie bei den
Properties auch muss der Zustand eine Hashmap sein.   Die Funktion
holt sich den Anfangszustand aus den Properties (die wir noch
entsprechend übergeben müssen):

{% highlight clojure %}
((def CommentBox
  (js/React.createClass
   #js {:getInitialState 
        (fn []
          (this-as this
                   #js {:comments (.-comments (.-props this))}))
{% endhighlight %}


Die `render-Methode` baut jetzt eine `CommentList`- und eine
`NewComment`-Komponente ein und übergibt an `NewComment` in den
Properties die `newComment`-Methode, die auf `render` folgt:

{% highlight clojure %}
        :render (fn []
                  (this-as this
                           (js/React.DOM.div
                            nil
                            (js/React.DOM.h1 nil "Comments")
                            (CommentList #js {:comments (.-comments (.-state this))})
                            (js/React.DOM.h2 nil "New Comment")
                            (NewComment #js {:newComment (.-newComment this)}))))
        :newComment 
        (fn [c]
          (this-as this
                   (.setState this #js {:comments (conj (.-comments (.-props this)) c)})))}))
{% endhighlight %}

Die `newComment`-Methode schließlich benutzt die React-Methode
`setstate`, um das Kommentarfeld im Zustand um den neuen Kommentar zu
erweitern.

Fertig!  Na ja, fast: Um im Browser etwas zu sehen, müssen noch eine
`CommentBox`-Komponente im DOM aufhängen:

{% highlight clojure %}
(js/React.renderComponent
 (CommentBox #js {:comments [{:author "Mike Sperber" :text "Mikes Kommentar."}
                             {:author "David Frese" :text "Davids Kommentar."}]})
 (.getElementById js/document "content"))
{% endhighlight %}

Damit das funktioniert, müssen wir in `index.html` noch ein
Platzhalter-`div` mit `id`-Feld `content` unterbringen.  Das sieht
dann fertig so aus:

{% highlight html %}
<html>
    <body>
        <div id="content"></div>
        <script src="http://fb.me/react-0.8.0.js"></script>
        <script src="out/goog/base.js" type="text/javascript"></script>
        <script src="cljs_hello.js" type="text/javascript"></script>
        <script type="text/javascript">goog.require("cljs_hello.core");</script>
    </body>
</html>
{% endhighlight %}

Jetzt fertig!

# Clojure und React

Aus dem eleganten Modell von React lässt sich durch die Verwendung von
ClojureScript durch rein funktionale Programmierung noch mehr
herausholen.  Es sind schon mehrere ClojureScript-Frameworks um React
herum entstanden, zum Beispiel [Om](https://github.com/swannodette/om)
und[Reagent](http://holmsand.github.io/reagent/).  

Der komplette Code für dieses Beispiel befindet sich auf
[github](https://github.com/funktionale-programmierung/cljs-hello).

Viel Spaß beim Ausprobieren!

<!-- more end -->

