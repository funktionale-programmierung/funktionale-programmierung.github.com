---
layout: post
description: ClojureScript in Desktopanwendungen verwenden.
title: "ClojureScript in Desktopanwendungen"
author: helmut-dobretzberger
tags: ["Scala", "Reacl", "ClojureScript", "JavaFX", "Equals"] 
page_title: "ClojureScript in Desktopanwendungen"
---

Wie bereits im Artikel
[Web-Apps mit Reacl programmieren](/2014/07/07/reacl.html)
beschrieben, verwenden wir bei der
[Active-Group](http://www.active-group.de) für die Entwicklung von
Web-Frontends [ClojureScript](http://clojure.org/clojurescript) mit
unserem Framework [Reacl](https://github.com/active-group/reacl).
Im Projekt [EQUALS](/2014/04/03/equals-java-scala.html)
entwickeln wir derzeit für EQUALS-Desktopanwendung Teile der grafischen
Benutzeroberfläche neu. Dieser Artikel zeigt, wie man in einer Scala-
und Java-Desktopanwendung ClojureScript-Programme verwenden kann.
<!-- more start -->

Seit Version 7u6 integriert die Java-Runtime (JRE) das Framework
[ JavaFX](http://docs.oracle.com/javase/8/javase-clienttechnologies.htm),
welches unter anderem einen eingebetteten Browser (als `WebView`
bezeichnet) enthält. Dieser kann in eine Java/Scala-Benutzeroberfläche
integriert werden, und erlaubt somit die Einbindung beliebiger
Webseiten in eine Desktopanwendung. Es wird dabei, wie auch in Apples
Safari, die Rendering-Engine [WebKit](https://www.webkit.org/)
verwendet. Seit der Version 8 der Java-Runtime ist die Darstellung von
Webseiten , die sich ja häufig von Browser zu Browser unterscheidet,
in JavaFX sehr ähnlich zu Apples Browser Safari und auch Google Chrome.

## Motivation

Für uns bietet die Möglichkeit, die Benutzeroberfläche mit Webtechnologien zwei entscheidende Vorteile:

- Es sind nahezu beliebige JavaScript oder CSS-Bibliotheken für die Oberflächengestaltung verwendbar. Wir verwenden unser Framework Reacl in Kombination mit Twitters [Bootstrap-Framework](http://getbootstrap.com/)

- Das grafische Design ist auch unabhängig von der Desktopanwendung
  einsetzbar. Für uns ist das vor allem im Hinblick auf eine
  Webversion, die ähnliche Funktionalität wie die Desktopanwendung
  bieten soll, relevant. Hier ist dann keine Neugestaltung mehr
  notwendig sondern die grafische Benutzeroberfläche, die ja bereits
  eine Webanwendung ist, kann wiederverwendet werden.

JavaFX bietet noch [mehr](http://docs.oracle.com/javase/8/javafx/get-started-tutorial/jfx-overview.htm#A1131418) als einen eingebetteten Browser, wir betrachten hier aber nur die Einbindung von Webanwendungen.
Wer nicht unbedingt JavaFX verwenden muss/möchte, kann auch gern auf Alternativen zurückgreifen, so bietet z.B. das [.NET-Framework](http://msdn.microsoft.com/de-de/library/w0x726c2)  mit der [WebBrowser-Klasse](http://msdn.microsoft.com/de-de/library/system.windows.controls.webbrowser) ähnliche Funktionalität.

## Integration einer Webanwendung in eine Scala-Desktopanwendung

Es ist sehr einfach möglich, in einer mit Swing gestalteten
Benutzeroberfläche JavaFX zur Gestaltung der Benutzeroberfläche zu verwenden: Das `JFXPanel` ist eine `JComponent` und kann daher überall eingesetzt werden, wo man auch andere Swing-Komponenten wie `JTextField` oÄ. verwenden kann.

Ein typisches Aussehen in einer Scala-Anwendung, die eine Webanwendung integriert, sieht wie folgt aus:

{% highlight scala %}
object Main {
  def main(args: Array[String]) {
    SwingUtilities.invokeLater(new Runnable() {
        override def run() = initPanel()
    })
  }

 def initPanel() = {
  val frame = new scala.swing.Frame()
  val panel = new JFXPanel()
  frame.peer.add(panel)
  frame.visible = true
  frame.peer.setSize(800,600)

  initView(panel)
 }

 def initView(p: JFXPanel) = {
   javafx.application.Platform.runLater(new Runnable {
    override def run() {
        val webView = new javafx.scene.web.WebView()
        val webEngine = webView.getEngine
        webEngine.load("http://www.funktionale-programmierung.de")
    }
  })

  p.setScene(new Scene(webView))
 }
}
{% endhighlight %}

Der obige Code erzeugt eine Main-Klasse, die einen Frame erstellt. In
diesem Frame wird ein `JFXPanel` platziert. In diesem Panel hat man
die Möglichkeit, beliebige JFX-Funktionalitäten einzubinden. Wir
wollen eine Webanwendung anzeigen, was beispielhaft in `initView` geschieht: Es wird ein neuer `WebView` erzeugt, und dann die Seite `www.funktionale-programmierung.de` geladen. Damit die Seite im `JFXPanel` angezeigt wird, muss diese noch mit einer `Scene` dem Panel hinzugefügt werden. (Eine Scene ist vom Grundsatz her nichts anderes als ein JPanel. Es heißt in JavaFX nur anders und kann mehr.)

Somit hat man eine einfaches Programm, welches Webanwendung wie ein Browser lädt und anzeigt.
Jetzt fehlt noch die Möglichkeit, mit dieser Webanwendung zu interagieren:

-  Woher weiß Scala, wann in der Webanwendung ein Knopf gedrückt wurde, oder welche Eingaben ein Benutzer in ein Textfeld gemacht hat?
-  Woher weiß die Webanwendung, wenn der Benutzer im restlichen Teil
   der Scala-Anwendung (also in der "normalen" Benutzeroberfläche)
   eine Aktion getätigt hat, die sich auf die Webanwendung auswirken
   soll?

Zur Lösung und Erklärung dieser Fragen verwenden wir im Folgenden eine
mit reacl entwickelte Mini-Webanwendung, die nur einen Button enthält. 

## Kommunikation von der Desktopanwendung zur Webanwendung

Die folgende Reacl-Anwendung wird im Namespace `javafx.simple`
erstellt und enthält nur eine Reacl-Klasse, die einen Button erzeugt,
sowie eine Funktion `simple-app`, die diese Reacl-Klasse aufruft.
Erstmal macht dieser Button noch nichts, wir füllen Ihn später mit Funktionalität.


{% highlight clojure %}
(ns javafx.simple
  (:require [reacl.core :as reacl :include-macros true]
            [reacl.dom :as dom :include-macros true]))

(reacl/defclass javafx-simple-app
  this []
  render
  (dom/div
    (dom/button "Beenden - Aber ich funktioniere noch nicht!")))

(defn simple-app []
  (reacl/render-component
    (.getElementById js/document "content")
    javafx-simple-app))
	
{%endhighlight%}

Außerdem gehört zu jeder Reacl-Anwendung noch eine kleine HTML-Datei,
hier `javafxtest.html`, die die benötigtigen JavaScript-Bibliotheken einbindet (mehr Details
gibts dazu im früheren Artikel [Erste Schritte mit ClojureScript](/2014/02/14/clojurescript-react.html)).

{%highlight html %}
<html>
<body>
<script src="out/goog/base.js" type="text/javascript"></script>
<script src="main.js" type="text/javascript"></script>
<script type="text/javascript">goog.require("javafx.simple");</script>
</body>
<div id="content"></div>
</html>
{%endhighlight%}


Diese Reacl-Anwendung soll dann innerhalb des Scala-Programmes
angezeigt werden. Dabei sind zwei Dinge zu beachten: Zum einen ist die Webanwendung
 nicht mehr im Internet verfügbar, sondern eine lokale Anwendung,
die Teil des Scala-Programms sein soll, und zum anderen reicht es
nicht, die zum ClojureScript-Programm zugehörige HTML-Seite zu
laden - die Anwendung mit dem Namen `simple-app`muss auch noch
gestartet werden. Der Code der dies erledigt sieht wie folgt aus:

{% highlight scala %}
def initView(p: JFXPanel) = {
   javafx.application.Platform.runLater(new Runnable {
     override def run() {
       val webView = new javafx.scene.web.WebView()
       val webEngine = webView.getEngine
       val mainURL = getClass.getResource("resources/javafxtest.html")
       webEngine.load(mainURL.toExternalForm)
	   
       webEngine.getLoadWorker.stateProperty.addListener(new ChangeListener[State] {
         override def changed(p1: ObservableValue[_ <: State], p2: State, p3: State) {
          val namespace = "javafx.simple"
           webEngine.executeScript("goog.require(\"" + namespace + "\");")
           val jsjfx = webEngine.executeScript(namespace).asInstanceOf[JSObject]
            jsjfx.call("simple_app")
          }
       })
	   
      p.setScene(new Scene(webView))
      }
   })
}
{%endhighlight %}

Die Klasse `WebView` stellt dem Entwickler eine `WebEngine` zur Verfügung. Dabei handelt es sich um ein Objekt, mit dessen Hilfe man
mit der eingebundenen Webanwendung interagieren kann: Die Methode `executeScript` als Teil der `WebEngine` kann JavaScript-Code der
Webanwendung benutzen. Das machen wir uns zunütze, um unsere
ClojureScript-Anwendung zu starten.
Nachdem die HTML-Seite im Pfad
`resources/javafxtest.html` fertig geladen wurde (deshalb das Konstrukt
mit `getLoadWorker.stateProperty.addListener`) ruft Scala mit
`webEngine.executeScript` JavaScript-Code der Webanwendung auf:

- `webEngine.executeScript("goog.require(\"" + namespace + "\");")`
 lädt das reacl-Programm im angegebenen Namespace und sorgt dafür,
 dass mögliche Abhängigkeiten aufgelöst werden
 - `val jsjfx =
   webEngine.executeScript(namespace).asInstanceOf[JSObject]` holt das
   reacl-Programm im angegebenen Namespace und speicherte es in  `jsjfx`
- `jsjfx.call("simple_app")` ruft die in ClojureScript definierte
   Funktion `simple-app` auf, die dann die Anwendung startet. Aber Moment mal: Wieso ruft der Code `simple_app`auf, obwohl die
Anwendung in ClojureScript `simple-app`heißt? Der Grund liegt darin,
dass in ClojureScript Namen erlaubt sind, die in JavaScript nicht
möglich sind. Dadurch finden beim Compilieren Umbenennungen statt:
`-`zu `_`oder ein `!`zu `_BANG_`
Es gibt hierfür noch viele weitere Regeln, die
z.B. [hier](https://groups.google.com/d/msg/clojure/jeg3LdDQnaU/UrOv75FfYh0J)
gut zusammengefasst sind.

Und das war schon, zumindest für die Richtung von Scala nach Webanwendung. Die Kommunikation von Scala zur eingebundenen Webanwendung ist mit dem
Aufruf von `executeScript` also sehr einfach zu bewerkstelligen. Der
Rückgabetyp der aufgerufenen JavaScript-Methoden wird übrigens von der
WebEngine automatisch in bestimmte Java-Datentypen zurückkonvertiert. Hierfür gibt es bestimmte Regeln,
die man am Besten einfach [nachliest](http://docs.oracle.com/javafx/2/api/javafx/scene/web/WebEngine.html#executeScript).


## Kommunikation vom Webanwendung zu Desktopanwendung

Es ist tatsächlich sehr einfach, Code der Desktopanwendung aus der
Webanwendung aufzurufen (also sogenannte "Upcalls" durchzuführen). Wir benötigen nur das Wissen, dass man solche
Scala-Klassen oder Funktionen an eine ClojureScript-Anwendung übergeben, und von dort
aufrufen kann.
Jetzt ist es sehr einfach, wir müssen dazu
lediglich nur eine Zeile unserer bereits bekannten Scala-Codes
modifzieren:

{% highlight scala %}
jsjfx.call("simple_app", Array({() => System.exit(0)}))
{% endhighlight %}

`call` erlaubt die Angabe eines Arrays mit Argumenten, die der aufgerufenen JavaScript-Funktion übergeben werden.
Und genau das geschieht jetzt: Es wird die JavaScript-Funktion
`simple_app`mit einem Array, welches unsere parameterlose Funktion
`{() => System.exit(0)}` zum Beenden der Scala-Anwendung enthält.

Der Button der ClojureScript/React-Anwendung war vorhin noch ohne Funktion - jetzt können wir das ändern:

{% highlight clojure %}
(reacl/defclass javafx-simple-app
  this [exit]
  render
  (dom/div
    (dom/button  {:onClick #(.apply exit)} "Beenden")))


(defn simple-app [scala-args]
  (reacl/render-component
    (.getElementById js/document "content")
    javafx-simple-app nil (first scala-args)))
{% endhighlight %}

`simple-app` nimmt jetzt noch einen Parameter entgegen, der die
Argumente des Scala-Aufrufes enthält. Das erste Argument enthält
unsere `exit`-Funktion, welche dann auch an die
React-Klasse `javafx-simple-app` weitergereicht wird. Das bedeutet,
wir können den Scala-Code jetzt in ClojureScript aufrufen! Somit kann auch der Button mit einer
Beenden-Funktionalität versehen werden: Man muss einfach nur beim
Klicken `exit` aufrufen. 

An dieser Stelle muss allerdings noch auf ein Problem hingewiesen
werden: Der Aufruf von `exit` ist synchron. Bei einer Webanwendung mit
Client und Server wäre so ein Aufruf allerdings asynchron. Es ist nun sinnvoll
ClojureScript-Anwendung erstellen, die hier den Scala-Code ansychron
aufrufen, um dann den Wechsel von einer
ClojureScript-Webanwendung innerhalb von JavaFX zu einer "echten" Webanwendung deutlich zu erleichtern.
Dafür ist allerdings etwas mehr Arbeit notwendig, die Thema eines
künftigen Artikels sein wird. 

## Parameterübergabe und Tykonvertierung
Etwas knifflig ist noch die Frage, wie man am Besten mit Methoden
umgeht, die Parameter enthalten.  Grundsätzlich ist es kein Problem,
denn man kann auch eine Funktion mit Parametern bei einem
`call`-Aufruf übergeben und diese dann in ClojureScript mit Argumenten aufrufen.

{% highlight scala %}
jsjfx.call("simple_app", Array({arg:Object => doWithArg(arg)}))
{%endhighlight%}

Das funktioniert auch problemlos für Zahlen, Strings oder
Booleans. Bei uns war es jedoch der Fall, dass wir häufig komplexere
JavaScript-Objekte als Argumente verwenden wollen. In diesem Fall
bietet sich an, mit [JSON](http://json.org) zu arbeiten: Unsere
Datenstrukturen in JavaScript werden nach JSON serialisiert und der
Ergebnisstring dann als Parameter übergeben.

Auf Scala-Seite wird dann der String mithilfe von JSON deserialisiert und passend verarbeitet.
Somit hat man nicht nur eine klare Schnittstelle - man übergibt immer JSON-Strings - sondern hat auch noch Kontrolle darüber, wie Methodenparameter vearbeitet werden, da die bereits erwähnten automatischen Typkonvertierungsregeln von JavaFX nicht mehr zur Anwendung kommen. Es gibt einige JSON-Bibliotheken für Scala, wir haben beispielsweise gute Erfahrungen mit der [Play JSON Bibliothek](https://www.playframework.com/documentation/2.0/ScalaJson) gemacht.

## Zusammenfassung
JavaFX bietet eine komfortable Möglichkeit, Webanwendungen in eine
Desktopanwendung zu integrieren. Die Vorteile davon sind vor allem in
den größeren Freiheiten bei der grafischen Gestaltung der Anwendung,
sowie in der Wiederverwendbarkeit einer so entwickelten Anwendung für
"echte" Webanwendungen. Gerade in Kombination mit ClojureScript/Reacl
können wir hier funktionale Programmierung sowohl für die
Benutzeroberfläche, als auch den zugrundeliegenden Scala-Code einsetzen.
