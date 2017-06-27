---
layout: post
description: Reacl 2 - Side Effects und Actions
title: "Reacl 2 - rein funktionale Programmierung, Side Effects und Actions"
author: marco-schneider
tags: ["reacl", "clojure", "rein funktional"]
page_title: "reacl2 und actions"
---

Vor einigen Tagen schaffte [Reacl](https://github.com/active-group/reacl), eine von uns im Haus entwickelte, rein 
funktionale Bibliothek um [React.js](https://facebook.github.io/react/), 
den Sprung auf Version *2.0.0*. In diesem Artikel betrachten wir einen neu
eingeführten Mechanismus etwas genauer: die Actions.

<!-- more start -->

Schon seit einiger Zeit ist Reacl bei uns in verschiedensten Projekten in
Verwendung um zuverlässige, schnelle und wartbare grafische Benutzeroberflächen 
(kurz *GUI*, vom englischen *Graphical User Interface*)
zu konstruieren.
Genug Zeit also, um die hierfür getroffenen Enscheidungen in der Realität zu 
erproben.

So ergeben sich in der neuesten Version einige Änderungen, welche allesamt aus 
den Erfahrungen im Produktiveinsatz resultieren. Das zugrunde liegende Konzept 
bleibt dabei allerdings gleich:
weiterhin dreht sich alles rund um das Konzept der Reacl-Klassen (eine 
ausführlichere Beschreibung finden Sie 
[in einem früheren Post hier in unserem Blog](http://funktionale-programmierung.de/2014/07/07/reacl.html)).

In diesem Artikel soll nun auf ein Detail der aktualisierten Verion im
Besonderen eingegangen werden.

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## Seiteneffekte und Actions ##

Ein neuer Mechanismus, welcher in Reacl 2 implementiert wurde ist der der 
*Action*s - eine Abstraktion mit dem Primärziel, Seiteneffekte zu kapseln 
und deren Logik von der Darstellung zu trennen.
Betrachtet man aktuelle Entwicklungen wie beispielsweise 
[Facebooks GraphQL -Biblothek](http://graphql.org/learn/), aber auch der sonst 
üblichen Verwendung (asynchroner) Kommmunikation zwischen Server und Client über 
sogenannte *Ajax*-Anfragen, so wird Eines schnell klar: der Trend geht eindeutig 
weiter in Richtung der Programmlogik auf der Seite des Klienten.
Jedoch ist unser Ziel weiterhin eine _rein funktionale Abbildung_ von Daten auf
eine sogenannte View.
Hier versteckt sich allerdings ein nicht-triviales Problem: wenn einerseits die 
Kommunikationslogik stärkeren Einzug in die Klientenlogik erhält, andererseits
die Grafischen Benutzerschnittstellen eine pure Funktion des 
Applikationszustandes sein soll, wie lassen sich diese zwei Fakten dann in einer 
Bibliothek miteinander vereinbaren?

Hier kommen die oben genanten Actions ins Spiel, welche in einem kurzen
Beispiel erläutert werden. Zu Beginn betrachten wir folgende Funktion, welche 
zur Darstellung benötigte Daten von eine Server erfragt.

{% highlight clojure %}
(defn fetch-data-from-server!
  "Fetch some data from the server."
  [req]
  ...)
{% endhighlight %}

Was auch ohne konkrete Implementierung schnell klar ist: das hat nichts in der
Logik der Darstellung verloren!
Greifen wir uns (unter einigen Problemen) nur die *Testbarkeit* einer Komponente 
heraus, welche auf diese Weise innerhalb der GUI-Logik einen solchen Effekt 
auslöst. Wie sollte ein Test für diese Komponente aussehen? Diese Frage lässt
sich nicht ganz einfach beantworten (möglicherweise benötigen wir hierzu ein
gemockte Serverimplementierung für die Client-seitigen Tests, eine selber nicht
triviale Angelegenheit). Im Übrigen wollen wir im besten Fall auch das
*Verhalten* einer Komponente testen und das am besten *unabhängig* von solcher
Logik.
Bislang ließ sich die Situation nur unelegant
auflösen; an einem Punkt im GUI-Code *musste* nun diese Anfrage abgesetzt werden,
ein Umstand, der die Idee der rein funktionalen Abbildung verlassen musste.

Intuitiv wäre folgender Ansatz wohl der Bessere: möchte eine Komponente gewisse
Daten erhalten, so stellt sie nicht selbst eine Anfrage an den Server. 
Stattdessen benachrichtigt Sie ein Modul, welches selbst nicht Teil der
GUI-Logik ist und damit unabhängig von der Darstellungslogik agieren kann. 
Betrachten wir nun folgenden Code:

{% highlight clojure %}
;; A record for storing requests and the recipient of the response.
(define-record-type message
  (make-message recipient request) message?
  [recipient message-recipient  ;; The component sending the request.
   request message-request])    ;; The request message to send to the server.

(defn handle-action
  [state req]
  (cond
    (message? req)
    (let [;; Fetch some data from the server
          response-data (...)]
      (send-answer-to (message-recipient msg) repsonse-data))))
{% endhighlight %}

 
Die Funktion `handle-action` bekommt hier eine beliebige Nachricht,
kodiert in den `message`-Record.
Wenn die Anfrage nun verarbeitet wurde, sendet diese Funktion
die Antwort an die anfragestellende Komponente, welche diese als reguläre 
Nachricht  entgegennehmen und verarbeiten kann. Und genau das ist der 
Mechanismus der `actions`, welchen wir im nächsten Abschnitt nochmal genauer
unter die Lupe nehmen.

## Ein konkretes Beispiel ##

Betrachten wir folgendes Szenario: eine GUI-Komponente soll den ein Element
einer Liste von Social-Media-Posts darstellen, welche dem Zustand einer 
überliegenden Komponente bereits bekannt sind. Wir beginnen damit, einen Record
Type für Posts zu schreiben, welche sich aus einer Id und einem Body
zusammensetzen.

{% highlight clojure %}
(ns funktionale-programmierung.core
  ;; Einige Imports für den späteren Gebrauch.
  (:require [active.clojure.record :as r :include-macros true]
            [reacl2.core :as reacl2 :include-macros true]
            [reacl2.dom :as dom]))

;; Record type for posts. See below for details on steps 1-3.
(r/define-record-type post
  ;; 1. Constructor
  (make-post id body)
  ;; 2. Predicate
  post?
  ;; 3. Selectors
  [id post-id
   body post-body])

(def post-1 (make-post 0 "Reacl 2 is out! Time to celebrate!"))
(def post-2 (make-post 1 "Go read the post on Funktionale Programmierung"))

;; Collection of initial posts.
(def initial-posts [post-1 post-2)

;; Example usage of those records.
(post? post-1)      ;; => true
(post? 42)          ;; => false
(post-id post-1)    ;; => 0
(post-body post-1)  ;; => "Reacl 2 is out! Time to celebrate!"
{% endhighlight %}

Wir definieren hier mit Hilfe der aus der `active-clojure` Bibliothek stammenden
Record-Types in drei Schritten zuerst einen neuen Typ:

1. Hier definieren wir einen Datenkonstruktor. Dieser erwartet zwei Argumente:
   eine Id und einen Body. Damit lassen sich neue Instanzen des Typs `post`
   erzeugen.
2. `post?` definiert eine Prädikatfunktion für diesen Typ.
3. In einem abschließenden Vektor definieren wir die Selektoren des Records,
   welche jeweils aus dem Namen des Feldes (wie im Konsturktor angegeben) und
   dem gewählten Namen der Selektorfunktion besteht.

Damit instaniziieren wir zwei Postings und binden diese im Anschluss in einem
Vektor an den Namen `initial-posts`. In einer Reacl-Komponente ließen sich diese
nun beispielsweise als ungeordnete Liste anzeigen. Wir wollen uns allerdings
hier mit der Detailansicht dieser Posts beschäftigen. Diese definiert sich wie
folgt (eine Erklärung findet sich wieder darunter):

{% highlight clojure %}

(reacl2/defclass post-detail this post [parent]  ;; 1. App state and arguments.
  local-state [comments []]  ;; 2. Local state.

  ;; 3. Tell reacl how to render this component.
  render
  (dom/div

    ;; We can access the app-state (aka. `post`) here.
    (dom/h1 "Post " (post-id post)) 
    (dom/p (post-body post))

    ;; 4. Button to get back to the post list.
    (dom/button {:onclick #(reacl2/send-message! parent :back)} "back")

    ;; 5. Render a list of comments.
    (dom/div
      (dom/h2 "Comments")
      (dom/ul
      (map-indexed (fn [idx comment]
                      (dom/keyed (str "comment-" idx)
                                (dom/li comment))) comments)))))
{% endhighlight %}

Sehen wir uns diese Komponente im Detail an:

1. In der ersten Zeile definieren wir die Reacl Klasse und geben ihr den Namen
   `post-detail`. Unter `this` kann diese Komponente sich selbst Referenzieren,
   Beispielsweise um sich selbst Nachrichten zu senden oder aber eine Referenz
   zu sich selbst an andere Komponenten weiterzugeben. Den App-State dieser
   Komponente nennen wir `post`, in welchem sich ein einzelner Post befindet und
   innerhalb des Rumpfes der Klasse unter diesem Namen erreichbar ist. Zuletzt
   findet sich dort ein Vektor, über den sich mögliche Argumente für diese 
   Klasse definieren lassen. Wir beschränken uns hier auf eine Referenz an die
   aufrufende Komponente um ihr im Zweifel mitteilen zu können, dass "zurück"
   Navigiert werden soll.
2. In der nächsten Zeile definieren wir einen sogenannten `local-state`. Dieser
   ist ebenfalls innerhalb der ganzen Komponente erreichbar, lässt sich
   allerdings auchh nur von dieser Lesen und nicht von außen abrufen. Beim
   ersten Instanziieren der Komponente wird dies als *initialer* lokaler Zustand
   verwendet; hier binden wir unter dem Namen `comments` einen leeren Vektor.
3. Anschließend an das Schlüsselwort `render` bestimmen wir, wie die Komponente
   nun als HTML dazustellen ist. Reacl bring dafür den `reacl2.dom` Namespace
   mit, mit Hilfe dessen sich HTML Knoten definieren lassen.
4. Ein Teil des UIs ist ein "Zurück"-Button. Wird dieser von der Userin/dem User
   bestätigt (`:onclick`) sendet die Komponente an den übergebenen Parent die
   Nachricht `:back`.
5. Zum Schluss stellen wir die Kommantare dieses Posts dar. Dies erfolgt über
   eine ungeordnete Liste mit einem Listeneintrag für jeden Kommentar.

Die Frage lautet nun: wie bekommen wir die Kommentare, welche auf einem Server
liegen und momentan noch nicht im Zustand der Applikation bekannt sind in die
Detailkomponente integriert?

## Actions ##
Genau hier kommen wir wieder auf die Actions zu sprechen. Anstatt wie sonst
einen Weg zu finden, möglichst reibungslos innerhalb der Reacl-Komponente
Seiteneffekte auszuführen greifen wir auf die oben definierte
`handle-action`-Funktion zurück. 
Da wir die Kommentare erst dann brauchen, wenn wir tatsächlich
einen einzelnen Post rendern, sollten wir diese auch erst beim Start der
Komponente anfragen. Das geht dank Reacl 2 nun sehr einfach; es muss lediglich 
ein Wert zur `post-detail`-Klasse hinzugefügt werden, der beim Start eine
entsprechende Aktion auslöst:

{% highlight clojure %}
(reacl2/defclass ...

  component-did-mount
  #(reacl2/return
    :action
    (make-messsage this [:fetch-comments-for-post (post-id post)])))
{% endhighlight %}

Wir geben als `return` Wert einfach eine Action an. Diese wird von Reacl an unseren
Action-Handler, der den Request verarbeitet (bitte im Kopf behalten, dass
das konkrete Absetzen des Requests hier nur als Platzhalter zu verstehen ist) 
weitergeleitet.
Schließlich, also nachdem der Request verarbeitet wurde und die Antwort 
verfügbar ist benachrichtigt `handle-message` unsere Komponente per regulärer
Reacl Nachricht.

{% highlight clojure %}
(defn handle-action
  "The first argument is the app-state of the sender (which we do not use in
   this example). The second one is the value returned via `return :action`."
  [_ req]
  (cond
    ...
    (message? req)
    (let [response-data (http-get (construct-request-from-data req))]
      (reacl2/send-message! (message-recipient msg) response-data))
    ...))

{% endhighlight %}

Im `handle-action` unserer Detailklasse können wir nun diese Antwort als
reguläre Nachricht empfangen und in den Zustand übernehmen. Das sieht so aus:

{% highlight clojure %}
(reacl2/defclass ...

  handle-message
  (fn [fetched-comments]
    (reacl2/return :local-state fetched-comments))))
{% endhighlight %}

Empfängt unsere Detail-Komponente nun in ihrem Messagehandler eine Antwort,
so übernimmt sie diese als ihren neuen, lokalen Zustand (welcher zu Anfang leer
war).

Schließlich müssen wir die Antwort in unserem Messagehandler abfragen
und das Resultat in unseren State übernehmen. Zuletzt müssen wir nur noch beim
Einhängen unserer App den `action-handler` registrieren und sind damit bereit,
die App laufen zu lassen:

{% highlight clojure %}
(reacl2/render-component
 (.getElementById js/document "app")
 post-component
 (reacl2/opt :reduce-action handle-action))
{% endhighlight %}

<!-- ## Und die Testbarkeit? ## -->
<!-- Eingehend führte ich eine Verbesserung der Testbarkeit als Argument für Reacl 2 -->
<!-- an. Bezüglich dieser ist hier einiges gewonnen: `:actions` sind wie -->
<!-- auch lokaler und "App" Zustand einfache Eigenschaften einer Komponentente. -->
<!-- Was das bedeutet ist recht einfach -- eine Komponente kann bezüglich der von ihr -->
<!-- ausgelösten Aktionen befragt werden, und zwar genau so, wie nach ihrem Zustand! -->

<!-- Das Testen von Reacl-Komponenten sollte zu einem späteren Zeitpunkt hier im Blog  -->
<!-- noch  einmal größere Beachtung finden. Zwischenzeitlich folgt hier noch ein -->
<!-- kleines Beispiel anhand unserer Detailkomponente: -->


## Fazit ##
Reacl in seiner aktuellsten Version macht (nicht nur!) bezüglich des Ziels, 
eine rein  funktionale Abbildung des Zustandes zu sein, mithilfe der Actions 
große Schritte in die richtige Richtung. Das Entkoppeln der Nebeneffekte stellt
auf diese
Weise kein Problem mehr dar, da die App vollständig parallel zur (asynchronen)
Kommunikation mit einem Backend agieren kann; Unterscheidung zwischen side
effects und Klientapplikationslogik ist damit hinfällig.
