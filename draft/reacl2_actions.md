---
layout: post
description: Reacl 2.0 - Side Effects und Actions
title: "Reacl 2.0 - rein funktionale Programmierung, Side Effects und Actinos"
author: marco-schneider
tags: ["reacl", "clojure", "rein funktional"]
page_title: "reacl2 und actions"
---

Vor einigen Tagen schaffte Reacl, eine von uns im Haus entwickelte, rein 
funktionale Bibliothek um [React.js](https://facebook.github.io/react/), 
den Sprung auf Version *2.0.0*. In diesem Artikel betrachten wir einen neu
eingeführten Mechanismus etwas genauer: die Actions.

<!-- more start -->

Vor einigen Tagen schaffte Reacl, eine von uns im Haus entwickelte, rein 
funktionale Bibliothek um [React.js](https://facebook.github.io/react/), 
den Sprung auf Version *2.0.0*.
Schon seit einiger Zeit ist sie bei uns in verschiedensten Projekten in
Verwendung um zuverlässige, schnelle und wartbare grafische Benutzeroberflächen 
(kurz *GUI*, vom englischen Graphical User Interface)
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

## Seiteneffekte und `actions` ##
Ein neuer Mechanismus, welcher in `reacl` 2.0.0 implementiert wurde ist der der 
`action`s &mdash eine Abstraktion mit dem Primärziel, Seiteneffekte zu kapseln 
und deren Logik von der Darstellung zu trennen.
Betrachtet man aktuelle Entwicklungen wie beispielsweise 
[Facebooks GraphQL Biblothek](http://graphql.org/learn/), aber auch der sonst 
üblichen Verwendung asynchroner Kommnukation zwischen Server und Client über 
sogenannte `Ajax`-Anfragen, so wird Eines schnell klar: der Trend geht eindeutig 
weiter in Richtung der Programmlogik auf der Seite des Klienten.
Jedoch ist unser Ziel weiterhin eine _rein funktionale Abbildung_ von Daten auf
eine sogenannte View.
Hier versteckt sich allerdings ein nicht-triviales Problem: wenn einerseits die 
Kommunikationslogik stärkeren Einzug in die Klientenlogik erhält, andererseits
die Grafischen Benutzerschnittstellen eine pure Funktion des 
Applikationszustandes sein soll, wie lassen sich diese zwei Fakten dann in einer 
Bibliothek miteinander vereinbaren?

Hier kommen die oben genanten `actions` ins Spiel, welche in einem kurzen
Beispiel erläutert werden. Zu Beginn betrachten wir folgende Funktion, welche 
zur Darstellung benötigte Daten von eine Server erfragt.

{% highlight clojure %}
(defn fetch-data-from-server!
  "Fetch some data from the server."
  [req]
  ...)
{% endhighlight %}

Was auch ohne konkrete Implementierung schnell klar ist: das hat nichts in der
Logik der Darstellung verloren! Bislang ließ sich die Situation nur unelegant
lösen; an einem Punkt im GUI-Code *musste* nun diese Anfrage abgesetzt werden,
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
  [req]
  (cond
    (message? req)
    (go [response-data (<! (http-get (message-request msg)))]
      (send-answer-to (message-recipient msg) response-data))
    ...))
{% endhighlight %}

 
`handle-action` bekommt hier eine beliebige Nachricht, kodiert in den `message`
record. Wenn die Anfrage nun asynchron verarbeitet wurde sendet diese Funktion
die Antwort an die anfragestellende Komponente, welche diese als reguläre 
Nachricht  entgegennehmen und verarbeiten kann. Und genau das ist der 
Mechanismus der `actions`, welchen wir im nächsten Abschnitt nochmal genauer
unter die Lupe nehmen.

## Ein konkretes Beispiel ##

Betrachten wir folgendes Szenario: eine GUI-Komponente soll den ein Element
einer Liste von Social-Media-Posts darstellen, welche dem Zustand einer 
überliegenden Komponente bereits bekannt sind. 

{% highlight clojure %}
(ns funktionale-programmierung.core
  (:require [active.clojure.record :as r :include-macros true]
            [active.clojure.lens :as lens]
            [reacl2.core :as reacl2 :include-macros true]
            [reacl2.dom :as dom]))

(def initial-posts [{:id 0
                     :body "Reacl 2 is out! Time to celebrate!"}
                    {:id 1
                     :body "Go read the post on Funktionale Programmierung"}])

;; Record to represent the state of the post-component.
;; It contains a list of posts and an optional post if it's details should be
;; shown.
(r/define-record-type post-component-state
  (make-post-component-state posts detail) post-component-state?
  [posts post-component-posts
   (detail post-component-detail post-component-detail-lens)])

;; A message to communicate the intent to show a post detail.
(r/define-record-type show-detail-msg
  (make-show-detail-msg post) show-detail-msg?
  [post show-detail-msg-post])

(reacl2/defclass post-component this []
  ;; A component that renders a list of posts.
  local-state [state (make-post-component-state initial-posts nil)]

  render
  (if-let [detail (post-component-detail state)]
    ;; If there is a detail element, render it via the `post-detail` class.
    (post-detail
     (reacl2/opt :reaction (reacl2/pass-through-reaction this))
     detail)
    ;; If there is no detail element, show all elemetns
    (dom/ul
     (map-indexed
      (fn [idx post]
        (dom/keyed
         (str "post-" idx)
         (dom/li
          (dom/a
           {:onclick #(reacl2/send-message! this (make-show-detail-msg post))
            :href "#"}
           (:body post)))))
      (post-component-posts state))))
  handle-message
  (fn [msg]
    (cond
      (show-detail-msg? msg)  ;; In this case, put the post in the state.
      (reacl2/return :local-state (lens/shove state
                                              post-component-detail-lens
                                              (show-detail-msg-post msg))))))
{% endhighlight %}

Wir kennen hier eine initiale Liste von Posts und darüber hinaus wissen wir, wie
eine solche Liste anzuzeigen ist (nämlich als ungeordnete Liste der Werte des
`:body` Keys jedes Posts). Klickt der Nutzer nun auf eines
der Elemente in der Liste, so sendet sich die Komponente selber die Nachricht
`show-detail-msg` für diesen Post und übernimmt ihn in ihren eigenen, lokalen
Zustand. In solch einem Fall wird dann im nächsten Render-Schritt die
Detailansicht gezeigt. Eine weitere kleine Änderung in Reacl 2: `reaction`s 
werden nun als Option (via `reacl2/opt`) an die Kind-Komponente übergeben. In
unserem Fall wollen wir nur die Nachricht der unterliegenden Komponente an die
Obere weiterleiten und reagieren daher mit einem `pass-through-reaction`.

Werfen wir nun einen Blick auf die Detail Ansicht:

{% highlight clojure %}
(r/define-record-type back-msg (make-back-msg) back-msg? [])
(r/define-record-type post-detail-state
  (make-post-detail-state post comments) post-detail-state?
  [post post-detail-state-post
   comments post-detail-state-comments])

(reacl2/defclass post-detail this post []
  local-state [post+comments (make-post-detail-state post [])]

  render
  (let [post (post-detail-state-post post+comments)]
    (dom/div
      (dom/h1 "Post " (:id post))
      (dom/p (:body post))
      (dom/button {:onclick #(reacl2/send-message! this (make-back-msg))} "back")
      (when-let [comments (post-detail-state-comments post+comments)]
        (dom/div
          (dom/h2 "Comments")
          (dom/ul
          (map-indexed (fn [idx comment]
                          (dom/keyed (str "comment-" idx)
                                    (dom/li comment))) comments))))))
  handle-message
  (fn [msg]
    ;; Singal the parent component we want to navigate back.
    (reacl2/return :app-state msg)))
{% endhighlight %}

Wieder keine großen Überraschungen; die Komponente kennt ihren Zustand, welches
aus dem Post selbst und dessen Kommentaren besteht. Darüber hinaus kann über den
`back`-Button zurück navigiert werden (hier wird die Nachricht `back-msg` über
den `:app-state` der Komponente "nach oben kommuniziert").

Die Frage lautet nun: wie bekommen wir die Kommentare, welche auf einem Server
liegen und Momentan noch nicht im Zustand der Applikation bekannt sind in die
Detailkomponente integriert?

## Actions ##
Genau hier kommen wir wieder auf die Actions zu sprechen. Anstatt wie sonst
einen Weg zu finden, möglichst reibungslos innerhalb der Reacl-Komponente
Seiteneffekte auszuführen greifen wir auf die oben definierte `handle-action`
Funktion zurück. Da wir die Kommentare erst dann brauchen, wenn wir tatsächlich
einen einzelnen Post rendern, sollten wir diese auch erst beim Start der
Komponente anfragen. Das geht dank Reacl2 nun sehr einfach; es muss lediglich 
ein Wert zur `post-detail` Klasse hinzugefügt werden, der beim Start eine
entsprechende Aktion auslöst:

{% highlight clojure %}
(...
  component-did-mount
  #(reacl2/return
    :action
    (make-messsage this [:fetch-comments-for-post (:id post)]))
  ...)
{% endhighlight %}

Wir geben als `return` Wert einfach eine Action an. Diese wird von Reacl an unseren
Action-handler, der den Request verarbeitet (bitte im Kopf behalten, dass
das konkrete Absetzen des Requests hier nur als Platzhalter zu verstehen ist) 
weitergeleitet.
Schließlich, also nachdem der Request verarbeitet wurde und die Antwort 
verfügbar ist benachrichtigt `handle-message` unsere Komponente per regulärer
Reacl Nachricht. Der angepasste action-handler sieht nun so aus

{% highlight clojure %}
(defn handle-action
  "The first argument is the app-state of the sender (which we do not use in
   this example). The second one is the value returned via `return :action`."
  [_ req]
  (cond
    (message? req)
    (go [response-data (<! (http-get (construct-request-from-data req)))]
        (reacl2/send-message! (message-recipient msg) response-data))))
{% endhighlight %}

Im `handle-action` unserer Detailklasse können wir nun diese Antwort als
reguläre Nachricht empfangen und in den Zustand übernehmen. Das sieht so aus:

{% highlight clojure %}
(...
  (fn [msg]
    (cond
      (show-detail-msg? msg)  ;; In this case, put the post in the state.
      (reacl2/return :local-state (lens/shove state
                                              post-component-detail-lens
                                              (show-detail-msg-post msg)))
      (back-msg? msg)
      (reacl2/return :local-state (lens/shove state
                                              post-component-detail-lens
                                              nil))))
  ...)
{% endhighlight %}

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

## Fazit ##
Reacl in seiner aktuellsten Version macht (nicht nur!) bezüglich des Ziels, 
eine rein  funktionale Abbildung des Zustandes zu sein, mithilfe der Actions 
große Schritte
in die richtige Richtung. Das Entkoppeln der Nebeneffekte stellt auf diese
Weise kein Problem mehr dar, da die App vollständig parallel zur (asynchronen)
Kommunikation mit einem Backend agieren kann; Unterscheidung zwischen side
effects und Klientapplikationslogik ist damit hinfällig.
