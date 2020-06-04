---
layout: post
description: "Conditional Restarts in Clojure"
title: "Simple Restarts: Hinter den Kulissen" 
author: simon-haerer
tags: ["common lisp", "conditional", "restarts", "conditional restarts", "effekte", "fehler", "clojure"]
---

Im vorherigen Blogpost haben wir Conditional Restarts in Clojure kennen gelernt
und dabei die Bibliothek "Simple Restarts" verwendet. Diese Bibliothek wurde als
Anschauungsbeispiel erstellt, um Conditional Restarts in Clojure zu erklären. In
diesem Blogpost werden wir einen Blick hinter die Kulissen werfen und verstehen,
wie die Magie hinter "Simple Restarts" implementiert ist: Eine lehrreiche Reise
voller Illusion, Überraschungen und "Aha!"s.

<!-- more start -->

## Voraussetzungen

Neben guten Kenntnissen in Clojure, sollte der Leser die [Einführung zu
Conditional
Restarts](https://funktionale-programmierung.de/2020/04/24/conditional-restarts.html),
die auf "Simple Restarts" aufbaut, gelesen haben. Außerdem ist unsere Blogreihe
zu
[Makros](https://funktionale-programmierung.de/2019/01/30/clojure-macros.html)
in
[Clojure](https://funktionale-programmierung.de/2019/09/17/clojure-macros-2.html)
sehr zu empfehlen, da ein großer Teil der Bibliothek mithilfe von Makros
implementiert wurde. Insbesondere der [dritte
Teil](https://funktionale-programmierung.de/2020/02/18/clojure-macros-3.html)
der Serie ist wichtig, denn neben Makros kommen auch Records aus [Active
Clojure](https://github.com/active-group/active-clojure) zum Einsatz, um
zusammengesetzte Daten zu beschreiben.


## Den Call-Stack hinauf und wieder hinunter

Conditional Restarts ermöglichen das Zusammenspiel von Restarts und Handlern und
das Auslösen von Conditions über mehrere Funktionsaufrufe hinweg. Daher werden
wir zunächst den Call-Stack genauer betrachten und verstehen, wie dieser
funktioniert, um dann Conditional Restarts zu begreifen.

Wird eine Funktion aufgerufen, legt ein Prozessor ein Frame auf den Call-Stack.
Dieser Frame beinhaltet unter anderem die Argumente der Funktion, aber auch
[andere wichtige Informationen.](https://de.wikipedia.org/wiki/Aufrufstapel)
Ruft die aufgerufene Funktion wiederum eine
weitere Funktion auf, wird der Vorgang wiederholt, der Stack wächst. Ist eine
Funktion abgearbeitet, wird sie vom Stack entfernt und die Berechnungen werden
nach der Stelle fortgesetzt, an der der Frame auf den Stack gelegt wurde.

Historisch bedingt wächst der Call-Stack nach unten:

<div id="center" style="padding: 40px 0px">
  <img src="/files/conditional-restarts/callstacks.png"/> 
</div>

Wird `foo` aufgerufen und somit `bar`, liegen beide Funktionen auf dem
Stack (b). Durch die Auswertung von `bar` kommt `baz` hinzu (c), das nach vollständiger
Auswertung wieder entfernt wird (d).

Doch was hat dies mit Conditional Restarts zu tun? Um im Falle einer Condition
den passenden Restart zu finden, müssen wir im Stack zurück und sukzessive in den
vorherigen Frames nach dem ersten passenden Restart suchen. Beispielsweise
müssen wir die Ausführung in `baz` unterbrechen und im Stack rückwärts in `bar`
und dann in `foo` nach passenden Restarts suchen. Ebenso müssen wir aber dafür
sorgen, dass die Handler, die definieren, welcher Restart infrage kommt, in den
tieferen Stack-Frames bekannt sind: So muss ein Handler, der in der `foo` definiert
ist, in `bar` und `baz` sichtbar sein.

Wir benötigen also die Möglichkeit, Informationen in tiefere Stack-Frames zu
reichen, aber auch die, wieder zurückzuspringen. Clojure bietet zwei
Mechanismen, die das ermöglichen:
[Bindings](https://clojuredocs.org/clojure.core/binding) und
[Exceptions](https://clojuredocs.org/clojure.core/ex-info). Zudem verwenden wir
Makros, um die Illusion perfekt zu machen.


## Binden der Handler

Bevor es so richtig losgeht, implementieren wir ein Makro, um Conditions zu
definieren. Um eine Condition zu beschreiben, führen wir einen Record mit den
Feldern `identifier` und `parameters` ein. In dem Makro zur Definition einer
Condition `defcondition` definieren wir einfach eine Funktion, die den Namen des
Identifiers trägt und die Parameter entgegen nimmt:

{% highlight clojure %}

(rec/define-record-type Condition
  (make-condition identifer params) condition?
  [identifer condition-identifier
   params condition-params])

(defmacro defcondition [name params]
  `(defn ~name ~params
     (make-condition ~name ~params)))
     
;; Erstellen einer Condition
(defcondition example-condition [a b c])

{% endhighlight %}
     
Wird also `(example-condition 1 2 3)` aufgerufen, wird ein Record erzeugt, welcher
das definierte Symbol `example-condition` der Funktion selbst als Identifier beinhaltet,
sowie den Vektor `[1 2 3]` im Feld `condition-params`.

Diese Condition kann in der Bibliothek dazu verwendet werden, Handler zu binden: 

{% highlight clojure %}

(bind-handler 
   [example-condition (fn [a] (invoke-restart :my-restart a))]
   ;; Code für den die Bindung des Handler gelten soll:
   ...
)

{% endhighlight %}

Um dieses Makro zu implementieren, verwenden wir Clojure-Bindings in Verbindung
mit [dynamischen Variablen](https://clojure.org/reference/vars). Diese erlauben
es uns, Variablen neu zu binden (_pro Thread_). Die Idee ist, eine Map von
aktuell gültigen Handlern mitzuführen:

{% highlight clojure %}

(def ^:dynamic *handlers* {})

(defmacro handler-bind [bindings & body]
  (assert (even? (count bindings)))
  `(binding [*handlers* (apply assoc *handlers* ~bindings)]
     ~@body))

{% endhighlight %}

Mit der eingebauten Funktion `binding` fügen wir die an `bind-handler`
übergebenen Condition-Handler-Paare in die dynamischen Map `*handlers*` ein. Die
Map verwendet die durch die Condition-Definitionen definierten Funktionen als
Schlüssel, die Werte sind die Handler-Funktionen, die im Vektor jeweils an
gerader Stelle stehen. Wir verwenden hier ein Makro, da in `body` beliebiger
Code stehen kann, der vorerst nicht ausgeführt wird. Wie die Funktion
`invoke-restart` implementiert ist, erfahren wir später.


# Conditions abfeuern

Sind die Handler erstmal in tieferen Stackframes sichtbar, kann das Feuerwerk
beginnen: Conditions können ausgelöst, vom Handler bearbeitet und
Exceptions geworfen werden, um in höheren Stackframes zu einem passenden Restart
zu gelangen. Wir implementieren die Funktion `fire-condition`, die eine Instanz
einer Condition entgegennimmt und eine Funktion `condition-handler`, die
verwendet wird, um aus der dynamischen Map den passenden Handler auszuwählen.

{% highlight clojure %}

(def exception-identifier ::restart-invocation)

(defn condition-handler [name]
  (if-let [handler (get *handlers* name)]
    handler
    (throw (ex-info "No handler found" {:condition name}))))

(defn fire-condition [condition]
  (let [handler        (-> condition condition-identifier condition-handler)
        params         (condition-params condition)
        handler-result (apply handler params)]
    (if (restart-invocation? handler-result)
      (throw (ex-info "" {:type           exception-identifier
                          :handler-result handler-result}))
      (throw (ex-info "No restart invocation returned" {:handler handler})))))

{% endhighlight %}

 In `fire-condition` wird also mithilfe des Condition-Identifiers der passende
Handler ausgwählt und dieser anhand der Condition-Parameter ausgewertet. Handler
müssen stets eine Restart-Invocation (siehe folgenden Abschnitt) zurückgeben.
Die Funktion lässt im nächsten Schritt eine Exception fallen, die diese
Restart-Invocation beinhaltet und vom Typ `::restart-invocation` ist.


# Restart und Restart-Invocation

Sowohl Restarts also auch Restart-Invocations sind lediglich Daten:

{% highlight clojure %}

(rec/define-record-type Restart
  (restart n invocation-function) restart?
  [n restart-name
   invocation-function restart-invocation-function])

(rec/define-record-type RestartInvocation
  (make-restart-invocation restart-name params) restart-invocation?
  [restart-name restart-invocation-restart-name
   params restart-invocation-params])

(defn invoke-restart [sym & params]
  (make-restart-invocation sym params))

{% endhighlight %}

Ein Restart besteht aus einem Namen für den Restart und einer Funktion für den
Wiedereinstieg. Eine Restart-Invocation dagegen beinhaltet den Namen des
auszulösenden Restarts, sowie einer Liste von Parametern für die Funktion für
den Wiedereinstieg.

Restarts können anhand der Bibliothek folgendermaßen in den Code eingehängt
werden:

{% highlight clojure %}

(restart-case 
    ;; do something here, e.g
    (fire-condition (example-condition 1 2 3))
    (restart :my-restart-1 
           (fn [baz] ...)) 
    (restart :my-restart-2
           (fn [buzz] ...)))

{% endhighlight %}
               
`restart-case` nimmt einen Ausdruck entgegen, der ausgewertet werden soll und
eine Liste von Restarts, die im Falle einer Condition für diesen Ausdruck gelten
sollen. Da das Auslösen einer Condition über Exceptions den Stack aufwärts
kommuniziert, fangen wir diese im Makro auf:


{% highlight clojure %}

(defn restart-invocation-exception? [e]
  (= (:type (ex-data e)) exception-identifier))

(defn find-restart [restarts name]
  (first (filter (fn [restart] (= (restart-name restart) name)) restarts)))

(defn restart-case-catch [e available-restarts]
  (if (restart-invocation-exception? e)
    (let [handler-result   (:handler-result (ex-data e))
          restart-name     (restart-invocation-restart-name handler-result)
          restart-params   (restart-invocation-params handler-result)]

      (if-let [restart (find-restart available-restarts restart-name)]
        (apply (restart-invocation-function restart) restart-params)
        (throw e)))
    (throw e)))

(defmacro restart-case [body & restarts]
  `(try
    ~body
    (catch Exception e#
      (restart-case-catch e# ~(vec restarts)))))
          
{% endhighlight %}

Die Magie passiert in der Funktion `restart-case-catch`: Diese überprüft zuerst,
ob es sich um die gewünschte Exception handelt---ansonsten wird die Exception
einfach erneut geworfen. Im Falle einer Restart-Invocation-Exception wird der
Name und die Parameter des auszulösenden Restarts aus der Restart-Invocation
extrahiert und der passende Restart aus der Liste der übergebenen Restarts gesucht.

Existiert kein passender Restart, wird die Exception erneut geworfen. Es kann ja
sein, dass ein passender Restart in höheren Stack-Frames definiert wurde. Findet
sich ein solcher Restart, wird die Wiedereinstiegsfunktion ausgewertet---und
fertig ist das Kunststück.


# Fazit

In diesem Blogpost haben wir die Tricks, die hinter der "Simple
Restarts"-Bibliothek stehen, nachvollzogen. Es wurde gezeigt, wie die zwei
Mechanismen, Bindings und Exceptions, verwendet werden, um bidirektional über
den Stack hinweg zu kommunizieren. Makros helfen dabei, wie Illusionen, über die
eigentlichen Vorgänge hinwegzutäuschen und ermöglichen es dem Anwender der
Bibliothek ohne Verständnis der Interna, Sachverhalte abzubilden---wir
Informatiker nennen dies Abstraktion. Lisps und ihr mächtiges Makrosystem
erleichtern die Implementierung solcher Bibliotheken enorm.

Die Bibliothek hat auch Schwächen: Neben Fehlerbehandlung lässt sich die Verwendung
von Exceptions nicht vollständig vor dem Anwender verbergen: Fängt dieser an
einer Stelle alle Exceptions ab, funktioniert auch der Restart-Mechanismus nicht
mehr. Dies könnte vom Anwender nicht gewollt sein und aus Unwissenheit über die
Interna der Bibliothek passieren. Für Anschauungszwecke reicht der
Exception-Mechanimus jedoch vollkommen.

Der vollständige Quelltext der Bibliothek ist [auf GitHub
verfügbar.](https://github.com/smoes/simple-restarts)
