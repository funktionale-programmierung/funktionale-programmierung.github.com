---
layout: post
description: "Conditional Restarts in Clojure"
title: "Ein alter Hut: Conditional Restarts" 
author: simon-haerer
tags: ["common lisp", "conditional", "restarts", "conditional restarts", "effekte", "fehler", "clojure"]
---

Viele unserer Blogposts beschäftigen sich damit, wie effektbehaftete Programme
beschrieben und getestet werden können. Dabei kommen freie Monaden oder
algebraische Effekte zum Einsatz---Effektsysteme sind gerade ein heißes Thema.
Common Lisp denkt sich: Das ist ein alter Hut. Denn mit Conditional Restarts ist
ein mächtiger Mechanismus zur Abbildung von Effekten im Sprachkern vorhanden.
Wir haben diesen in Clojure nachprogrammiert und werden Conditional Restarts in
einer zweiteiligen Blogpost-Serie vorstellen. In diesem ersten Teil werden
Conditional Restarts erklärt und an Beispielen die praktischen
Einsatzmöglichkeiten aufgezeigt.

<!-- more start -->

## Voraussetzungen

Neben Kenntnissen von Clojure hilft es dem Leser, die vorausgegangenen Artikel
über [Koka-Effekte](https://funktionale-programmierung.de/2019/10/24/algebraic-effects.html) gelesen zu haben.


## Über Fehler, Seiteneffekte und Neuanfänge

Beim Entwickeln von Software steht man immer wieder vor der Frage, wie mit
Fehler umgegangen werden soll. Schlägt das Verarbeiten einer Eingabe fehl oder
stürzt der Datenbankservice ab, soll dies abgefangen und möglichst reibungslos
verarbeitet werden. In der Regel geht es bei der Fehlerbehandlung um die
Bearbeitung von Einflüssen von Seiteneffekten, also unvorhersehbarem Verhalten.
Verschiedene Sprachen und Paradigmen bieten hier unterschiedliche Lösungen: Dies
reicht von Exceptions, über den Einsatz von Monaden, um Seiteneffekte an den
Rand der Ausführung zu drängen, um sie dort explizit abzuhandeln, bis hin zu
"let it crash".

Während Monaden dem Programmierer meist zusätzliche ungewünschte Komplexität
aufbürden, kommen auch Exceptions mit Limitierungen: So lassen sich zwar Fehler
in tieferen Callstack-Frames abfangen, doch was dann? Meist reicht es nur für
ein Graceful Shutdown, denn der Wiedereinstieg an der Stelle des Auftretens des
Fehlers ist nicht so einfach möglich. Der Programfluss wird unterbrochen.

Im Folgenden betrachten einen mächtigen Mechanimus, der die genannten Nachteile
von Exceptions beseitigt und somit für weit mehr als Fehlerbehandlung verwendet
werden kann.


## Simple Restarts, eine beispielhafte Clojure-Bibliothek

In vorherigen Blogposts haben wir bereits das Effektsystem der jungen
Programmiersprache Koka kennengelernt. Damit kann im Fehlerfall nach Behandlung
wieder zurück in den Code besprungen werden und somit können Fehler an Ort und
Stelle repariert werden. Common Lisp kann das schon lange: Die fast 40 Jahre
alte Sprache implementiert Conditional Restarts: bedingte Neustarts. Wir werden
in diesem Blogpost eine kleine Clojure Bibliothek namens Simple Restarts
verwenden, um Conditional Restarts zu erklären. Diese bietet eine einfache
Implementierung von Conditional Restarts, die sich syntaktisch nahe an das
Common-Lisp-Original hält. Im Folgeblogpost wird die Implementierung dieser
Bibliothek diskutiert.

Die Bibliothek kann auf [Github](https://github.com/smoes/simple-restarts)
 gefunden oder über [Clojars](https://clojars.org/simple-restarts) eingebunden
 werden. Alle Code-Beispiele sind zudem auf [Github
 verfügbar.](https://github.com/smoes/blogpost-conditional-restarts).


## Der klassiche Zeilenparser

In den meisten Beispielen über Conditional Restarts wird ein Parser zur
Veranschaulichung verwendet, der in verschiedenen Kontexten Eingaben (also
Seiteneffekte) unterschiedlich verarbeitet. Einmal wird dieser von einem
Compiler aus aufgerufen, einmal aus einer interaktiven Konsole. Der Compiler
soll im Falle eines Fehlers das Parsen abbrechen, die Benutzerkonsole unbeirrt
die nächste Eingabe entgegennehmen. Die Grundfunktionalität des Parsers ist hier
vereinfacht implementiert:


{% highlight clojure %}

(defn parse-line [line]
    (if (valid? line)
      (do-parse line)
      (error "invalid line!")))
          
(defn parse-lines []
    (for [line some-source]
        (parse-line line)))

{% endhighlight %}
            
Die Funktionen sind exemplarisch zu verstehen, weder `do-parse`, `error` noch
`some-source` werden weiter erklärt. Interessant ist die Frage, wie mit dem
Fehler, der aus `error` resultiert umgegangen werden soll, da das Verhalten im
Fehlerfall kontextabhängig ist. Und hier kommen Conditional Restarts ins Spiel.


## Conditions

Wie der Name bereits sagt, benötigen wir Conditions. Conditions sind ein Mittel,
um einen bestimmten Zustand zu signalisieren, zum Beispiel einen Fehlerzustand.
Um einen Zustand zu definieren, benutzen wir `defcondition` aus der verwendeten
Bibliothek. Eine Condition kann Argumente entgegennehmen, die bei der späteren
Verarbeitung verwendet werden können, hier `line`:


{% highlight clojure %}

(defcondition invalid-line-error [line])

{% endhighlight %}

Nun erzeugen wir die Condition anstelle des Fehlers und signalisieren zudem
gleich, dass die Condition behandelt werden soll, indem wir `fire-condition`
aufrufen:

{% highlight clojure %}

(defn parse-line [line]
    (if (valid? line)
      (do-parse line)
      (fire-condition (invalid-line-error line))))

{% endhighlight %}

In diesem Beispiel ist die Condition ein Fehler, jedoch wird später klar, dass
eine Condition mehr als nur Fehler repräsentieren kann.

## Restarts

Mögliche Wiedereinstiegspunkte im Falle einer Condition werden über sogenannte
Restarts definiert. Diese können wir an frei wählbaren Punkten in unserem
Program definieren, sie müssen im Stack nur unterhalb der Condition liegen, zum
Beispiel in der `parse-lines`-Funktion.

Restarts werden jeweils über einen Namen und eine Funktion definiert, die
beschreibt, wie der Neustart abläuft. Die Funktion kann Argumente
entgegennehmen, die wir anhand von Handlern (siehe nächster Abschnitt) übergeben
können. Restarts werden in der `restart-case`-Funktion definiert. Diese nimmt
als erstes Argument eine Funktion entgegen, für deren bei der Ausführung
ausgelöste Conditions die Restarts gültig sein sollen. Im Folgenden sind zwei
Restarts definiert, einer, der eine fehlerhafte Zeile in der interaktiven
Konsole einfach überspringt und ein weiterer, der im Compiler-Fall zum Abbruch führt:


{% highlight clojure %}

(defn parse-lines []
    (for [line some-source]
        (restart-case (parse-line line)
           (restart :skip-line 
               (fn [line]
                   (println "Skipping line " line))) 
           (restart :abort
               (fn [line] 
                   (println "Aborting after invalid line " line)
                   ;; abort parsing via other condition or exception
                   ;; ...
                   )))))

{% endhighlight %}
               
Der Restart mit dem Namen `:skip-line` macht nichts, außer den Fehler auszugeben
und die nächste Schleifeniteration zuzulassen.

Restarts ermöglichen verschiedene Strategien:

- Neustart mit Standardwert/abgeändertem Wert (zum Beispiel durch erneuten
  Aufruf von `parse-line` im Restart)
- Ignorieren von Durchläufen
- Abbruch der Ausführung
- Rückgabe eines Standardwertes
- ...

Im folgenden Abschnitt wird erklärt, wie wir vom Auslösen einer Condition zu einem
passenden Restart kommen.


## Handler

Handler behandeln Conditions und geben an, welcher Restart ausgeführt werden
soll. Handler werden im Stack weiter unten definiert, als die Restarts. In
unserem Beispiel kann das der Aufrufer der `parse-lines`-Funktion machen. 

Ein Handler ist eine Funktion, die die Argumente einer bestimmten Condition
entgegennimmt und einen Befehl zur Ausführung eines Restarts zurückgibt. Handler
werden über das Makro `bind-handler` stets an eine Condition gebunden.
`bind-handler` nimmt als letztes Argument außerdem Code entgegen, für dessen
Ausführung der Handler gebunden werden soll:

{% highlight clojure %}

(defn parse-lines-compiler []
   (handler-bind 
       [invalid-line-error (fn [line]
                               (invoke-restart :abort line))] 
       (parse-lines)))
       
{% endhighlight %}

In diesem Beispiel wurde die Funktion `parse-lines` für den Compiler-Fall
spezialisiert, indem der Handler für die `invalid-line-error`-Condition den
Restart `:abort` auswählt. Dazu gibt der Handler `invoke-restart`, das den Namen
des Restarts und Parameter enthält, die dem Restart übergeben werden sollen, zurück.
Ein `parse-lines-console` würde als Restart hingegen `:skip-line` wählen:

{% highlight clojure %}

(defn parse-lines-console []
   (handler-bind 
       [invalid-line-error (fn [line]
                               (invoke-restart :skip-line line))] 
       (parse-lines)))

{% endhighlight %}

Wird der Code ausgeführt, wird im Fehlerfall die Condition ausgelöst, ein
gebundener Handler zu der jeweiligen Condition ausgeführt und anhand des
Rückgabewertes zum passenden Restart gesprungen. Die Funktion, die im Restart
definiert wurde, wird ausgeführt und damit die Ausführung fortgesetzt.

Conditional Restarts erlauben es, von außen das Verhalten im Falle einer
vorher definierten Condition zu bestimmen. Anders als bei Exceptions haben wir
durch Restarts viele Möglichkeiten auf die Conditions zu reagieren und den
normalen Programmfluss weiterzuführen. Wie im folgenden Abschnitt gezeigt wird,
ermöglichen Conditional Restarts weit mehr, als nur Fehlerbehandlung.


## Effekte mit Conditional Restarts

Im Artikel über
[Koka](https://funktionale-programmierung.de/2019/10/24/algebraic-effects.html)
wurde gezeigt, wie Effekte verwendet werden, um kontextabhängige Behandlung zu
ermöglichen. Ein gerne verwendetes Beispiel sind Datenbankoperationen, die sich
in Tests anders handhaben lassen als in Produktion, wo die echte Datenbank
läuft. Conditional Restarts können verwendet werden, um diese Mechanik
nachzustellen:

{% highlight clojure %}

(defcondition database-effect [op params])

(defn database [op & params]
  (restart-case
    (fire-condition (database-effect op params))
    (restart :return-value identity)))

(defn do-database-stuff []
  (database :put 1 "Kaan")
  (database :put 2 "Tim")
  (database :put 3 "Simon")
  [(database :get 2) (database :get 1) (database :get 3)])

{% endhighlight %}
      
      
Dazu wird zuerst eine Condition `database-effect` definiert, die eine Operation
und Parameter entgegennimmt. Die Datenbank ist in diesem Fall lediglich ein
Atom, das später interpretiert wird. Die Funktion `database` konstruiert den
Datenbank-Effekt, feuert ihn ab und definiert den Wiedereinstieg im selben
Aufruf. Der zugehörige Restart gibt den Wert, der durch den Handler übergeben
wird, unverändert zurück. Die Funktion `do-database-stuff` zeigt, wie dieses
Konstrukt verwendet werden kann. Es fehlt nur noch ein Handler:

{% highlight clojure %}

(def db-atom (atom {}))

(defn database-get [k]
  (get @db-atom k))

(defn database-put [k v]
  (swap! db-atom assoc k v))

(defn database-interpreter [op params]
  (case op
    :get (apply database-get params)
    :put (apply database-put params)))

(defn database-handler [op params]
  (invoke-restart :return-value (database-interpreter op params)))

(defn do-database-stuff-handled []
  (handler-bind
    [database-effect database-handler]
    (do-database-stuff)))
    
{% endhighlight %}

`database-handler` nimmt die Operation und deren Parameter entgegen und gibt sie
an einen Interpreter weiter. Dieser wertet die jeweiligen Aufrufe gegen ein Atom
aus. Das Ergebnis wird vom Handler an das in `database` definierte Restart
`:return-value` übergeben. Der Handler wird in `do-database-stuff-handled` an
die Condition `database-effect` gebunden. Dieser Handler könnte für Tests
verwendet werden, in denen die echte Datenbank nicht verfügbar ist. Die Funktion
`do-database-stuff` bleibt davon unbeeinflusst. Ein Aufruf von
`do-database-stuff-handled` gibt `["Tim", "Kaan", "Simon"]` zurück.


## Fazit

Wie zu sehen ist, sind Conditional Restarts ein Mechanismus, der als Grundlage
für viele hilfreiche Techniken dienen kann. Dabei sind Conditional Restarts
keine Neuentdeckung, es gibt sie schon mehrere Jahrzehnte. Dennoch sieht man
sie außerhalb von Common Lisp kaum im Einsatz. 

Wir haben gezeigt, dass man sich mit Conditional Restarts die Komplexität von
Monaden sparen und trotzdem die Behandlung von Seiteneffekten spielend leicht
austauschen kann. Inbesondere können Handler am Rande der Ausführung gebunden
werden, zum Beispiel in den Routen-Definitionen eines Webservers. Das ermöglicht
den einfachen Austausch, etwa für Tests, indem für die Ausführung der
Businesslogik ein anderer Handler gebunden wird. Beschreibung lässt sich von
Ausführung auf natürliche Art und Weise entkoppeln.

Bei der Behandlung von Fehlern helfen die Restarts, das Program trotzdem
dort fortzusetzen, wo der Fehler aufgetreten ist---abermals abhängig vom
Einsatzkontext des Programmcodes.

In einem folgenden Blogpost werden wir uns den Code von Simple Restarts genauer
ansehen und verstehen, wie die Bibliothek implementiert ist.
