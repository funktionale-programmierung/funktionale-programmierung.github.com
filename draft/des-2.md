---
layout: post
description: Ereignisorientierte Simulation mit funktionaler Programmierung, Teil 2
title: "Ereignisorientierte Simulation mit funktionaler Programmierung, Teil 2"
author: michael-sperber
tags: ["Haskell", "Java"]
---

Hier ist endlich der zweite Teil zur Übersetzung eines in Java
geschriebenen ereignisorientierten Simulators nach Haskell.

<!-- more start -->

Der [erste Teil]({% post_url 2016-03-09-des-1 %}) hat sich damit damit
beschäftigt, das Kernmodells des Simulators so direkt wie möglich nach
Haskell zu übersetzen. Das hat weitestgehend auch funktioniert.  Zwei
auffällige Unterschiede hat es aber gegeben:

- Wir haben für die Variablen des Modellzustands (wo in Java
  `Object` steht) eine Typvariable eingeführt.

- Für Zustandsänderungen und Verzögerungen (die Zugriff auf einen
  Zufallszahlengenerator benötigen) haben wir jeweils Monaden für
  die `void`-Methoden in Java benutzt.

Als nächstes schauen wir uns den eigentlichen Simulator an - wie
gehabt erstmal den Original-Java-Code, und dann die Übersetzung nach
Haskell.

## Simulationsausführung ##

Der Simulator muss noch ausstehend Ereignisse verwalten und die daraus
entstehenden Zustandsänderungen durchführen.  

Dazu fehlt dem Java-Code bisher noch das Konzept einer
*Ereignis-Instanz*, also das Wissen, dass ein gegebenes Ereignis zu
einem bestimmten Zeitpunkt stattfinden soll.  Dazu wird das Ereignis
einfach zusammen mit dem Zeitpunkt in ein Objekt gepackt:

{% highlight java %}
public class EventInstance implements Comparable<EventInstance> {

    private final Event event;
    private final Long time;
{% endhighlight %}

Für die beiden Felder gibt es noch einen Konstruktor und
Getter-Methoden:

{% highlight java %}
    protected EventInstance(Long time, Event event) {
        this.event = event;
        this.time = time;
    }

    protected Event getEvent() {
        return event;
    }

    protected Long getTime() {
        return time;
    }
{% endhighlight %}

Später soll der Simulator die Ereignisse natürlich in zeitlich
aufsteigender Reihenfolge ausführen.  Außerdem haben Ereignisse ja
noch eine Priorität, welche die Reihenfolge mehrerer Ereignis-Instanzen
festlegt, die zur gleichen Zeit stattfinden sollen. Deswegen werden
`EventInstance`-Objekte über das `Comparable`-Interface vergleichbar
gemacht.  Dieses spezifiziert eine `compareTo`-Methode, die ein Zahl
zurückliefert, die negativ bei "kleiner", 0 bei "gleich" und 1
bei "größer" ist:

{% highlight java %}
    public int compareTo(EventInstance eventInstance) {
        int result = 0;
        if (this.time.compareTo(eventInstance.getTime()) == 0) {
            result = Integer.compare(this.getEvent().getPriority(),
                                   eventInstance.getEvent().getPriority());
        } else {
            result = this.time.compareTo(eventInstance.getTime());
        }
        return result;
    }
}
{% endhighlight %}

Die Ereignis-Instanzen werden dann in einer Ereignisliste in der
richtigen Reihenfolge verwaltet.  Für diese Liste gibt es eine Klasse
`EventList`:

{% highlight java %}
public class EventList {

    private List<EventInstance> eventList;

    public EventList() {
        this.eventList = new ArrayList<EventInstance>();
    }
{% endhighlight %}

Die Methode `addEvent` steckt eine Ereignis-Instanz einfach ans Ende
von `eventList`:

{% highlight java %}
    public void addEvent(EventInstance eventinstance) {
        this.eventList.add(eventinstance);
    }
{% endhighlight %}

Ich war erst etwas verwirrt, weil `addEvent` keine Rücksicht auf die
gewünschte Reihenfolge der Ereignis-Instanzen nimmt.  Das macht
erst `removeNextEvent`, welches die erste stattzufindende Ereignis-Instanz
liefert und entfernt:

{% highlight java %}
    public EventInstance removeNextEvent() {
        Collections.sort(this.eventList);
        return this.eventList.remove(0);
    }
{% endhighlight %}

Das sieht ziemlich ineffizient aus - erst wird sortiert, und das
`remove` muss in `ArrayList` alle Elemente um eins nach vorn schieben.
Besser wäre eine *priority queue*, die es in Java auch
[in der Standardbibliothek](https://docs.oracle.com/javase/7/docs/api/java/util/PriorityQueue.html)
gibt.  In Haskell werden wir uns nach sowas auch mal umsehen.

Eine weitere Hilfsklasse wird noch benötigt, um die Simulation
durchzuführen.  Sie verwaltet die aktelle Zeit (einfach eine
aufsteigende Zahl) und heißt `Clock`:

{% highlight java %}
public class Clock {
    
    private Long time;
    public Clock(Long time) {
        this.time = time;
    }

    public Long getCurrentTime() {
        return time;
    }
    
    public void setCurrentTime(Long currentTime) {
        this.time = currentTime;
    }
    
}
{% endhighlight %}


Nun wird die Simulation durchgeführt - das passiert in einer Klasse
`MainProgram`:

{% highlight java %}
public class MainProgram {

    private Model model;
    private ModelState modelState;
    private EventList eventList;
    private Clock clock;
    private ReportGenerator reportGenerator;
{% endhighlight %}


Die Klassen `Model` und `ModelState` kennen wir schon vom
[letzten Mal]({% post_url 2016-03-09-des-1 %}).  Die letzte Klasse
`ReportGenerator` ist dafür da, Informationen über den Verlauf der
Simulation aufzunehmen, um sie später (zum Beispiel in Form eines
Ausdrucks) wiederzugeben.  Da diese Klasse nur "beobachtende Funktion"
hat, vertagen wir ihre Diskussion erstmal und stürzen uns ins
Getümmel.  Die Methode `runSimulation` stößt die Simulation an:

{% highlight java %}
  public void runSimulation(Model model, Long endTime,
                            ReportGenerator reportGenerator) {
      this.model = model;

      initializationRoutine(reportGenerator);

      while (this.clock.getCurrentTime() <= endTime && this.eventList.getSize() > 0) {
          EventInstance currentEvent = this.timingRoutine();
          eventRoutine(currentEvent);

      }
  }
{% endhighlight %}

Es wird also erstmal initialisiert und dann die Simulation solange
iteriert, bis keine Ereignis-Instanzen mehr übrig sind.  Die
Initialisierung ist in dieser Methode:

{% highlight java %}
    private void initializationRoutine(ReportGenerator reportGenerator) {
        this.clock = new Clock(0L);

        this.modelState = new ModelState();
        this.reportGenerator = reportGenerator;

        this.eventList = new EventList();
        EventInstance initialEvent = new EventInstance(this.clock.getCurrentTime(),
                                                       this.model.getStartEvent());
        this.eventList.addEvent(initialEvent);
    }
{% endhighlight %}

Entscheidend ist die Initialisierung der Ereignis-Liste, in die das
Anfangsereignis aus dem Modell eingefügt wird.

Die eigentliche Ausführung der Simulation ist in zwei Schritte
aufgeteilt: Die `timingRoutine` ist dafür zuständig, die Zeit
voranzuschalten und die nächste Ereignis-Instanz zu liefern:

{% highlight java %}
    private EventInstance timingRoutine() {
        EventInstance result = this.eventList.removeNextEvent();
        this.clock.setCurrentTime(result.getTime());
        return result;
    }
{% endhighlight %}

Die `eventRoutine` schließlich kümmert sich um die Anwendung dieser
Ereignis-Instanz:

{% highlight java %}
    private void eventRoutine(EventInstance eventInstance) {
        Event event = eventInstance.getEvent();
        for (StateChange stateChange : event.getStateChanges()) {
            stateChange.changeState(this.modelState);
        }

        this.reportGenerator.update(eventInstance.getTime(), this.modelState);

        for (Transition transition : event.getTransitions()) {

            if (transition.getCondition().isTrue(this.modelState)) {
                Event targetEvent = transition.getTargetEvent();
                Delay delay = transition.getDelay();

                this.eventList.addEvent(
                   new EventInstance(this.clock.getCurrentTime() + delay.getDelay(),
                                     targetEvent));
            }

        }

    }
{% endhighlight %}

Zunächst wendet die `evenRoutine` die Zustandsänderungen der
Ereignis-Instanz.  Dann ermittelt sie die anwendbaren
Zustandsübergänge, zieht daraus neue Ereignis-Instanzen und steckt
diese in die Ereignis-Liste.  (Dazwischen wird noch der
`reportGenerator` informiert.)

Das reicht erstmal wieder an Java-Code ...

## Von Java nach Haskell

... und nun wieder die Übertragung nach Haskell.

Zuerst ist `EventInstance` dran, da funktioniert die direkte Übersetzung:

{% highlight haskell %}
data EventInstance v = EventInstance Time (Event v)
  deriving (Show, Eq)
{% endhighlight %}

Das Haskell-Gegenstück zum Java-Interface `Comparable` ist die
Typklasse `Ord`, deren Methode `compare` ebenso funktioniert wie
`compareTo`:

{% highlight haskell %}
instance Ord (EventInstance v) where
  compare (EventInstance t1 e1) (EventInstance t2 e2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x
{% endhighlight %}

Für `EventList` benutzen wir in Haskell eine *priority queue*.  Dafür
gibt es in Haskell gleich mehrere Packages, wir benutzen
[`Data.Heap`](https://hackage.haskell.org/package/heap/docs/Data-Heap.html).
Da ist ein Typ `MinHeap` definiert, der exakt das macht, was wir
brauchen - aus einer sortierten Folge von Elementen das jeweils erste
herausholen und etnfernen.  Importiert wird das so:

{% highlight haskell %}
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
{% endhighlight %}

Als Pendant für `EventList` brauchen wir dann gar keinen eigenen Typ,
sondern können direkt `MinHeap (EventInstance v)` hinschreiben.

Für `Clock` definieren wir einen Record-Typ, der dem Java-Typ direkt entspricht:

{% highlight haskell %}
newtype Clock = Clock { getCurrentTime :: Time }
  deriving Show
{% endhighlight %}

Jetzt wird es aber tatsächlich etwas schwieriger - der Java-Code
mutiert fröhlich im `ModelState` herum, um die Ausführung
voranzubringen.  In Haskell muss das innerhalb der
`ModelAction`-Monade vom letzten Mal erfolgen.  Zur Erinnerung, das
war eine Instanz der Zustandsmonade:

{% highlight haskell %}
type ModelAction v = State.State (ModelState v)
{% endhighlight %}

Außerdem müssen wir ja noch die Delays ausführen - die laufen auch in
einer Monade:

{% highlight haskell %}
type Random = Random.Rand Random.StdGen
{% endhighlight %}

Das sind also zwei *unterschiedliche* Monaden, die aber innerhalb
*derselben* Simulation laufen sollen: Doof!  Wir müssen einen Weg
finden, beide zu kombinieren.

Diese Monade ist bisher nur `exponentialDelay` geschuldet, das eine
Zufallszahl braucht.  Die `ModelAction`-Monade ist aber wirklich
intrinisch zur Simulation selbst.  Gut wäre also eine Möglichkeit, die
`ModelAction`-Monade mit einer *beliebigen* anderen Monade zu
kombinieren.

In Haskell geht sowas mit einem
[*Monaden-Transformator*](https://en.wikibooks.org/wiki/Haskell/Monad_transformers),
also einer Abbildung, die aus einer Monade eine andere Monade macht.
Glücklicherweise gibt es bei Zustandsmonade aus
[`Control.Monad.Strict`](https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Strict.html)
nicht nur die "fertige" Monade `State` sondern auch einen
Transformator
[`StateT`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Strict.html#t:StateT).
Den benutzen wir, um bei `ModelAction` über die "innere Monade" zu
abstrahieren - und fangen uns dabei einen weiteren Typparameter ein:

{% highlight haskell %}
type ModelActionT v m = State.StateT (ModelState v) m
{% endhighlight %}

Den müssen wir jetzt durch den Haskell-Code durchfädeln - immerhin
sagt uns der Compiler, wenn er noch wo fehlt.  Folgende Typen bekommen
also noch ein `m` am Ende:

{% highlight haskell %}
data Transition v m
data Model v m
data Event v m
type StateChange v m
data EventInstance v m
{% endhighlight %}

Den "Kerncode" selbst müssen wir allerdings nicht ändern, das betrifft
erstmal nur die Typen.  Z.B. sieht `getValue` jetzt so aus:

{% highlight haskell %}
getValue :: Monad m => String -> ModelActionT v m v
getValue name =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     return v
{% endhighlight %}

Jetzt können wir den Typ für die große Simulationsmonade definieren.
Dazu müssen wir ja auch noch die aktuelle Uhrzeit sowie die
Ereignis-Liste verwalten, ebenfalls als Teil einer Zustandsmonade.
Außerdem kommt noch der Report-Generator dazu, für das es einen
Typparameter `r` gibt.  Fertig sieht das so aus:

{% highlight haskell %}
data SimulationState r v m = SimulationState {
  clock :: Clock,
  events :: MinHeap (EventInstance v m),
  reportGenerator :: r
}

type Simulation r v m = State.StateT (SimulationState r v m) (ModelActionT v m)
{% endhighlight %}

Dies ist also eine Typdefinition für eine Monade, in der zwei
Zustandsmonaden kombiniert werden - die äußere verwaltet ein Tripel
aus Uhrzeit (`Clock`), Ereignis-Liste (`MinHeap`) und Report-Generator
(`r`).  Die innere Monade verwaltet den Modell-Zustand.

Bei der Hauptfunktion kümmern wir uns erstmal um die innere Schleife.
(Die Initialisierung kommt dann als Teil der
Simulations-Hauptfunktion, weil diese Reihenfolge leichter zu
erläutern ist.)  Sie orientiert sich stark an der Java-Version: Sie
akzeptiert eine Endzeit und liefert dann eine Berechung in der Simulations-Monade:

{% highlight haskell %}
simulation :: ReportGenerator r v => Time -> Simulation r v Random ()
simulation endTime =
  let loop =
        do ss <- State.get
           if ((getCurrentTime (clock ss)) <= endTime) && not (Heap.null (events ss)) then
             do currentEvent <- timingRoutine
                updateModelState currentEvent
                updateStatisticalCounters currentEvent
                generateEvents currentEvent
                loop
           else
             return ()
  in loop
{% endhighlight %}

Da diese Iteration auch Delays ausführen muss, ist jetzt der
Typparameter `m` mit `Random` instanziert.

Um überhaupt zu überprüfen, ob die Iteration am Ende angelangt ist,
muss `loop` die entsprechenden Werte aus dem Zustand holen - mit
`State.get`.  Sie ruft dann `timingRoutine` auf.  Der Inhalt der
Java-Methode `eventRoutine` ist dann auf ihre drei Aufgaben verteilt:

- `updateModelState` aktualisiert den Modell-Zustand
- `updateStatisticalCounters` aktualisiert den Report-Generator
  (vertagen wir)
- `generateEvents` generiert neue Events und stellt diese in die Event-Liste.

Die Funktion `timingRoutine` ist das Pendant zur gleichnamigen
Java-Funktion:

{% highlight haskell %}
timingRoutine :: Monad m => Simulation r v m (EventInstance v m)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result
{% endhighlight %}

Diese entspricht direkt der Java-Version.  Die Aufgabe der
Java-Methode `removeNextEvent` übernimmt die Funktion `getNextEvent`.
Diese geht davon aus, dass in der Ereignis-Liste mindestens ein
Ereignis steckt:

{% highlight haskell %}
getNextEvent :: Monad m => Simulation r v m (EventInstance v m)
getNextEvent =
  do ss <- State.get
     case Heap.view (events ss) of
       Just (ev, evs') ->
         do State.put (ss { events = evs' })
            return ev
       Nothing -> fail "can't happen"
{% endhighlight %}

Die `getNextEvent`-Funktion holt sich die Ereignis-Liste aus dem
Zustand und benutzt dann `Heap.view`, um das erste Ereignis und die
restliche Ereignis-Liste zu extrahieren.  Die restliche Ereignis-Liste
wird dann zurück in den Zustand geschrieben.

Hier weist uns Haskell auf einen unbefriedigenden Aspekt des
originalen Java-Codes hin: Dass noch Elemente in der Ereignis-Liste
sind, muss vom Aufrufer von `getNextEvent` sichergestellt werden.
Besser wäre es, das Pattern-Matching in die `simulation`-Funktion zu
integrieren.

Kommen wir zur Funktion `updateModelState`, die den Modell-Zustand
aktualisiert.   Sie muss "einfach" nur die Zustands-Änderungen, die im
Ereignis-Objekt stecken, nacheinander in der Simulations-Monade
ausführen.  Dazu gibt es schon eine passende monadische Operation
`sequence_`, sieht also im Kern ganz einfach aus:

{% highlight haskell %}
sequence_ (stateChanges ev)
{% endhighlight %}

Allerdings laufen die `stateChanges` nicht in der Simulations-Monade
ab, sondern in der `ModelAction`-Monade, also der *inneren* Monade von
`Simulation`.  Wir müssen den Ausdruck also noch auf die
Simulations-Monade ausdehnen.  Dazu gibt es die Funktion `lift`:

{% highlight haskell %}
updateModelState :: Monad m => EventInstance v m -> Simulation r v m ()
updateModelState (EventInstance _ ev) = State.lift (sequence_ (stateChanges ev))
{% endhighlight %}

Es bleibt `generateEvents`.  Hier müssen wir über die Transitionen
eines Ereignisses iterieren (dazu gibt es die eingebaute
Monaden-Funktion `mapM_`), die Bedingung überprüfen, den Delay
berechnen und dann die entstehende Ereignis-Instanz in die
Ereignis-Liste einfügen.  Wieder müssen wir daran denken, Operationen
aus der `ModelState`-Monade in die `Simulation`-Monade zu "liften".
Bei der `Random`-Monade müssen wir sogar zweimal liften!

{% highlight haskell %}
generateEvents :: EventInstance v Random -> Simulation r v Random ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ms <- State.lift getModelState
             if condition tr ms then
               do ss <- State.get
                  d <- State.lift (State.lift (delay tr))
                  let evi = EventInstance ((getCurrentTime (clock ss)) + d)
                                          (targetEvent tr)
                  let evs' = Heap.insert evi (events ss)
                  State.put (ss { events = evs' })
             else
               return ())
         (transitions ev)
{% endhighlight %}

Damit ist die Funktion `simulation` fertig: Wir müssen jetzt nur noch
die Simulation initialisieren, `simulation` aufrufen, die entstehende
Berechnung laufenlassen und den resultierenden Report-Generator
liefern.  Das geht so:

{% highlight haskell %}
runSimulation :: ReportGenerator r v => Simulation r v Random () -> Model v Random -> Time -> r -> r
runSimulation sim model clock rg =
  let clock = Clock 0
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Heap.singleton initialEvent
      ma = State.execStateT sim 
                            (SimulationState {
                               clock = clock,
                               events = eventList,
                               reportGenerator = rg
                             })
      ss = Random.evalRand (State.evalStateT ma Map.empty) (mkStdGen 0)
  in reportGenerator ss
{% endhighlight %}

Die Initialisierung entspricht im wesentlichen der Java-Version:
Anfangszeit, erstes Ereignis aus dem Modell sowie Ereignis-Liste mit
dem Anfangs-Ereignis.  Um die Simulation laufen zu lassen, müssen wir
die drei geschachtelten Monaden ausführen - zweimal mit
[`State.evalStateT`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Strict.html#v:evalStateT)
für die beiden Zustandsmonaden und einmal
[`Random.evalRand`](https://hackage.haskell.org/package/MonadRandom-0.1.3/docs/Control-Monad-Random.html#v:evalRand).

## So weit, so gut ... ##

Es fehlen noch ein paar Bausteine (insbesondere der Report-Generator),
aber die wichtigsten Teile sind jetzt nach Haskell übersetzt.

Die Haupterkenntnisse sind:

- Wenn man zustandsbehaftete Berechnungen in Monaden ausführen möchte,
  muss man sich vorher gut überlegen, was alles in die Monade gehört.
  Dann kann man sich die Monade mit Monadentransformatoren
  zusammenbauen.
  
- Die Operationen aus der einfachen, inneren Monade einer größeren,
  äußeren Monade müssen zur Benutzung geliftet werden.
  
Letzteres geht oft auch automatisch, aber in unserem Fall sind zwei
Zustands-Monaden geschachtelt, was es dem Haskell-Compiler schwer
macht zu sehen, welche gemeint ist.  

In Java ist das deutlich einfacher, aber dafür können dort alle
Methoden unkontrolliert Effekte auslösen.   Der "Java-Ansatz" ist auch
Haskell möglich - alle gängingen monadischen Berechnungen können auch
in die [`IO`-Monade](https://www.haskell.org/tutorial/io.html)
geliftet werden, und das Gefrickel mit den Monadentransformatoren ist
dann unnötig.  

Warum das aber vielleicht keine gute Idee ist, heben wir uns für den
dritten Teil auf.

<!-- more end -->
