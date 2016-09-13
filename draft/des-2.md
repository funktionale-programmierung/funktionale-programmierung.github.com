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
finden, beide zu kombinieren.  Außerdem müssen wir ja auch noch die
Uhrzeit, die Ereignis-Liste und den Report-Generator verwalten.

Glücklicherweise sind all diese drei Monaden Zustandsmonaden, deren
Zustände wir in einem einzelnen *Simulationszustand* zusammenfassen
können.  Den Typ für den Report-Generator lassen wir erstmal offen (im
Java-Code steht da ein Interface) und schreiben eine Typvariable dazu:

{% highlight haskell %}
data SimulationState r v = SimulationState {
  clock :: Clock,
  events :: MinHeap (EventInstance v),
  reportGenerator :: r,
  modelState :: ModelState v,
  randomGenerator :: Random.StdGen
}
{% endhighlight %}

([`Random.StdGen`](http://hackage.haskell.org/package/random-1.0.0.2/docs/System-Random.html#t:StdGen)
ist der Typ des *Zustands* des Standard-Zufallszahlengenerators.)

Die Simulationsmonade ist also einfach eine Zustandsmonade über dem
"großen" Zustand:

{% highlight haskell %}
type Simulation r v = State.State (SimulationState r v)
{% endhighlight %}

Bei der Hauptfunktion kümmern wir uns erstmal um die innere Schleife.
(Die Initialisierung kommt dann als Teil der
Simulations-Hauptfunktion, weil diese Reihenfolge leichter zu
erläutern ist.)  Sie orientiert sich stark an der Java-Version: Sie
akzeptiert eine Endzeit und liefert dann eine Berechung in der Simulations-Monade:

{% highlight haskell %}
simulation :: ReportGenerator r v => Time -> Simulation r v ()
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

Um überhaupt zu überprüfen, ob die Iteration am Ende angelangt ist,
muss `loop` die entsprechenden Werte aus dem Simulationszustand holen - mit
`State.get`.  Sie ruft dann `timingRoutine` auf.  Der Inhalt der
Java-Methode `eventRoutine` ist dann auf ihre drei Aufgaben verteilt:

- `updateModelState` aktualisiert den Modell-Zustand
- `updateStatisticalCounters` aktualisiert den Report-Generator
  (vertagen wir)
- `generateEvents` generiert neue Events und stellt diese in die Event-Liste.

Die Funktion `timingRoutine` ist das Pendant zur gleichnamigen
Java-Funktion:

{% highlight haskell %}
timingRoutine :: Simulation r v (EventInstance v)
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
getNextEvent :: Simulation r v (EventInstance v)
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
ab, sondern in der `ModelAction`-Monade.  Wir müssen also den
Modellzustand aus dem Simulationszustand extrahieren, darauf die
`ModelAction` laufen lassen (das macht die Funktion
[`State.execState`](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-State-Strict.html#v:execState)
und den resultierenden Zustand wieder zurück in den Simulationszustand
stecken.

{% highlight haskell %}
updateModelState :: EventInstance v -> Simulation r v ()
updateModelState (EventInstance _ ev) =
  do ss <- State.get
     let ms = State.execState (sequence_ (stateChanges ev)) (modelState ss)
     State.put (ss { modelState = ms })
{% endhighlight %}

Es bleibt `generateEvents`.  Hier müssen wir über die Transitionen
eines Ereignisses iterieren (dazu gibt es die eingebaute
Monaden-Funktion `mapM_`), die Bedingung überprüfen, die Verzögerung
berechnen und dann die entstehende Ereignis-Instanz in die
Ereignis-Liste einfügen.

Diesmal müssen wir daran denken, die Berechnung der Verzögerung in der
Zufallszahlenmonade laufen zu lassen, angefangen mit dem Zustand, der
im Simulationszustand im Feld `randomGenerator` steckt.  Das geht mit [`Random.runRand`](http://hackage.haskell.org/package/MonadRandom-0.1.1/docs/Control-Monad-Random.html#v:runRand).

{% highlight haskell %}
generateEvents :: EventInstance v -> Simulation r v ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ss <- State.get
             let ms = modelState ss
             if condition tr ms then
               do ss <- State.get
                  let (d, rg) = Random.runRand (delay tr) (randomGenerator ss)
                  let evi = EventInstance ((getCurrentTime (clock ss)) + d) (targetEvent tr)
                  let evs' = Heap.insert evi (events ss)
                  State.put (ss { events = evs', randomGenerator = rg })
             else
               return ())
         (transitions ev)
{% endhighlight %}

Damit ist die Funktion `simulation` fertig: Wir müssen jetzt nur noch
die Simulation initialisieren, `simulation` aufrufen, die entstehende
Berechnung laufenlassen und den resultierenden Report-Generator
liefern.  Das geht so:

{% highlight haskell %}
yrunSimulation :: ReportGenerator r v => Simulation r v () -> Model v -> Time -> r -> r
runSimulation sim model clock rg =
  let clock = Clock 0
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Heap.singleton initialEvent
      ss = SimulationState {
             clock = clock,
             events = eventList,
             reportGenerator = rg,
             modelState = Map.empty,
             randomGenerator = mkStdGen 0
           }
      ss' = State.execState sim ss
  in reportGenerator ss'
{% endhighlight %}

Die Initialisierung entspricht im wesentlichen der Java-Version:
Anfangszeit, erstes Ereignis aus dem Modell sowie Ereignis-Liste mit
dem Anfangs-Ereignis.  Die Funktion baut einen
Anfangs-Simulations-Zustand zusammen, lässt darauf die Simulation
laufen und extrahiert aus dem Endzustand den Report-Generator.

## So weit, so gut ... ##

Es fehlen noch ein paar Bausteine (insbesondere der Report-Generator),
aber die wichtigsten Teile sind jetzt nach Haskell übersetzt.  Die
heben wir uns für einen späteren Teil auf.

Für heute konstatieren wir erst einmal, dass die Monaden helfen
einzugrenzen, was einzelne Operationen anstellen können: Während im
Original-Java-Code alle Funktionen uneingeschränkt Zustandsänderungen
bewirken können, ist im Haskell-Code klar, dass Verzögerungen nur auf
den Zufallszahlengenerator zugreifen können und `ModelAction`s nur auf
den Modellzustand.

Allerdings ist die Kombination der drei involvierten Monaden auch
immer Arbeit, die in der Java-Version nicht anfällt.  Hier ging das
noch glimpflich ab, weil es sich um Zustandsmonaden handelt.  In
komplizierteren Fällen sind
[Monaden-Transformatoren](https://wiki.haskell.org/Monad_Transformers)
nötig.  Auch diese Diskussion heben wir uns für ein zukünftiges
Posting auf.

<!-- more end -->
