---
layout: post
description: Ereignisorientierte Simulation mit funktionaler Programmierung
title: "Ereignisorientierte Simulation mit funktionaler Programmierung"
author: michael-sperber
tags: ["Haskell", "Java"]
---

Wie sieht der Umstieg von klassischer objektorientierter
Programmierung in die (rein) funktionale Programmierung konkret aus?
Diese Artikel ist der erste einer Serie, in der wir ein kleines, aber
realistisches Java-Projekt in Haskell übersetzen und die dabei
auftretenden architektonischen Unterschiede beleuchten.


<!-- more start -->

Wir unterhalten uns schon seit einigen Zeit mit den Mitarbeitern des
[Lehrstuhls "Modellbildung und Simulation"](https://www.unibw.de/iis/forschung/rose)
an der Universität der Bundeswehr München um Prof. Oliver Rose über
funktionale Programmierung.  Die Software, die dort geschrieben wird,
ist meist klassischer OO-Java-Code.  Wie, so fragten mich Oliver Rose
und seine Kollegen, würde denn so ein typisches Java-Projekt aussehen,
wenn man es stattdessen in Haskell schriebe.  Kurze Zeit später
lieferte mir der Lehrstuhl ein beispielhaftes und aufgeräumtes
Java-Projekt, das
[ereignisorientierte Simulation](https://de.wikipedia.org/wiki/Ereignisorientierte_Simulation)
implementiert.  (Ab jetzt unter der Abkürzung "DES" für
[discrete event simulation](https://en.wikipedia.org/wiki/Discrete_event_simulation)
geführt.)  Diese Artikelserie beschreibt, wie ich den Java-Code nach
Haskell übersetzt habe und beleuchtet die dabei auftretenden Fragen
zur Software-Architektur.

Der ganze lauffähige Code ist
[auf Github](https://github.com/active-group/tiny-des) zu finden.

## Ereignisorientierte Simulation ##

Ereignisorientierte Simulation ist eine Technik für die Modellierung
in Simulationssystemen, bei denen es um diskrete Ereignisse geht -
also nicht um kontinuierliche Prozesse wie Materialfluss, Sonnenschein
o.ä.  Die Idee ist, dass im System *Ereignisse* ("events") auftreten,
die den *Zustand* des Systems verändern und weitere Ereignisse
auslösen, die dann in der Folge abgearbeitet werden - bis keine
Ereignisse mehr da sind.

## Simulationsmodelle ##

Der Java-DES fängt zunächst mit einer allgemeinen Repräsentation von
Simulationsmodellen an, bevor es um deren Ausführung geht.
Entsprechend wird ein *Metamodell* gebaut - also ein Java-Modell für
Simulationsmodelle.  Das schauen wir uns erst einmal in Gänze an,
bevor wir diesen Teil des Systems dann nach Haskell übersetzen.

Wir fangen ganz oben an - mit einem Interface für Simulationsmodelle:

{% highlight java %}
public interface Model {
    public String getModelName();
    public Event getStartEvent();
}
{% endhighlight %}

Die Methode `getModelName` liefert einen Namen und `getStartEvent` das
erste Ereignis des Modells, von dem aus es dann in Bewegung gesetzt wird.

Ein Ereignis ist durch ein Objekt der Klasse `Event` repräsentiert.
Der Code dafür fängt so an:

{% highlight java %}
public class Event {
    private final String name;
    private int priority;
    private List<Transition> transitions;
    private List<StateChange> stateChanges;

    public Event(String name) {
        this.name = name;
        this.priority = 0;
        this.transitions = new ArrayList<Transition>();
        this.stateChanges = new ArrayList<StateChange>();
    }
{% endhighlight %}

Ein Ereignis hat also einen informativen Namen und eine Priorität, die
später bestimmen wird, in welcher Reihenfolge die Ereignisse
abgearbeitet werden.

Außerdem hängt an jedem Ereignis eine Liste von "transitions", das
sind mögliche neue Ereignisse, die von diesem Ereignis ausgelöst
werden.  Die Liste `stateChanges` enthält Objekte, die Veränderungen
am Zustand des Modells beschreiben.

Der Rest der Klasse sind Standard-Getter-, Setter- und
Update-Funktionen für diese Felder:

{% highlight java %}
    public String getName() {
        return name;
    }

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public List<Transition> getTransistions() {
        return transitions;
    }

    public void addTransistion(Transition transition) {
        this.transitions.add(transition);
    }

    public List<StateChange> getStateChanges() {
        return stateChanges;
    }

    public void addStateChange(StateChange stateChange) {
        this.stateChanges.add(stateChange);
    }
}
{% endhighlight %}

Machen wir mit den erwähnten "transitions" weiter, der Code für die
Klasse `Transition` fängt folgendermaßen an:

{% highlight java %}
public class Transition {
    private final Event targetEvent;
    private Condition condition;
    private Delay delay;

    public Transition(Event targetEvent) {
        this.targetEvent = targetEvent;
        this.condition = new TrueCondition();
        this.delay = new ZeroDelay();
    }
{% endhighlight %}

So eine Transition sagt also aus, dass das Ereignis `targetEvent`
generiert werden soll, aber nur unter einer bestimmten Bedingung und
u.U. nach einer Verzögerung.  Die Standardwerte für `condition` und
`delay` stehen dafür, dass das Ereignis immer und sofort ausgelöst
wird.  (`Condition` und `Delay` sowie die konkreten Implementierungen
für `TrueCondition` und `ZeroDelay` werden später erläutert.)

Auch in der `Transition`-Klasse gibt es wieder Getter- und
Setter-Methoden für alles:

{% highlight java %}
    public Event getTargetEvent() {
        return targetEvent;
    }

    public Condition getCondition() {
        return condition;
    }

    public void setCondition(Condition condition) {
        this.condition = condition;
    }

    public Delay getDelay() {
        return delay;
    }

    public void setDelay(Delay delay) {
        this.delay = delay;
    }
}
{% endhighlight %}

Neben `Transition` war in der `Event`-Klasse auch noch `StateChange`
erwähnt.  Objekte dieses Interfaces beschreiben die Auswirkung eines
Ereignisses auf das Modell.  Das Interface enthält eine einzige
Methode, die die entsprechende Änderung am Modellzustand vornimmt:

{% highlight java %}
public interface StateChange {
    public void changeState(ModelState modelState);

}
{% endhighlight %}

`ModelState` fehlt auch noch - der Modellzustand ist als Sammlung von
Zustandsvariablen modelliert, von denen jede einen Namen hat:

{% highlight java %}
public class ModelState {
    private Map<String, Object> states;
    public ModelState() {
        this.states = new LinkedHashMap<String, Object>();
    }
    
    public Map<String, Object> getStates() {
        return states;
    }
}
{% endhighlight %}

Bleiben noch die Interfaces `Delay` und `Condition`:

{% highlight java %}
public interface Delay {
    public Long getDelay();
}

public interface Condition {
    public boolean isTrue(ModelState modelState);
}
{% endhighlight %}

Beide Interfaces sehen auf den ersten Blick so aus, als ob sie im
wesentlichen reine Funktionen beschreiben.  Das schon benutzte
`ZeroDelay` sieht so aus:

{% highlight java %}
public final class ZeroDelay implements Delay {
    public Long getDelay() {
        return 0L;
    }
}
{% endhighlight %}

Außerdem gibt es eine Klasse `ConstantDelay` für eine feste Verzögerung:

{% highlight java %}
public class ConstantDelay implements Delay {
    private final Long value;
    public ConstantDelay(Long value) {
        this.value = value;
    }
    public Long getDelay() {
        return this.value;
    }
}
{% endhighlight %}

Es riecht schon ein bißchen nach funktionaler Programmierung!
Allerdings gibt es auch eine Klasse für zufällige, exponenziell
verteilte Verzögerungen:

{% highlight java %}
public class ExponentialDelay implements Delay {
    private final double mean;
    private final Random random;
    public ExponentialDelay(double mean, Random random) {
        this.mean = mean;
        this.random = random;
    }
    public Long getDelay() {
        Long result = 0L;
        double u = random.nextDouble();
        double x = -mean * Math.log(u);
        result = Math.round(x);
        return result;
    }
}
{% endhighlight %}

Hier ist also impliziter Zustand über den Zufallszahlengenerator
`random` im Spiel.

Bei den Implementierungen von `Condition` geht alles recht gesittet zu.
Zunächst `TrueCondition`:

{% highlight java %}
public final class TrueCondition implements Condition {
    public boolean isTrue(ModelState modelState) {
        return true;
    }
}
{% endhighlight %}

Außerdem gibt es noch `LargerThanValueCondition`, das überprüft, ob
eine Zustandsvariable größer als ein bestimmter fester Wert ist:

{% highlight java %}
public class LargerThanValueCondition implements Condition {
    private String name;
    private Long value;
    public LargerThanValueCondition(String name, Long value) {
        this.name = name;
        this.value = value;
    }
    public boolean isTrue(ModelState modelState) {
        return (Long) modelState.getStates().get(this.name) > this.value;
    }
}
{% endhighlight %}

Bei `Condition` scheint es sich also tatsächlich um reine Funktionen
zu handeln.

Schließlich hat der DES-Code auch noch zwei Implementierungen von
`StateChange` anzubieten.  Die erste setzt einfach eine bestimmte
Zustandsvariable auf einen festen Wert:

{% highlight java %}
public class SetValueStateChange implements StateChange {
    private final String name;
    private final Long value;
    public SetValueStateChange(String name, Long value) {
        this.name = name;
        this.value = value;
    }
    public void changeState(ModelState modelState) {
        modelState.getStates().put(this.name, this.value);
    }
}
{% endhighlight %}

Die zweite `StateChange`-Implementierung inkrementiert eine
Zustandsvariable:

{% highlight java %}
public class IncrementValueStateChange implements StateChange {
	private String name;
	private long increment;
	public IncrementValueStateChange(String name) {
		this.name = name;
		this.increment = 1;
	}
	public IncrementValueStateChange(String name, long increment) {
		this.name = name;
		this.increment = increment;
	}
	public void changeState(ModelState modelState) {
		modelState.getStates().put(name, ((Long) modelState.getStates().get(name) + increment));
	}
}
{% endhighlight %}

So, das muss erst einmal reichen, um mit der Übersetzung nach Haskell
anzufangen.

## Von Java nach Haskell

Wir versuchen einfach mal, den Java-Code mehr oder minder zeilenweise
zu übersetzen, ohne uns großartig Gedanken über die größere
Softwarearchitektur zu machen.  Es geht also mit `Model` los.  Das
Java-Interface liefert nur zwei Werte (Name und Start-Ereignis), die
mit einer Record-Definition übersetzt werden kann:

{% highlight haskell %}
data Model v = Model {
  modelName :: String,
  startEvent :: Event v
  }
{% endhighlight %}

Als nächstes ist `Event` dran, das ebenfalls durch eine
Record-Definition übersetzt werden kann:

{% highlight haskell %}
data Event v = Event { name :: String,
                       priority :: Int,
                       transitions :: [Transition v],
                       stateChanges :: [StateChange v] }
{% endhighlight %}

Hier wird gleich der erste kleine Unterschied klar: Ein `Event`-Wert
lässt sich in Haskell erstmal nur durch Angabe von Werten für *alle*
Felder konstruieren.  Die Java-Version hatte Standardwerte für alle
Felder außer `name`.  In Haskell können wir das aber auch einfach
simulieren, indem wir ein "Standard-Event" anlegen:

{% highlight haskell %}
event = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }
{% endhighlight %}

Wir können dann z.B. das hier schreiben, um ein Ereignis mit
spezifizierten Namen und Übergängen zu erzeugen, bei dem `priority`
und `stateChanges` Standardwerte bekommen:

{% highlight haskell %}
event { name = ..., transitions = ... }
{% endhighlight %}

(Die Getter sind schon als Teil der `data`-Definition definiert,
Setter gibt es nicht in Haskell.)

Weiter geht es mit Transition - auch hier verwenden wir einen Record-Typ,
welcher der Java-Klasse direkt entspricht:

{% highlight haskell %}
data Transition v = Transition { targetEvent :: Event v,
                                 condition :: Condition v,
                                 delay :: Delay v }
{% endhighlight %}

Weiter geht's mit `StateChange`.  Das ist jetzt etwas schwieriger, da
es, wie der Name schon sagt, um eine Zustandsänderung geht.  In
Haskell nehmen wir, wenn es um Manipulation von Zustand geht, in der
Regel eine <a href="{% post_url 2013-04-18-haskell-monaden
%}">Monade</a>, in diesem Fall eine Zustandsmonade.  Dazu importieren
wir erstmal das Modul
[`Control.Monad.State.Strict`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Strict.html):

{% highlight haskell %}
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
{% endhighlight %}

Nun soll ja `StateChange` den Modellzustand manipulieren.  Dazu
brauchen wir erstmal eine Typdefinition für diesen Modellzustand.  In
Java ist das eine Klasse, die eine Map von `String` nach `Object`
kapselt.  `Object` geht in Haskell gar nicht - wir verschieben das
Problem einfach, indem wir statt `Object` einen Typparameter einführen:

{% highlight haskell %}
type ModelState v = Map String v
{% endhighlight %}

Für den Typ von Maps importieren wir
[`Data.Map.Strict`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Strict.html):

{% highlight haskell %}
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
{% endhighlight %}

Als nächstes definieren wir einen Typ für zustandsbehaftete
Berechnungen, die auf dem `ModelState` operieren:

{% highlight haskell %}
type ModelAction v = State.State (ModelState v)
{% endhighlight %}

Damit können wir jetzt `StateChange` definieren als Berechnung, die
auf dem `ModelState` operiert und kein Ergebnis liefert:

{% highlight haskell %}
type StateChange v = ModelAction v ()
{% endhighlight %}

... und weiter im Java-Code.  Als nächstes ist `Delay` dran.  Zur
Erinnerung: `Delay` braucht Zufallszahlen.  Damit das funktioniert,
nehmen wir die Zufallszahlenmonade in
[`Control.Monad.Random`](https://hackage.haskell.org/package/MonadRandom-0.4.2.2/docs/Control-Monad-Random.html):

{% highlight haskell %}
import qualified Control.Monad.Random as Random
{% endhighlight %}

Bei der `Random.Rand`-Monade muss immer explizit ein
Zufallszahlengenerator angegeben werden.  Wir nehmen einfach den
Standard-Generator und definieren dafür eine Abkürzung:

{% highlight haskell %}
type Random = Random.Rand Random.StdGen
{% endhighlight %}

Mit deren Hilfe können wir nun den Typ für `Delay` definieren.  Zur
Erinnerung: Ein `Delay` muss einen Integer-Wert liefern, die
Definition sieht also so aus:

{% highlight haskell %}
type Delay = Random Integer
{% endhighlight %}

Damit können wir jetzt Pendants zu `ZeroDelay`, `ConstantDelay` und
`ExponentialDelay` definieren:

{% highlight haskell %}
zeroDelay :: Delay
zeroDelay = return 0
constantDelay :: Integer -> Delay
constantDelay v = return v

exponentialDelay :: Double -> Delay
exponentialDelay mean =
  do u <- Random.getRandom
     return (round (-mean * log u))
{% endhighlight %}

Als nächstes sind Conditions dran: Das sind ja in Java Interfaces mit
nur einer Methode, die den Modellzustand als Argument akzeptiert.  In
Haskell machen wir das natürlich als Funktion:

{% highlight haskell %}
type Condition v = ModelState v -> Bool

trueCondition :: Condition v
trueCondition = \ _ -> True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value ms =
  let (Just value') = Map.lookup name ms
  in  value' > value
{% endhighlight %}

Bleiben noch die zwei Implementierungen von `StateChange`.  Dazu
definieren wir entsprechende Funktionen, die Berechnungen in der
`ModelAction`-Monade liefern:

{% highlight haskell %}
setValue :: String -> v -> StateChange v
setValue name value =
  do ms <- State.get
     State.put (Map.insert name value ms)

incrementValue :: Num v => String -> v -> StateChange v
incrementValue name inc =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     setValue name (v + inc)
{% endhighlight %}

So, das wäre erst mal die naive Übersetzung des Java-Codes für
Simulationsmodelle.  Wir konstatieren:

- Das meiste können wir direkt übersetzen und es wird in Haskell kürzer.
- Wir müssen die Java-Methoden identifizieren, die Zustand
  manipulieren.
- Die werden dann in der Regel in Haskell zu monadischen Funktionen.

Die Haskell-Experten werden bemerkt haben, dass da noch was nicht
stimmt.  Aber dazu mehr im zweiten Teil, demnächst auf diesem Blog.

<!-- more end -->


