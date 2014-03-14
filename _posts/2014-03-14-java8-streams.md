---
layout: post
description: "Streams in Java 8"
title: "Streams in Java 8"
author: michael-sperber
tags: ["Java"]
---

In einem <a href="{% post_url 2013-09-19-java8 %}">früheren Posting</a>
hatten wir bereits die wichtigste Neuerung in Java 8,
nämlich die Lambda-Ausdrücke vorgestellt.  Inzwischen steht das
offizielle Release von Java 8 [unmittelbar
bevor](https://blogs.oracle.com/theaquarium/entry/java_8_launch)
und es gibt einen [Release
Candidate](https://jdk8.java.net/download.html).  Heute beleuchten wir
die Hauptmotivation für die Einführung von Lambda-Ausdrücken, die
neuen *Streams* in Java 8.  Die lassen tatsächlich etwas funktionales
Programmiergefühl in Java aufkommen.  Allerdings muss man einige
Feinheiten beachten.

<!-- more start -->

Eine der besonderen Stärken der funktionalen Programmierung ist der
Umgang mit "Collections", weil viele Operationen auf Listen, Mengen
oder Maps als Higher-Order-Funktion ausgedrückt werden können.  Im
"alten Java" waren dafür Schleifen nötig.  Das ist nicht nur umständlich,
sondern sequenzialisiert den Programmablauf unnötig, was bei modernen
Multicore-Rechnern häufig Potenzial zum
[Datenparallelismus](http://en.wikipedia.org/wiki/Data_parallelism)
verschwendet.

Das alles lässt sich am einfachsten an einem konkreten Beispiel
demonstrieren: Wir analysieren die Spiele einer Fußballsaison.  Dazu
erzeugen wir erst einmal eine Klasse für die wichtigsten Daten eines
Fußballspiels:

{% highlight java %}
public class Game {
    public int matchDay; // Spieltag
    public String homeTeam; // Name des Gastgebers
    public int homeGoals; // Anzahl Tore des Gastgebers
    public String guestTeam; // Name des Gasts
    public int guestGoals; // Anzahl Tore des Gasts

    public Game(int matchDay, String homeTeam, int homeGoals, String guestTeam, int guestGoals) {
        this.matchDay = matchDay;
        this.homeTeam = homeTeam;
        this.homeGoals = homeGoals;
        this.guestTeam = guestTeam;
        this.guestGoals = guestGoals;
    }
    ...
}
{% endhighlight %}

Leider erspart uns Java 8 weder das explizite Hinschreiben des
Konstruktors, noch das von `equals`- und `hashCode`-Methode - diese
stellen wir uns hier einfach vor.  Auf Getter-Methoden verzichten wir
einfach.  Dies reicht, um eine komplette Fußball-Saison zu
repräsentieren:

{% highlight java %}
Game[] season_2009_2010a = {
        new Game(1, "Wolfsburg", 2, "Stuttgart", 0),
        new Game(1, "Mainz", 2, "Bayer 04" 2),
        ...
};
List<Game> season_2009_2010 = Arrays.asList(season_2009_2010a);
{% endhighlight %}

Als erstes fällt auf, dass auch in Java 8 die Collections nicht 
<a href="{% post_url 2013-03-20-warum-funktional %}">persistent</a>
sind.
Vorsicht also bei der letzten Zeile: Das Array `season_2009_2010a` hält
die Elemente der Liste `season_2009_2010`.  Wenn das Array
nachträglich verändert wird, verändert sich auch die Liste.  Wer das
Problem vermeiden möchte, sollte lieber folgendes schreiben:

{% highlight java %}
List<Game> season_2009_2010 = new ArrayList<>(Arrays.asList(season_2009_2010a));
{% endhighlight %}

Wir benötigen einige Methoden, um wenigstens halbwegs interessante
Sachen zu machen:

{% highlight java %}
    // Punkte des Gastgebers
    public int homePoints() {
        int g1 = this.homeGoals;
        int g2 = this.guestGoals;
        if (g1 > g2)
            return 3;
        else if (g1 < g2)
            return 0;
        else // if (g1 == g2)
            return 1;
    }

    // Punkte des Gasts
    public int guestPoints() {
        int g1 = this.guestGoals;
        int g2 = this.homeGoals;
        if (g1 > g2)
            return 3;
        else if (g1 < g2)
            return 0;
        else // if (g1 == g2)
            return 1;
    }
    
    // Spielt ein bestimmtes Team in diesem Spiel?
    public boolean playsGame(String t) {
        return this.homeTeam.equals(t) || this.guestTeam.equals(t);
    }

    // Wieviele Punkte hat ein bestimmtes Team in diesem Spiel bekommen?
    public int teamPoints(String t) {
        if (t.equals(this.homeTeam))
            return this.homePoints();
        else if (t.equals(this.guestTeam))
            return this.guestPoints();
        else
            throw new Error("not a known team");
    }
{% endhighlight %}

(Ich weiß, seit [gotofail](https://gotofail.com/) macht man `{...}`
nach den `if`s.  Tschuldigung.)

Es ist eine lohnende Fingerübung zu versuchen, die frustrierenden
Gemeinsamkeiten von `homePoints` und `guestPoints` durch Abstraktion
zusammenzufassen, vielleicht sogar unter Verwendung von Lambda-Ausdrücken.

Die Funktion `playsGame` können wir zum Beispiel benutzen, um alle
Spiele von Nürnberg aus der Saison herauszufiltern.  Jede funktionale
Programmiersprache hält hierfür eine Funktion namens
[`filter`](http://en.wikipedia.org/wiki/Filter_%28higher-order_function%29)
vor, die eine Collection und ein Prädikat akzeptiert.  Das Prädikat
liefert für jedes Collection-Element einen booleschen Wert, und
`filter` liefert eine neue Collection mit allen Elementen zurück, bei
denen das Prädikat `true` liefert.  In Java 8 geht das jetzt endlich
auch, allerdings nicht auf den bekannten Collection-Interfaces wie
[`List`](http://download.java.net/jdk8/docs/api/java/util/List.html),
sondern auf einer neuen Klasse namens
[`Stream`](http://download.java.net/jdk8/docs/api/java/util/stream/Stream.html),
und die hat eine
[`filter`-Funktion](http://download.java.net/jdk8/docs/api/java/util/stream/Stream.html#filter-java.util.function.Predicate-).
Glücklicherweise hat `List` eine Methode
[`stream()`](http://download.java.net/jdk8/docs/api/java/util/Collection.html#stream--),
die aus der Liste einen Stream macht.  Wir können dann `filter`
benutzen, um z.B. alle Spiele mit Nürnberg herauszusortieren:

{% highlight java %}
season_2009_2010.stream().filter(g -> g.playsGame("Nürnberg"))
{% endhighlight %}

Allerdings liefert `filter` wiederum einen Stream.  Um aus dem Stream
wieder herauszukommen, müssen wir einen
[`Collector`](http://download.java.net/jdk8/docs/api/java/util/stream/Collector.html)
bemühen, mit dessen Hilfe die
[`collect`-Methode](http://download.java.net/jdk8/docs/api/java/util/stream/Stream.html#collect-java.util.stream.Collector-)
die die Elemente des Streams aufsammelt.  Eine Menge
vorgefertigter Collector-Funktionen gibt es in der
[`Collectors`-Klasse](http://download.java.net/jdk8/docs/api/java/util/stream/Collectors.html),
insbesondere eine zum Aufsammeln in eine Liste:

{% highlight java %}
List<Game> l1 = season_2009_2010.stream()
                  .filter(g -> g.playsGame("Nürnberg"))
                  .collect(Collectors.toList())
{% endhighlight %}

Auch [`map`](http://en.wikipedia.org/wiki/Map_%28higher-order_function%29) [gibt es](http://download.java.net/jdk8/docs/api/java/util/stream/Stream.html#map-java.util.function.Function-):

{% highlight java %}
List<Integer> hgs = season_2009_2010.stream()
        .filter(g -> g.playsGame("Hamburg"))
        .map(g -> g.teamPoints("Hamburg"))
        .collect(Collectors.toList());
{% endhighlight %}

Wenn wir jetzt auch noch die Punkte aufsummieren wollen, dann können
wir noch
[`summingInt`](http://download.java.net/jdk8/docs/api/java/util/stream/Collectors.html#summingInt-java.util.function.ToIntFunction-)
bemühen, die ein "Map" mit einer Summenbildung kombiniert:

{% highlight java %}
int hg = season_2009_2010.stream()
        .filter(g -> g.playsGame("Hamburg"))
        .map(g -> g.teamPoints("Hamburg"))
        .collect(Collectors.summingInt(i -> i));
{% endhighlight %}

Aus irgendeinem Grund gibt es keinen Collector, der direkt die Summe
der Stream-Elemente ausrechnet.  Dies lässt sich natürlich optimieren
zu:

{% highlight java %}
int hg = season_2009_2010.stream()
        .filter(g -> g.playsGame("Hamburg"))
        .collect(Collectors.summingInt(g -> g.teamPoints("Hamburg")));
{% endhighlight %}

Das tolle an Operationen wie `map` und `filter` ist, dass sie die
Elemente der Collection unabhängig voneinander verarbeiten.  Es ist
also möglich, jeweils Teile der Liste in verschiedenen Threads zu
verarbeiten.  Dazu ist nur eine klitzekleine Änderung nötig, nämlich
`parallelStream()` statt `stream()`:

{% highlight java %}
int hg = season_2009_2010.parallelStream()
        .filter(g -> g.playsGame("Hamburg"))
        .collect(Collectors.summingInt(g -> g.teamPoints("Hamburg")));
{% endhighlight %}

Die `filter`-Methode bemüht sich dann, das Filtern parallel
durchzuführen.  Genauso ist es bei `map`.  Diese Art der parallelen
Programmierung ist deutlich angenehmer und weniger fehleranfällig als
das Programmieren mit Locks oder sogar der [Fork/Join-Parallelismus
aus Java
7](http://docs.oracle.com/javase/tutorial/essential/concurrency/forkjoin.html).

(Ein nicht-paralleler Stream kann auch mit der `parallel()`-Methode
nachträglich zu einem parallelen gemacht werden.)

Selbst `collect` kann parallel laufen, da die Collectoren
[assoziative](http://de.wikipedia.org/wiki/Assoziativgesetz)
Operationen implementieren müssen.  Bonuspunkte gibt es für
[kommutative](http://de.wikipedia.org/wiki/Kommutativgesetz)
Operationen.  (Die gute alte Schulmathematik hat also tatsächlich
praktische Anwendungen!) Mehr Informationen gibt es in der
[Dokumentation von
`Collector`](http://download.java.net/jdk8/docs/api/java/util/stream/Collector.html).

Insgesamt also eine tolle Sache, diese Streams.

Einige Aspekte der Streams sind trotzdem zu beachten, vor allem für
funktionale Programmierer.  So sind Streams *nicht* persistent.  Das
hier geht also nicht:

{% highlight java %}
Stream<Game> season_2009_2010_stream = season_2009_2010.stream();
List<Game> l3 = season_2009_2010_stream
        .filter(g -> g.playsGame("Nürnberg"))
        .collect(Collectors.toList());
List<Game> l4 = season_2009_2010_stream
        .filter(g -> g.playsGame("Nürnberg"))
        .collect(Collectors.toList());
{% endhighlight %}

Das compiliert zwar noch, liefert aber folgenden Fehler:

    Exception in thread "main" java.lang.IllegalStateException: stream has already been operated upon or closed
        at java.util.stream.AbstractPipeline.<init>(AbstractPipeline.java:203)
        at java.util.stream.ReferencePipeline.<init>(ReferencePipeline.java:94)
        at java.util.stream.ReferencePipeline$StatelessOp.<init>(ReferencePipeline.java:618)
        at java.util.stream.ReferencePipeline$2.<init>(ReferencePipeline.java:163)
        at java.util.stream.ReferencePipeline.filter(ReferencePipeline.java:162)
        at Game.main(Game.java:202)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
        at java.lang.reflect.Method.invoke(Method.java:483)
        at com.intellij.rt.execution.application.AppMain.main(AppMain.java:120)

Außerdem gibt es separate Stream-Klassen für einige primitiven Typen:
[`IntStream`](http://download.java.net/jdk8/docs/api/java/util/stream/IntStream.html),
[`LongStream`](http://download.java.net/jdk8/docs/api/java/util/stream/LongStream.html)
und
[`DoubleStream`](http://download.java.net/jdk8/docs/api/java/util/stream/DoubleStream.html)
(allerdings kein `BooleanStream`) und einen ganzen Zoo spezialisierter
Methoden wie
[`mapToInt`](http://download.java.net/jdk8/docs/api/java/util/stream/Stream.html#mapToInt-java.util.function.ToIntFunction-).
Da haben es Scala-Programmierer doch deutlich leichter.

<!-- more end -->

