---
layout: post
description: Eine Java-Performance-Challenge zum Anlass für übersichtlichen und doch performanten Java-Code
title: "Übersichtlicher und Performanter Code mit Scala"
author: Andreas Bernauer
tags: Scala Java Performance Übersichtlichkeit
---

Auf der [OOP](http://www.sigs-datacom.de/konferenzen/oop.html) hat
[dynatrace](http://www.compuware.com/application-performance-management/dynatrace-enterprise.html)
eine "Java Performance Challenge" abgehalten: welcher von zwei
dargestellten Stück Java-Code läuft schneller?

Als ich mir die Stücke Java-Code anschaute, fiel mir wieder auf, wie
langatmig ich Java fand, wie schlecht man hinschreiben kann, was man
meint und wie fehleranfällig der Code dadurch wird.  Und natürlich
habe ich mir überlegt, wie das funktionale Pendant aussehen würde und
ob das Performance-Problem dort auch auftauchen würde.

<!-- more start -->

Zunächst zum ersten Stück Java-Code, das aus einer Applikation stammt,
die zu langsam lief, und zu der dynatrace gerufen wurde, um sie
schneller zu machen:

{% highlight java %}
public class GetRoutingDetailsA {
  public static void main(String[] args) {
    StringBuilder strRouteList = new StringBuilder();
    int totalNodeCount = 0;
    for(int routeNumber = 0; routeNumber < 126; routeNumber++) {
      int nodeCount = 0;
      for(int infoNumber = 0; infoNumber < 125; infoNumber++) {
        String fromLocation = "YYZ";
        String toLocation = ""+infoNumber;
        totalNodeCount++;
        if (nodeCount != 0) {
          if (strRouteList.toString().isEmpty())
            strRouteList.append(fromLocation + toLocation);
          else
            strRouteList.append("|" + fromLocation + toLocation);
          strRouteList.append("\n");
        }
        nodeCount++;

      }
    }
    System.err.print(strRouteList.toString());
    System.out.format("Node Count: %d",totalNodeCount);
  }
}
{% endhighlight %}

Der Code enthält zwei verschachtelte Schleifen, welche mittels eines
`StringBuilder` eine Route erzeugen und eine Anzahl von Knoten
berechnen.

Das zweite Stück Java-Code ist die schnellere Version und sieht wie
folgt aus:

{% highlight java %}
public class GetRoutingDetailsB {
  public static void main(String[] args) {
    StringBuilder strRouteList = new StringBuilder();
    int totalNodeCount = 0;
    for(int routeNumber = 0; routeNumber < 126; routeNumber++) {
      int nodeCount = 0;
      for(int infoNumber = 0; infoNumber < 125; infoNumber++) {
        String fromLocation = "YYZ";
        String toLocation = ""+infoNumber;
        if (nodeCount != 0) {
          if (totalNodeCount == 1)
            strRouteList.append(fromLocation + toLocation);
          else if (totalNodeCount > 1)
            strRouteList.append("|" + fromLocation + toLocation);
          strRouteList.append("\n");
        }
        nodeCount++;
        totalNodeCount++;
      }
    }
    System.err.print(strRouteList.toString());
    System.out.format("Node Count: %d", totalNodeCount);
  }
}
{% endhighlight %}

Der Unterschied zum ersten Stück Java-Code ist auf den ersten Blick
kaum auszumachen: statt `strRouteList.toString().isEmpty()` prüft das
zweite Stück Java-Code die Variable `totalNodeCount == 1`. Damit
vermeidet es, dass ständig ein (potentieller langer) String erst
erzeugt wird und dann nur geprüft wird, ob er überhaupt etwas enthält.
Das erste Stück Java-Code läuft daher etwa 100 Mal langsamer als das
das zweite Stück Java-Code.

Wie sieht das funktional Pendant aus? Da das Original in Java ist,
bietet sich Scala an:

{% highlight scala %}
object GetRoutingDetailsScala1 extends App {
  val subRoutes = (0 until 126).map({routeNumber =>
    val nodes = (1 until 125).map({infoNumber => "YYZ" + infoNumber})
    (nodes.size + 1, nodes.mkString("\n|"))
   })
  val route = subRoutes.map(x => x._2).mkString("\n|") ++ "\n";
  val totalNodeCount = subRoutes.map(x => x._1).sum

  System.err.print(route);
  System.out.print("Node Count: %d".format(totalNodeCount));
}
{% endhighlight %}

Wie zu erwarten, fällt der Scala-Code viel kürzer aus.  (Java war ja
nie berühmt dafür, dass man darin besonders knapp programmieren
kann...)

Außerdem fällt auf, dass es keine `for`-Schleifen gibt. Stattdessen
bevorzuge ich Operatoren wie zum Beispiel `map`: das erste
`map({routeNumber => ...})` liefert die Subrouten, das zweite
`map({infoNumber => ...})` liefert die Länge einer Subroute und die
Subroute selbst. `subRoutes` ist damit eine Liste von Paaren, dessen
erstes Element die Länge der Subroute und dessen zweites Element die
Subroute selbst ist.

Um die Ergebnisroute `route` zu berechnen, bilde ich mit `map` die
`subRoutes` auf ihr zweites Element ab (`x._2`) und verbinde sie zu
einem einzigen String mit `mkString`, das die Subrouten mit dem String
`"\n|"` verbindet.  Die Anzahl der Knoten `totalNodeCount` ist die
Summe der ersten Paar-Elemente.

Was ist mit dem Peformance-Problem in der Scala-Version? Die Stelle,
welche im Java-Programm das Performance-Problem erzeugte, taucht im
Scala-Programm nicht auf: das Aneinanderhängen der Routenelemente wird
von `mkString("\n|")` erledigt.

<!-- 
10 repetitions:
Durations:
 A:   2754998000 ns (=  61 x B)
 B:     45339000 ns (=   1 x B)
C2:    182031000 ns (=   4 x B)
C3:    120153000 ns (=   3 x B)
C1:     53343000 ns (=   1 x B)
Checking consistency
B.route == C2.route                  : true
B.totalNodeCount == C2.totalNodeCount: true
B.route == C3.route                  : true
B.totalNodeCount == C3.totalNodeCount: true
B.route == C1.route                  : true
B.totalNodeCount == C1.totalNodeCount: true

500 repetitions:
Durations:
 A: 122355874000 ns (=  99 x B)
 B:   1232411000 ns (=   1 x B)
C2:   2767531000 ns (=   2 x B)  // map
C3:   2746754000 ns (=   2 x B)  // foldLeft
C1:   2033017000 ns (=   2 x B)  // Java in Scala
Checking consistency
B.route == C2.route                  : true
B.totalNodeCount == C2.totalNodeCount: true
B.route == C3.route                  : true
B.totalNodeCount == C3.totalNodeCount: true
B.route == C1.route                  : true
B.totalNodeCount == C1.totalNodeCount: true
-->

Die Scala-Version läuft etwa 50 Mal schneller als das erste Stück
Java-Code und etwa halb so schnell wie das zweit Stück Java-Code. Für
ein Stück Code, das übersichtlicher ist und das das ursprüngliche
Performance-Problem nicht aufzeigt, finde ich das gut genug.

Geht es schneller in Scala? Wenn der Code auf einem kritischen Pfad
liegt, kann man auch eine schnellere Scala-Version schreiben. Diese
kann dann fast so aussehen wie die Java-Version und ist dann fast so
schnell.  Wenn das immer noch nicht reicht, kann man aus Scala heraus
direkt Java-Code aufrufen (`System.err.print` ist zum Beispiel
Java-Code).

Mit Scala erhält man also für das ursprüngliche Problem
übersichtlichen Code, der auf Anhieb schnell genug ist.

Zum Schluss möchte ich mich bei dynatrace bedanken, welche mir die
Idee zu diesem Blogbeitrag lieferten und mir freundlicherweise völlig
unbürokratisch den Java-Code zur Verfügung stellten. Auf ihrem
englischen Blog [About:Performance](http://apmblog.compuware.com/)
finden Interessierte mehr über Performance und Skalierbarkeit.
