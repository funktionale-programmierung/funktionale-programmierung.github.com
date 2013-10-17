---
layout: post
description: Schleifen in Scala
title: "Schleifen in Scala"
author: andreas-bernauer
tags: ["Scala", "Schleifen", "Endrekursion"]
---

Wer mit funktionaler Programmierung beginnt und sich Code-Beispiele
anschaut, bemerkt schnell, dass die Code-Beispiele 
selten for- oder while-Schleifen verwenden.  Dieser Artikel
beschreibt in Scala, was funktionale Programmierer statt Schleifen
verwenden und zeigt dabei, was es mit Endrekursion auf sich hat.

<!-- more start -->

Java-Programmierer sind es gewohnt, for-Schleifen zu verwenden, wenn
sie über den Inhalt einer Collection iterieren möchten.   Hier ein
[Beispiel](https://github.com/nathanmarz/storm/blob/85895c84ed28eb75ec6e5f7997cda74ce1bed61f/storm-core/src/jvm/backtype/storm/utils/Utils.java#L140)
aus dem Projekt [Storm](http://storm-project.net):

{% highlight java %}
    public static Map readCommandLineOpts() {
        Map ret = new HashMap();
        String commandOptions = System.getProperty("storm.options");
        if(commandOptions != null) {
            commandOptions = commandOptions.replaceAll("%%%%", " ");
            String[] configs = commandOptions.split(",");
            for (String config : configs) {
                String[] options = config.split("=");
                if (options.length == 2) {
                    ret.put(options[0], options[1]);
                }
            }
        }
        return ret;
    }
{% endhighlight %}

`readCommandLineOpts` wandelt die System-Eigenschaft "storm.options"
der Art "foo=13,bar=22" in eine Map "{foo -> 13, bar -> 22}" um.
Kernstück ist dabei eine for-Schleife, welche aus den Konfigurationen
in der System-Eigenschaft die Einträge in der Map erzeugt.

Natürlich kann man so ähnlich auch in Scala programmieren:

{% highlight scala %}
  def readCommandLineOpts() = {
    val ret = new HashMap[String, String]
    var commandOptions = System.getProperty("storm.options")
    if (commandOptions != null) {
      commandOptions = commandOptions.replaceAll("%%%%", " ")
      val configs = commandOptions.split(",")
      for (config <- configs) {
        val options = config.split("=")
        if (options.length == 2) {
          ret.put(options(0), options(1))
        }
      }
    }
    ret
  }
{% endhighlight %}

Hier wird die for-Schleife verwendet, um Einträge in der
Ergebnis-Map `ret` via Seiteneffekt zu erzeugen.  Eine Schleife, die
wegen ihrer Seiteneffekte aufgerufen wird, stellt man in Scala am
besten mit `foreach` dar:

{% highlight scala %}
  def readCommandLineOpts2() = {
    val ret = new HashMap[String, String]
    val commandOptions = System.getProperty("storm.options")
    if (commandOptions != null) {
      commandOptions
        .replaceAll("%%%%", " ")
        .split(",")
        .foreach { config =>
          val options = config.split("=")
          if (options.length == 2) {
            ret.put(options(0), options(1))
          } 
      }
    }
    ret
  }
{% endhighlight %}

`foreach` erhält dabei eine Funktion als Argument, die sie für jedes
Element einer `Iterable` Collection aufruft.  Ich finde,
`readCommandLineOpts2` lässt leichter erkennen, was mit
`commandOptions` passiert: zunächst werden Zeichen ersetzt und an
Kommata getrennt, und anschließend wird jedes Element einzeln
behandelt.  Man kann den Code "von links nach rechts" lesen.  Die
Verkettung von `replaceAll` und `split` ist in Java natürlich auch
möglich.

Ähnlich zu `foreach` gibt es in Scala und anderen funktionalen
Programmiersprachen eine Reihe von Funktionen, welche die üblichen
Anwendungsfälle von Schleifen abdecken:

* `filter(p)` filtert Elemente aus einer Collection, die eine gegebene
  Bedingung erfüllen.
* `count(p)` zählt die Elemente einer Collection, welche eine gegebene
  Bedingung erfüllen.
* `take(n)` liefert die ersten *n* Elemente einer Collection
* `drop(n)` liefert alle Elemente einer Collection *bis auf die ersten n*.
* `takeWhile(p)` liefert die ersten Elemente einer Collection, die
  alle der Bedingung p genügen.
* `dropWhile(p)` liefert alle Element einer Collection, bis auf die
  ersten, welche der Bedingung p genügen.
* `forall(p)` prüft, ob eine Bedingung auf alle Element einer
  Collection zutrifft.
* `contains(p)` prüft, ob eine Bedingung auf irgend ein Element einer
  Collection zutrifft.
* `splitAt(n)` teilt eine Collection an der Position *n* in zwei
  Teile.
* `partition(p)` teilt eine Collection in die zwei Teile, welche der
  Bedingung p genügen oder nicht genügen.
* `foldLeft(start)(op)` ist quasi das Schweizer Taschenmesser und
  steht für folgenden Code-Block:

    {% highlight scala %}
       var result = start
       for (x <- collection) {
           result = op(result, x)
       }
       result
    {% endhighlight %}

    Damit lassen sich praktisch alle for-Schleifen ersetzen.

    In der [Einführung in die rein funktionale
    Programmierung](http://funktionale-programmierung.de/2013/04/10/rein-funktional-2.html)
    hatten wir `foldLeft` als `foldl` der Programmiersprache Racket vorgestellt.

* `reduceLeft(op)` entspricht `foldl(op0)(op)`, wobei op0 das erste
  Element der Collection ist.
* `scanLeft(start)(op)` ist ähnlich zu `foldl`, nur dass es auch alle
  Zwischenergebnisse liefert statt nur dem Endergebnis.
* `map(f)` wendet die Funktion *f* auf jedes Element einer Collection
  an und liefert die Ergebnisse in einer neuen Collection, wobei eine
  etwaige Reihenfolge erhalten bleibt. 
* `collect(pf)` wendet die Funktion *pf* nur auf die Elemente einer
  Collection an, für die *pf* definiert ist und liefert die Ergebnisse
  in einer neuen Collection, wobei eine etwaige Reihenfolge erhalten
  bleibt.


Meiner Erfahrung nach lässt sich mit diesem "Arsenal" an Funktionen
Code lesbarer gestalten: statt lauter for-Schleifen stehen Verben, die
ausdrücken, was die Schleifen erreichen wollen.

Mit `map` und `collect` lässt sich das eingangs erwähnte
`readCommandLineOpts` wie folgt schreiben:

{% highlight scala %}
  def readCommandLineOpts3(): Map[String,String] = {
    System.getProperty("storm.options") match {
      case null => Map()
      case commandOptions =>
        commandOptions.replaceAll("%%%%", " ")
        .split(",")
        .map { _.split("=") }
        .collect { case Array(key, value) => key -> value }
        .toMap
    }
  }
{% endhighlight %}

Nach dem `split` an den Kommas, trennt `map` jedes Element nochmals am
Gleichheitszeichen. `collect` sammelt dann alle Elemente ein, die aus
zwei Werten bestehen und wandelt sie in ein Schlüssel-Werte Paar um.
Die Collection aus Schlüssel-Werte Paaren wird dann zum Schluss
mit`toMap` in eine Map verwandelt.

Ein häufiger Einwand an dieser Stelle ist, dass "unnötige"
Zwischenergebnisse mit einer sehr kurzen Lebensdauer erzeugt werden.
Das stimmt, in der Praxis ist dies jedoch genauso häufig nicht
relevant.  Zum Beispiel kann man bei `readCommandLineOpts` davon
ausgehen, dass es weder viele Kommando-Zeilen Optionen gibt, noch dass
sie besonders groß sind.

Sollte Effizienz dennoch relevant sein, kann man natürlich immer noch
die Lesbarkeit der Effizienz opfern und Schleifen verwenden.
Allerdings bevorzugt man in der funktionalen Programmierung rekursive
Funktionen oder genauer gesagt endrekursive Funktionen.

Endrekursive Funktionen sind Funktionen, bei denen der rekursive
Aufrufe "an letzter Stelle" kommt, das heißt, das Ergebnis des
rekursiven Funktionsaufrufs wird nicht weiter in einer Berechnung
verwendet.

Für das Beispiel des `readCommandLineOpts`, könnte eine endrekursive
Hilfsfunktion `loop` wie folgt aussehen:

{% highlight scala %}
  def readCommandLineOpts5(): Map[String,String] = {
    System.getProperty("storm.options") match {
      case null => Map()
      case commandOptions =>
        def loop(opts: Array[String], result: Map[String, String]): Map[String, String] =
          opts match {
            case Array() => result
            case Array(keyValue, rest@_*) => keyValue.split("=") match {
   /* 86 */   case Array(key, value) => loop(rest.toArray, result + (key -> value))
              case _ => result
            }
          }
        loop(commandOptions.replaceAll("%%%%", " ").split(","), Map())
    }
  }
{% endhighlight %}

`readCommandLineOpts5` erzeugt keine unnötigen Zwischenergebnisse
mehr.  Der erzeugte Bytecode zeigt, dass der rekursive Aufruf von
`loop` in der Zeile 86 in ein simples `GOTO` übersetzt wird:

    L41
     LINENUMBER 82 L41
     ALOAD 11
     ICONST_1
     INVOKEINTERFACE scala/collection/SeqLike.apply (I)Ljava/lang/Object;
    L42
     LINENUMBER 86 L42
     INVOKEVIRTUAL scala/Predef$ArrowAssoc.$minus$greater (Ljava/lang/Object;)Lscala/Tuple2;
     INVOKEINTERFACE scala/collection/immutable/Map.$plus (Lscala/Tuple2;)Lscala/collection/immutable/Map;
     ASTORE 2
     ASTORE 1
     GOTO L0

Damit kann man in der funktionalen Programmierung als Programmier wählen, ob
man seine Schleifen lesbar oder effizient gestalten will.
