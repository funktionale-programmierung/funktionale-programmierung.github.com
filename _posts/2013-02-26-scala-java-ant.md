---
layout: post
title: "Funktional Programmieren am konkreten Beispiel (Ant)"
author: david-frese
tags: Scala Einführung
meta_description: >
  Dieser Artikel veranschaulicht die Vorteile der funktionalen Programmierung
  anhand eines konkreten Beispiels aus dem Java-Programm Ant.
page_title: "Ein konkretes Beispiel (Ant)"
---

Die Vorteile der funktionalen Programmierung bei der
Code-Wiederverwendung, Test- und Beweisbarkeit oder der
Parallelisierung kann man abstrakt aufzählen und erläutern, aber
wesentlich anschaulicher wird es doch, wenn man mal die gleiche
Funktionalität in einer imperativen und einer funktionalen Sprache
gegenüberstellt.

In diesem konkreten Beispiel nehmen wir einen kleinen Teil eines
reellen Java-Programms, und zeigen wie dieser mit rein funktionalen
Elementen in der Programmiersprache
[Scala](http://www.scala-lang.org/) implementiert werden kann. Dabei
wird zum Beispiel deutlich, wie die um ein vielfaches besseren
Möglichkeiten der Wiederverendung von Code die Fehlerrate deutlich
senkt.

<!-- more start -->

Der Java-Code in diesem Artikel ist nicht frei erfunden oder vom
Autor extra "schlecht" implementiert um den funktionalen Code
besonders gut aussehen zu lassen, sondern er stammt direkt aus dem
Quellcode des sehr weit verbreiteten Programms
[Ant](http://ant.apache.org). Der Code wurde nur etwas reduziert und
unwesentlicht vereinfacht, um den Artikel nicht noch länger werden zu
lassen.

Der Code in diesem Artikel implementiert vier der sogenannten
[_Mapper_](http://ant.apache.org/manual/Types/mapper.html), die in den
XML-Build-Dateien von Ant folgendermaßen kombiniert werden können:

{% highlight xml %}
<compositemapper>
  <identitymapper/>
  <chainedmapper>
    <globmapper from="*.java" to="*.class" />
    <globmapper from="src/*" to="classes/*" />
  </chainedmapper>
</compositemapper>
{% endhighlight %}

Ein Mapper bildet dabei einen einzelnen Dateinamen auf eine Liste von
Dateinamen ab, die eventuell leere sein kann. Es gibt Basis-Mapper
(`identitymapper` und `globmapper`), sowie Mapper die eine Reihe von
untergeordneten Mappern zu einem neuen kombinieren (`compositemapper`
und `chainedmapper`).

Das obige Beispiel würde den Dateienamen "demo/src/Hello.java" auf
eine Liste aus den Dateinamen "demo/src/Hello.java" und
"demo/classes/Hello.class" abbilden.

Die folgende Abschnitte beschreiben nun Stück für Stück die
Implementierung dieser Mapper in Java, und zeigt wie man sie in Scala
implementieren könnte.

## Wenige Typen, viele Operationen

In der objekt-orientierten Programmierung ist man geneigt oder sogar
gezwungen für die Implementierung einer neuen Funktionalität zunächst
einmal eine Klasse, und damit einen neuen Typ einzuführen. Ob man
diesen Typ dann später überhaupt benötigt, oder die Klasse nur zur
Modularisierung des Codes dient, ist dabei nicht entscheidend.

Die funktionale Programmierung verfolgt einen anderen Ansatz, der sich
mit der Überschrift "Wenige Typen, viele Operationen" zusammenfassen
lässt. Dies steht für das Prinzip, dass man einen neuen Typen nur dann
einführt, wenn man ihn wirklich benötigt. Wenn sich eine Operation als
Funktion über bestehenden Datentypen ausdrücken lässt, dann sollte man
das tun.

<!---Das hat Alan J. Perlis, der ersten Gewinner des Touring-Award schon
vor über 30 Jahren aufgegriffen:

> It is better to have 100 functions operate on one data structure
> than 10 functions on 10 data structures.
> <small>Alan J. Perlis in <cite title="Epigrams in Programming">Epigrams in Programming, 1982</cite></small>
-->
Dementsprechend ist in unserem Java-Code ein Mapper ein Objekt dass
folgenden Interface-Typ implementiert:

{% highlight java %}
interface Mapper {
  public List<String> mapFileName(String sourceFilename);
};
{% endhighlight %}

In unserem Scala-Code definieren wir hierfür keinen eigenen Typ,
sondern definieren einen Mapper einfach direkt als einen Funktion die
einen String auf eine Liste von Strings abbildet. Jede Funktion mit
dieser Signatur kann als Mapper verwendet werden. Um dieser Art von
Funktionen dennoch einen Namen geben zu können, der den Code lesbarer
und leichter zu schreiben macht, gibt es in Scala sogenannte
Typ-Aliase. Der folgende Code definiert `Mapper` als Alias (als
Abkürzung) für den Typ von Funktionen, die einen String nehmen und
eine Liste von Strings zurückgeben:

{% highlight scala %}
type Mapper = String => List[String];
{% endhighlight %}

Machen wir uns jetzt an die Implementierung des einfachsten Mappers,
den `IdentityMapper`.

## IdentityMapper -- einfache Funktionen

Der IdentityMapper soll einfach den zu verarbeitenden Dateinamen
unverändert zurückgeben. In der Implementierung heißt das also das
Argument in eine Liste packen und zurückgeben, was in Java
folgendermaßen gelöst wurde:

Als Java-Klasse:
{% highlight java %}
class IdentityMapper implements Mapper {
  public IdentityMapper() {};

  @Override
  public List<String> mapFileName(String sourceFileName) {
    List<String> result = new ArrayList<String>();
    result.add(sourceFileName);
    return result;
  }
};
{% endhighlight %}

Der funktionale IdentityMapper ist eine Funktion, die einen String
nimmt und diesen String in eine Liste packt - eine sehr einfache
Funktion. Dies können wir folgendermaßen in Scala schreiben:

{% highlight scala %}
def identityMapper =
  (sourceFileName : String) => List(sourceFileName)
{% endhighlight %}

Was hier nach kurzem Überlegen auffällt, ist dass es nur einen
`identityMapper` gibt; *der* IdentityMapper ist ein einzelner,
eindeutiger Wert im Programm. Es ist nicht notwendig und auch nicht
möglich einen Zweiten zu erzeugen.

Auch mit der obigen Java-Implementierung könnte man ein und dieselbe
Instanz der Klasse IdentityMapper wiederverwenden, da sie keinen
inneren Zustand besitzt. Aber das umzusetzen, ist erheblicher
zusätzlicher Programmieraufwand, und ist im originalen Ant-Quellcode
auch nicht gemacht worden - dort werden immer wieder neue Objekte der
Klasse IdentityMapper erzeugt.

Um die Definition von Funktionen noch einfacher zu machen, bietet
Scala noch knappere Möglichkeiten, die aber alle zum gleichen Ergebnis
führen. Eine davon wollen wir im folgenden öfters verwenden:

{% highlight scala %}
def identityMapper(sourceFileName : String) = List(sourceFileName)
{% endhighlight %}

Hier wird mit dem Namen der Funktion auch gleich eine Parameter-Liste
angegeben, und auf der rechten Seite des Gleihheitszeichens steht nur
noch die Definition.

Nun aber weiter zum `CompositeMapper`.

## CompositeMapper -- Closures

Der CompositeMapper kombiniert mehrere untergeordnete Mapper. Der
Dateiname wird dabei an jeden Mapper übergeben, und alle Ergebnisse in
eine gemeinsame Liste gesteckt, die dann das Ergebnis des CompositeMapper
darstellt.

Die Java-Implementierung besteht aus folgender Klasse:

{% highlight java %}
class CompositeMapper implements Mapper {
  private List<? extends Mapper> mappers;

  public CompositeMapper(List<? extends Mapper> mappers) {
    this.mappers = mappers;
  }

  @Override
  public List<String> mapFileName(String sourceFileName) {
    List<String> result = new ArrayList<String>();
    for (Mapper m : this.mappers) {
      List<String> mapped = m.mapFileName(sourceFileName);
      result.addAll(mapped);
    }
    return result;
  }
};
{% endhighlight %}

Für jeden untergeordneten Mapper ruft er die Abbildungsfunktion
`mapFileName` mit dem selben Dateinamen auf, sammelt die Ergebnisse in
einer neuen Liste zusammen und gibt diese zurück.

Wie sieht nun der funktionale Ansatz hierfür aus? Nun, wir definieren
eine Funktion `compositeMapper`, die eine Liste von Mappern nimmt, und
einen entsprechenden neuen Mapper erzeugt und diesen zurückgibt. Da
ein Mapper selbst eine Funktion ist die einen Dateinamen als Parameter
nimmt, ist unsere Funktion `compositeMapper` also eine Funktion die
eine Funktion zurückgibt; das Grundgerüst sieht daher folgendermaßen
aus:

{% highlight scala %}
def compositeMapper(mappers : List[Mapper]) =
  (sourceFileName : String) => ...
{% endhighlight %}

Eine solche zurückgegebene, neu erzeugte Funktion nennt man auch
"Closure", was sich hier in etwa mit "Umschließung" übersetzen
lässt. Diese Bezeichnung rührt daher, dass die innere Funktion nicht
nur auf die eigenen Parameter zugreifen kann, sondern auch auf die
Parameter der äußeren, der _umschließenden_ Funktion(en).

Für die Implementierung ("..."), können wir also direkt die Liste
`mappers` und den Dateinamen `sourceFileName` verwenden. Und was
sollen wir damit machen? Zunächst brauchen wir das Ergebnis jedes
Mappers in der Liste, wenn man ihn mit dem Dateinamen füttert. Für
diese Kategorie von Abbildungen gibt es in Scala bereits eine Funktion,
die das Iterieren über eine Eingangsliste und Zusammensetzen der
Ergebnisliste gleicher Länge übernimmt. Diese Funktion heißt `map`,
und das einzige was man ihr noch zusätzlich übergeben muss, ist eine
Funktion die ein einzelnes Element der Liste abbildet. Nennen wir sie
einfach `f`:

{% highlight scala %}
def f(m : Mapper) = m(sourceFileName)
{% endhighlight %}

In Scala ist `map` als Methode der Eingangsliste definiert, deswegen
schreibt man den Aufruf folgendermaßen:

{% highlight scala %}
mappers.map(f)
{% endhighlight %}

Dies liefert uns eine Liste von Listen mit den jeweiligen Ergebnissen
der einzelnen untergeordneten Mapper. Das ist noch nicht ganz was wir
brauchen, denn wir wollen alle Ergebnisse zusammen in einer großen
Liste. Auch diese Operation ist in Scala schon implementiert und
nennt sich "flatten". Alles zusammen können wir den CompositeMapper
also jetzt so definieren:

{% highlight scala %}
def compositeMapper(mappers : List[Mapper]) =
  (sourceFileName : String) => {
    def f(m : Mapper) = m(sourceFileName)
    mappers.map(f).flatten
  }
{% endhighlight %}

Die Implementierung der Funktion ist hier in geschweifte Klammern
gesetzt, innerhalb derer man in Scala zum Beispiel lokale Funktionen
wie hier `f` definieren kann. Der Wert und der Typ des geklammerten
Ausdrucks bestimmt sich dabei direkt über den *letzten* Ausdrucks
innerhalb der Klammern. Ein `return` ist nicht notwendig.

## ChainedMapper -- Listen falten

Auch der ChainedMapper kombiniert mehrere untergeordnete Mapper,
jedoch anders als der CompositeMapper. Der Dateiname wird dabei
zunächst an den ersten untergeordneten Mapper übergeben. Dessen
Ausgaben dienen, eine nach der anderen, als Eingabe für den zweiten
Mapper, usw. Die Ausgabe des letzten Mappers ist dann die Ausgabe des
kombinierten ChainedMapper.

Am Beispiel des ChainedMapper lässt sich die Verwendung von `foldLeft`
gut demonstrieren, einer weiteren sehr häufig verwendbaren Funktion
zum Verarbeiten von Listen. Zunächst aber wieder der Java-Code, in dem
die verschiedenen Listen alle mit `for`-Schleifen und `addAll`
verarbeitet werden:

{% highlight java %}
class ChainedMapper implements Mapper {
  private List<? extends Mapper> mappers;

  public ChainedMapper(List<? extends Mapper> mappers) {
    this.mappers = mappers;
  }

  @Override
  public List<String> mapFileName(String sourceFileName) {
    List<String> result = new ArrayList<String>();
    result.add(sourceFileName);
    for (Mapper m : this.mappers) {
      List<String> input = new ArrayList<String>();
      input.addAll(result);
      result.clear();
      for (String fn : input) {
        List<String> subres = m.mapFileName(fn);
        result.addAll(subres);
      };
    }
    return result;
  }
};
{% endhighlight %}

Der ChainedMapper wendet die untergeordneten Mapper einen nach dem
anderen an, jeweils mit dem Ergebnis des vorherigen als Eingabe in den
nächsten, beginnend mit dem übergebenen `sourceFileName` als
Eingabe für den Ersten.

Genau für diese Art der der Verarbeitung einer Liste, Element für
Element, von links nach rechts, und dem Konstruieren des Ergebnisses
aus dem jeweiligen Element und dem vorherigen Zwischenergebnis gibt es
in Scala die Methode `foldLeft` von Listen.

Der komplette ChainedMapper sieht dann damit so aus:

{% highlight scala %}
def chainedMapper(mappers : List[Mapper]) =
  (sourceFileName : String) => {
    def f(input : List[String], m : Mapper) =
      input.map(m).flatten
    mappers.foldLeft(List(sourceFileName))(f)
  }
{% endhighlight %}

Die lokale Funktion `f` nimmt die bisherigen Ergebnisse und einen
Mapper aus der Liste als Parameter, wendet den Mapper auf jedes
Element der Liste an, und kombiniert die einzelnen Ergebnisse zu einer
flachen Liste von Strings. Die Funktionen `map` und `flatten` sind ja
weiter oben bereits beschrieben.

In der nächsten Zeile iteriert dann also `foldLeft` über die Liste
`mappers`. Das erste Zwischenergebnis ist eine Liste mit dem
`sourceFileName`. Die Funktion `f` wird nun für jedes Element der
Liste `mappers` einmal aufgerufen, jeweils mit dem vorherigen
Zwischenergebnis als erstem Parameter. Das letzte Zwischenergebnis ist
dann gleichzeitig das Endergebnis und der Rückgabewert der
`foldLeft`-Abbildung. Aus Gründen auf die hier nicht näher eingegangen
werden kann, hat `foldLeft` dabei nicht zwei Parameter, sondern die
Startliste und die Funktion `f` müssen "nacheinander" übergeben
werden.

Die beiden Funktionen `map` und `foldLeft` für die Verarbeitung von
Listen gibt es im Prinzip in jeder funktionalen Sprache, und wann
immer man über die Elemente einer Liste iterieren möchte, bietet es
sich an kurz darüber nachzudenken, ob dies mithilfe einer dieser
Funktionen machbar ist - und das ist es fast immer. Scala bietet neben
diesen beiden Funktionen auch noch eine Reihe anderer üblicher
Listen-Operationen, darunter zum Beispiel auch `flatMap`, das die hier
verwendete Kombination auf `map` und `flatten` gleich auf einen Schlag
durchführt.

## GlobMapper -- Tupel

Zum Abschluss noch den GlobMapper, der einen Dateinamen anhand eines
einfachen Musters auf einen anderen Dateinamen abbildet. Seine
Implementierung benötigt einige Zeilen mehr Code, aber sie zeigt sehr
schön, warum die Wiederverwendung von Code insbesondere in Java so
schwierig ist. Sie ist so schwierig, dass man sehr oft auf Copy&Paste
zurückgreift, wie hier bei den Methoden `setTo` und `setFrom`:

{% highlight java %}
class GlobMapper implements Mapper {
  private String fromPrefix, fromPostfix = null;
  private int prefixLength;
  private int postfixLength;
  private String toPrefix, toPostfix = null;
  private boolean fromContainsStar, toContainsStar = false;

  public GlobMapper(String from, String to) {
    this.setFrom(from);
    this.setTo(to);
  }

  public void setTo(String to) {
    int tidx = to.lastIndexOf("*");
    if (tidx == -1) {
      this.toPrefix = to;
      this.toPostfix = "";
    }
    else {
      this.toPrefix = to.substring(0, tidx);
      this.toPostfix = to.substring(tidx + 1);
      this.toContainsStar = true;
    }
  }

  public void setFrom(String from) {
    int fidx = from.lastIndexOf("*");
    if (fidx == -1) {
      this.fromPrefix = from;
      this.fromPostfix = "";
    }
    else {
      this.fromPrefix = from.substring(0, fidx);
      this.fromPostfix = from.substring(fidx + 1);
      this.fromContainsStar = true;
    }
    this.prefixLength = this.fromPrefix.length();
    this.postfixLength = this.fromPostfix.length();
  }

  @Override
  public List<String> mapFileName(String sourceFileName) {
    List<String> result = new ArrayList<String>();
    if ((sourceFileName.length() < (prefixLength + postfixLength))
        || (!fromContainsStar
            && !sourceFileName.equals(fromPrefix)
           )
        || (fromContainsStar
            && (!sourceFileName.startsWith(fromPrefix)
               || !sourceFileName.endsWith(fromPostfix))
           )
       )
    {
      return result;
    }
    result.add(toPrefix
            + (toContainsStar ?
                    extractVariablePart(sourceFileName) + toPostfix
                    : ""));
    return result;
  }

  private String extractVariablePart(String name) {
    return name.substring(this.prefixLength, name.length() - this.postfixLength);
  }
};
{% endhighlight %}

In `setTo` und im ersten Teil von `setFrom` wird ein Pattern bei
Vorhandensein eines Sternchen in zwei Teile geteilt und in jeweils in
einer "Prefix" und einer "Postfix" Variable gespeichert. Das
Duplizieren von Code hat dabei nicht nur zur Folge dass der Code
länger wird, sondern auch dass man den Fehler, der sich in diese
Methoden eingeschlichen hat (wer hat ihn entdeckt?), an zwei Stellen
korrigieren muss.

In einer funktionalen Implementierung können wir diesen gemeinsamen
Code sehr leicht als eine Funktion implementieren, die ein sogenanntes
Tupel zurückgibt. Ein Tupel ist eine einfache Möglichkeit zwei oder
mehr Werte zusammenzupacken und wie hier als Ergebnis einer Funktion
zu verwenden:

{% highlight scala %}
def splitAtLastStar(s : String) = {
  val p = s.lastIndexOf('*')
  if (p >= 0) {
    val pre = s.substring(0, p);
    val post = s.substring(p+1);
    (pre, post)
  }
  else (s, "")
}
{% endhighlight %}

Ein Tupel erzeugt man in Scala einfach durch Umschließen der Werte in
runde Klammer, mit einem Komma zwischen den einzelnen Elementen.

Wir können nun also diese Funktion `splitAtLastStar` verwenden, um in
der Implementierung von `globMapper` die beiden Pattern
auseinanderzunehmen. Besonders praktisch ist dabei, dass das
Bindungskonstrukt `val` von Scala ein solches Tupel gleich wieder
auseinandernehmen kann, und es einem so ermöglicht den einzelnen
Teilen des Tupels eigene Namen zu geben:

{% highlight scala %}
def globMapper(fromPat : String, toPat : String) = {
  val (fromPrefix, fromPostfix) = splitAtLastStar(fromPat)
  val (toPrefix, toPostfix) = splitAtLastStar(toPat)
  ...
}
{% endhighlight %}

Die Tupel sind dabei streng typisiert, was bedeutet, dass man für eine
Funktion die wie hier ein Tupel mit 2 Elementen zurückgibt auch genau
zwei Namen angeben muss. Außerdem kann der Scala-Kompiler ableiten,
dass beide Werte vom Typ String sind. Im allgemeinen können Tupel aber
auch Werte unterschiedlichen Typs enthalten, zum Beispiel einen String
und einen Integer.

Mit einer weiteren Funktion die einen String anhand der Länge von
Prefix und Postfix in drei Teile teilt...

{% highlight scala %}
def splitPrePost(s : String, preLen : Int, postLen : Int) = {
  val (pre, r) = s.splitAt(preLen);
  val (mid, post) = r.splitAt(r.length() - postLen);
  (pre, mid, post)
}
{% endhighlight %}

...können wir die Funktion `globMapper` schließlich folgendermaßen
implementieren:

{% highlight scala %}
def globMapper(fromPat : String, toPat : String) = {
  val (fromPrefix, fromPostfix) = splitAtLastStar(fromPat)
  val (toPrefix, toPostfix) = splitAtLastStar(toPat)
  (sourceFileName : String) => {
    val (pre, mid, post) = splitPrePost(sourceFileName,
                                        fromPrefix.length(), fromPostfix.length())
    if ((pre == fromPrefix) && (post == fromPostfix))
      List(toPrefix + mid + toPostfix)
    else
      List()
  }
}
{% endhighlight %}

## Zusammenfassung

Der Artikel hat an einem konkreten Beispiel aus der Praxis gezeigt wie
man ein Java Programm einfacher in der funktionalen Sprache Scala
programmieren kann. Dabei hat er einige Grundelemente der funktionalen
Programmierung erklärt, nämlich:

- Wenige Typen, viele Operationen
- Funktionen als Parameter
- Closures
- Iteration über Listen mit map und fold
- Mehrere Rückgabewerte durch Tupel

Der Scala-Code zu diesem Beitrag kann
[hier](/files/beispiel-ant/Mappers.scala) heruntergeladen werden.
