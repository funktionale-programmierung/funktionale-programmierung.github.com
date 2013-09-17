---
layout: post
description: "Funktionale Programmierung mit Java 8"
title: "Ist Java 8 eine funktionale Programmiersprache?"
author: michael-sperber
tags: ["Java"]
---

Jein.

Java 8 wird erst 2014 fertig, aber die erste Testversion ist seit
kurzem als [Early Access
Download](http://www.oracle.com/technetwork/java/javase/downloads/ea-jsp-142245.html)
verfügbar.

Seit längerer Zeit ist auch bekannt, wo der inhaltliche Fokus der
neuen Java-Version ist: Im [Project
Lambda](http://openjdk.java.net/projects/lambda/), das endlich
funktionale Features in Java integriert.  Damit trägt Oracle vor allem
der Tatsache Rechnung, dass moderne Multicore-Architekturen in Java
nur sehr umständlich zu Höchstleistungen überredet werden können; dazu
mehr in einem zukünftigen Artikel.  Hier soll es erstmal um das
primäre Feature funktionaler Programmiersprachen gehen: die
Unterstützung von Funktionen als Objekte erster Klasse.

<!-- more start -->

Eigentlich ist es in Java schon seit jeher möglich, Funktionen als
Objekte zu behandeln.  Eine klassische Higher-Order-Funktion wie
`map` können wir z.B. so schreiben:

{% highlight java %}
    public static <A, B> List<B> map(Function<A, B> f, List<A> list) {
        List<B> result = new ArrayList<B>(list.size());
        for (A a : list) {
            result.add(f.apply(a));
        }
        return result;
    }
{% endhighlight %}

Diese Funktion akzeptiert eine Funktion und eine Liste, wendet die
Funktion auf jedes Element der Liste an, und liefert eine Liste der
Resultate.

Leider ist das Beispiel noch nicht vollständig, da der Typ `Function`
nicht bei Java mitgeliefert wird.  Hier ist seine Definition:

{% highlight java %}
interface Function<A, B> {
    B apply(A a);
}
{% endhighlight %}

Die `map`-Funktion ist nicht wesentlich schwieriger zu implementieren
als in funktionalen Sprachen.  Allerdings treten bei der *Benutzung*
das Problem zutage, zumindest in Java vor Version 8: Dort gibt es
nämlich keine direkte Notation, um Funktionen-als-Objekte
hinzuschreiben.  Stattdessen ist die platzsparendste Methode,
Funktionsobjekte herzustellen, die [anonyme innere
Klasse](http://docs.oracle.com/javase/tutorial/java/javaOO/anonymousclasses.html):

{% highlight java %}
        Integer[] array = {1, 2, 3};
        List<Integer> list = Arrays.asList(array);
        List<Integer> result = map(new Function<Integer, Integer>() {
            public Integer apply(Integer arg) {
                return arg + 1;
            }
        }, list);
{% endhighlight %}

Diese Notation ist schon eine Verbesserung gegenüber Java 1.0, wo es
noch keine inneren Klassen gab. Für die alltägliche Verwendung in dem
Maß, wie in funktionalen Sprachen üblich, ist diese Notation aber viel
zu umständlich und noch dazu schwer lesbar.  Typische
Programmiermuster aus der funktionalen Programmierung sind damit zwar
prinzipiell umsetzbar, aber nicht wirklich praktikabel.

Java 8 "Project Lambda" bietet nun eine kürzere Notation für
Funktionen-als-Objekte über sogenannte Lambda-Audrücke, die es
erlaubt, den Aufruf von `map` drastisch zu verkürzen:

{% highlight java %}
        List<Integer> result = map(x -> x + 1, list);
{% endhighlight %}

Das ist eine tolle Sache und macht viele Programmiertechniken aus der
funktionalen Programmierung in Java nicht nur hoffähig sondern auch
praktikabel.

Besonders schön ist, dass die Definition von `map` nicht verändert
werden muss, um von den Lambda-Ausdrücken zu profitieren: Der
Java-Compiler transformiert den Lambda-Ausdruck `x -> x + 1`
automatisch in die anonyme innere Klasse, die wir in Java vor Version
8 noch schreiben müssen.  Das macht der Compiler über sogenanntes
*Target Typing*: Der Kontext eines Lambda-Ausdrucks bestimmt den
genauen Typ, und der Compiler inferiert hieraus den zu genierenden
Code.

Dieses Feature ist allerdings gleichzeitig auch ein Manko, da
Lambda-Ausdrücke keine unabhängige Existenz haben.  Folgendes geht
also z.B. nicht:

{% highlight java %}
    Object f = (Integer x) -> x + 1;
{% endhighlight %}

Der Compiler beschwert sich mit:

    Target type of a lambda conversion must be an interface type.

Oder anders formuliert : Funktionen, die durch Lambdas erzeugt werden,
müssen immer als Typ ein vordefiniertes Interface haben.  Es kommen
dafür sogenannte *single-method interfaces* in Frage, also solche
Interfaces, die genau eine Methode aufweisen.  Dies steht im Gegensatz
zu funktionalen Sprachen (genauer gesagt den statisch getypten
funktionalen Sprachen), bei denen Funktionstypen der Form `A -> B`
schon vorn vorhherein eingebaut sind.  Das heißt insbesondere, dass
jede Stelligkeit ein eigenes Interface erfordert: Das Interface
`Function` von oben funktioniert nur für einstellige Funktionen.  Die Unterscheidung
zwischen primitiven Typen und Referenztypen erfordert außerdem weitere
Überladung.  Entsprechend kommt Java 8 mit einem ganzen Zoo speziell
definierter Funktionstypen, zu besichtigen
[hier](http://download.java.net/jdk8/docs/api/java/util/function/package-summary.html).
(Da sind noch nicht einmal Funktionen mit mehr als zwei Parametern
dabei.)

Diese vielen spezialisierten Interfaces führen dazu, dass viele
Higher-Order-Funktionen immer noch nicht so nützlich sind wie in
funktionalen Sprachen.  Zum Beispiel ist es leicht möglich, eine
Kompositionsfunktion zu schreiben:

{% highlight java %}
    public static <A, B, C> Function<A, C> compose(Function<B, C> g, Function<A, B> f) {
        return x -> g.apply(f.apply(x));
    }
{% endhighlight %}

Diese funktioniert - wie `map` - eben nur für `Function`, aber nicht
für die anderen Interfaces für einstellige Funktionen, die in
`java.util.function` definiert sind, wie z.B. `Predicate`, `IntPredicate`,
`UnaryOperator`, `DoublePredicate`, `DoubleToIntFunction`,
`DoubleUnaryoperator`, `IntToDoubleFunction`, `IntToLongFunction`,
`IntUnaryOperator`, `LongPredicate`,`LongToDoubleFunction`,
`LongToIntFunction`, `LongUnaryOperator`, `ToDubleFunction`,
`ToIntFunction` und `ToLongFunction`.

Mit anderen Worten: Es gibt in Java 8 nun Lambda-Ausdrücke und
Funktionsobjekte, aber keine Funktionstypen - 5 von 10 Punkten, könnte
man sagen.

Diese Einschränkung ist funktionale Programmierer bitter, aus Sicht
der Java-Macher allerdings verständlich: Sie wollen einerseits Java
weiterentwickeln, ohne den Kern zu verändern.  Die Einführung
richtiger Funktionstypen würde allerdings massive Änderungen in der
JVM erfordern, die Oracle bisher noch scheut.  Hoffen wir, dass es
trotzdem eines Tages passiert.

<!-- more end -->
