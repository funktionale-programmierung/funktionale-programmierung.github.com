---
layout: post
title: "Funktionale Programmierung mit Swift"
author: stefan-wehr
tags: ["Swift", "Apple", "Mac", "iOS"]
---

Letztes Jahr hat Apple mit [Swift](https://developer.apple.com/swift/)
eine neue Programmiersprache vorgestellt. Über kurz oder lang wird Swift
die Standardsprache werden, um Apps für iPhone, iPad, Mac und Co zu
entwicklen. Und Swift enthält viele Elemente der funktionalen
Programmierung, Zeit genug also, dass wir in diesem Blob mal einen
genaueren Blick auf die Sprache werfen.

Interessant ist auch, dass Apple in Swift nicht nur einfach ein paar
liebgewonnene Features aus funktionalen Sprachen einbaut. Nein, es
scheint vielmehr, dass grundlegende funktionale Designparadigmen wie
"Werte statt veränderbare Objekte" und "Seiteneffekte ja, aber mit
Disziplin" auch in Apple Vorstellung von guter Softwarearchitektur eine
große Rolle spielen. Exemplarisch seien hier zwei Vorträge der
[Apple Worldwide Developers Conference](https://developer.apple.com/wwdc/) genannt.
Im Vortrag
[Building Better Apps with Value Types in Swift](https://www.youtube.com/watch?v=av4i3x-aZbM)
geht es um die Vorteile von *Werttypen* (*value types*) in Swift, also von
Typen deren Werte unveränderbar sind. Und der Vortrag
[Advanced iOS Application Architecture and Patterns](https://developer.apple.com/videos/play/wwdc2014-229/)
schlägt in dieselbe Kerbe, hier
geht es ebenfalls um Werttypen und *Unveränderbarkeit* (*immutability*).

<!-- more start -->

Im heutigen Blogartikel schauen wir uns anhand eines Beispiels einige der
Sprachfeatures von Swift an. Wir möchten eine kleine Bibliothek zum
Zeichnen von Diagrammen designen und implementieren. Damit können wir dann
z.B. solche Diagramme zeichnen:

<div id="center">
<img src="/files/swift/diag1.png" />
</div>

Solche Diagramme können wir natürlich auch mit herkömmlichen, imperativen
Mitteln zeichnen. Etwa so:

{% hightlight swift %}
NSColor.blueColor().setFill()
CGContextFillRect(ctx, CGRectMake(0.0, 37.5, 75.0, 75.0)
NSColor.redColor().setFill()
CGContextFillRect(ctx, CGRectMake(75.0, 0.0, 150.0, 150.0))
{% endhighlight %}

Dieser Code benutzt die Mac API zum Zeichnen, aber das Grundprinzip ist in
fast alle UI Toolkits gleich: wir benutzen einen Grafikkontext `ctx`, um
primitive Formen wie Rechtecke und Kreise auf den Bildschirm zu
zeichnen. Wir sagen dem System also genau *wie* gezeichnet werden soll.

Was passiert nun aber, wenn wir das Diagramm leicht ändern möchten und
z.B. eine grünen Kreis zwischen die beiden Rechtecke einfügen sollen.

<div id="center">
<img src="/files/swift/diag2.png" />
</div>

Mit dem imperativen Ansatz (*wie* wird gezeichnet) müssen wir nicht nur
neuen Code für den Kreis schreiben, sondern wir müssen auch bestehen Code
ändern, um das rote Rechteck weiter nach rechts zu schieben:

{% highlight swift %}
NSColor.blueColor().setFill()
CGContextFillRect(ctx, CGRectMake(0.0, 37.5, 75.0, 75.0))
// neuer Code
NSColor.greenColor().setFill()
CGContextFillEllipseInRect(ctx, CGRectMake(75.0, 37.5, 75.0, 75.0))
// alter Code, muss verändert werden
NSColor.redColor().setFill()
CGContextFillRect(ctx, CGRectMake(150.0, 0.0, 150.0, 150.0))
{% endhighlight %}

Mit einem funktionalen Desig werden solche Probleme vermieden. Denn
funktional gedacht spezifizieren wir lediglich *was* gezeichnet werden
soll und überlassen das *wie* einer Bibliothek.

In folgenden schauen wir uns an, wie wir in Swift Diagramm
deklarativ spezifizieren können und wie wir eine Bibliothek zum Umsetzen
der Spezifikation in echte Bilder realisieren können. Die Idee zu diesem
Beispiel stammt aus dem schönen Buch
[Functional Programming in Swift](https://www.objc.io/books/) von
Chris Eidhof, Florian Kugler und Wouter Swierstra.

## Spezifikation von Diagramm

Um zu Spezifizieren, was in einem Diagram enthalten sein soll, benutzen
wir das `enum`-Feature von Swift.

{% highlight swift %}
enum Shape {
    case Ellipse
    case Rectangle
}

// Das Schlüsselwort "indirect" wird benötigt, da der enum-Typ
// rekursiv ist.
indirect enum Diagram {
    case Primitive(CGSize, Shape)
    case Beside(Diagram, Diagram)
    case Below(Diagram, Diagram)
    case Annotated(Attribute, Diagram)
}

enum Attribute {
    case FillColor(NSColor)
    case Alignment(Align)
}

// Alignment auf der x- und y-Achse sind Werte zwischen 0 und 1, wobei
// 0 ganz links bzw. oben und 1 ganz rechts und unten bedeutet. Der Wert
// CGPointMake(x: 0.5, y: 1.0) spezifiziert als ein horizontal zentriertes
// und vertikal am unteren Rand fixiertes Alignment.
typealias Align = CGPoint
{% endhighlight %}

Ein Diagramm ist als entweder eine primitive Form mit einer Größe (die
Größe ist nicht in Pixel angegeben, sondern relativ zu den anderen
Diagrammelementen gedacht), oder zwei Diagramme neben- bzw. untereinander,
oder ein annotiertes Diagramm. Mit solche annotierten Diagrammen
modellieren wir Farben (`FillColor`) und Anordnung (`Alignment`).

Am obigen Beispiel sehen Sie, dass Enums in Swift viel mächtiger sind
als reine Aufzählungstypen wie beispielsweise in Java oder C#. Enums
können nämlich auch rekursiv sein (Schlüsselwert `indirect`) und die
einzelnen Alternativen eines Enums können mit Argumenten versehen sein. In
funktionalen Sprache sind solche Datentypen Standard; sie heißen dort
*algebraischen Datentypen* oder auch *Summentypen*.

## Beispiel-Diagramme

Nun schauen wir uns an, wie wir mit diesen Enums ein Diagramm
spezifizieren können:

{% highlight swift %}
let blueSquare = Diagram.Annotated(Attribute.FillColor(.blueColor()),
    Diagram.Primitive(CGSize(width:1, height:1), Shape.Rectangle))
{% endhighlight %}

Mit `let` führen wir eine neue Variable `blueSquare` ein, deren Wert nicht
verändert werden kann.

Das obige Diagram `blueSquare` ist sehr einfach und besteht nur aus einem blauen
Quadrat. Trotzdem ist der Code zum Erzeugen etwas länglich. Wir können ihn
vereinfachen, indem wir Hilfsfunktionen bereitstellen. In OO-Sprachen sagt
man zu solchen Funtionen oft "Smarte Konstruktoren", in funktionalen
Sprache werden sie auch "Kombinatoren" genannt.

{% highlight swift %}
func square(side: CGFloat) -> Diagram {
    return Diagram.Primitive(CGSize(width:side, height:side), Shape.Rectangle)
}

func circle(radius: CGFloat) -> Diagram {
    return Diagram.Primitive(CGSize(width:2*radius, height:2*radius), Shape.Ellipse)
}

func rectangle(width: CGFloat, height: CGFloat) -> Diagram {
    return Diagram.Primitive(CGSize(width:width, height:height), Shape.Rectangle)
}
{% endhighlight %}

//: Farben und Alignment

{% highlight swift %}
extension Diagram {
    func fill(color: NSColor) -> Diagram {
        return Annotated(Attribute.FillColor(color), self)
    }
    func align(x: CGFloat, y: CGFloat) -> Diagram {
        return Annotated(Attribute.Alignment(CGPoint(x:x, y:y)), self)
    }
    func alignRight() -> Diagram {
        return align(1, y:0.5)
    }
    func alignTop() -> Diagram {
        return align(0.5, y:1)
    }
    func alignBottom() -> Diagram {
        return align(0.5, y:0)
    }
}

//: Operatoren zur Platzierung nebeneinander- bzw. untereinander

{% highlight swift %}
infix operator ||| { associativity left }
func ||| (l: Diagram, r: Diagram) -> Diagram {
    return Diagram.Beside(l, r)
}

infix operator --- { associativity left }
func --- (top: Diagram, bottom: Diagram) -> Diagram {
    return Diagram.Below(top, bottom)
}
{% endhighlight %}
<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## Überschriften ##
 
Code: `f(a,b)`. Als Block:

    <-- vier Leerzeichen eingerückt

Codebeispiele einer Programmiersprache:

{% highlight scheme %}
(repl)
{% endhighlight %}

Links mit [Text](http://URL).

Hervorhebungen *mit Stern* oder _Unterstrich_.  **Doppelt** für mehr
__Druck__.  Geht auch mitt*endr*in in einem Wort.

<!-- more end -->

