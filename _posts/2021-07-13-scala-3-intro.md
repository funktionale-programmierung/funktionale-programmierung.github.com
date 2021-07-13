---
layout: post
description: "Einführung in Scala 3. Syntax im Detail."
title: "Scala 3: Scala im neuen Gewand" 
author: simon-haerer
tags: ["scala", "3", "syntax"]
---

Nach 8 Jahren, 28000 commits und 7400 pull-requests war es am 14. Mai 2021
endlich so weit: Scala 3 wurde veröffentlicht. Neben dem neuen Compiler "Dotty"
haben es eine neue Syntax sowie einige Neuerungen an der Sprache
in Scala 3 geschafft.  In dieser Blogpost-Reihe werden einige der interessanten
Neuerungen im Detail vorgestellt.  In diesem ersten Teil der Reihe wird der 
"quiet mode" vorgestellt. Dabei handelt es sich um eine alternative Syntax, die
mit Einrückungen Blockbildung vornimmt, statt mit geschweiften Klammern.


<!-- more start -->

## Voraussetzungen

Zwar werden Neuerungen und Änderungen in der Sprache von Grund auf erläutert und
Vergleiche zu Scala 2 ausführlich erklärt, dennoch unterstützt die Kenntniss von
Scala 2 das Verständnis dieses Blogposts.


## Weniger Klammern aber strikte Einrückung

Man kennt es bereits von Python oder Haskell: Code, der explizite Einrückung
fordert, welche Auswirkungen auf die Gültigkeit und Bedeutung haben. Die einen
lieben diesen Ansatz, da er visuell gleichmäßigeren und rauschärmeren Code zur
Folge hat. Andere vermissen die Freiheiten, die sie durch explizite Klammerung
erhalten und verlieren sich beim Zählen tiefer Einrückungen.

Es ist nicht verwunderlich, dass die neue Syntax in Scala 3 bei ihrer Konzeption
und Einführung kontrovers diskutiert wurde. So hat etwa eine
[Diskussion](https://contributors.scala-lang.org/t/feedback-sought-optional-braces/4702)
für Feedback zu dieser Neuerung 579 Beiträge und es gibt dort hitzige
Diskussionen zu entdecken. 

Scala 3 setzt diesem Feature noch die Krone auf und macht es optional.  Das
bedeutet nicht nur, dass die (geschweifte) klammerfreie Syntax an und aus
geschaltet werden kann. Beide Syntaxvarianten können parallel verwendet und
miteinander vermischt werden. Dass hier die Befürchtung aufkommt, dass eine
fragmentierte Syntaxlandschaft entsteht, ist durchaus nachvollziehbar.


## Geschweifte Klammern sind optional

Betrachten wir die folgende kleine Scala-Klasse und deren Companion-Object.

{% highlight scala %}

case class Bottle(content: String, volume: Float, currentVolume: Float){

  def fill(additionalVolume: Float) : Bottle = {
    val maybeNewVolume = volume + additionalVolume 
    if(maybeNewVolume > volume){
      this.copy(currentVolume = maybeNewVolume) 
    } else {
      this.copy(currentVolume = maybeNewVolume) 
    }
  }

  def empty() : Bottle = {
    this.copy(currentVolume = 0f)
  }

  def isEmpty() : Boolean = {
    this.volume == 0f
  }
}


object Bottle{
   def emptyMany(bottles : List[Bottle]) : List[Bottle] = {
     bottles.map{ x => x.empty() }
   }

   def fillMany(bottles : List[Bottle], volume: Float) : List[Bottle] = {
     for{
       bottle <- bottles
     } yield bottle.fill(volume))
   }
}

{% endhighlight %}

Diese Syntax kompiliert sowohl für Scala 2 als auch für Scala 3. In Scala 3
können wir die Klasse mit der neuen Syntaxvariante auch wie folgt beschreiben:

{% highlight scala %}

case class Bottle(content: String, volume: Float, currentVolume: Float):

  def fill(additionalVolume: Float) : Bottle = 
    val maybeNewVolume = volume + additionalVolume 
    if maybeNewVolume > volume then
      this.copy(currentVolume = maybeNewVolume) 
    else
      this.copy(currentVolume = maybeNewVolume) 

  def empty() : Bottle = 
    this.copy(currentVolume = 0f)

  def isEmpty() : Boolean = 
    this.volume == 0f


object Bottle:
   def emptyMany(bottles : List[Bottle]) : List[Bottle] = 
     import language.experimental.fewerBraces
     bottles.map:
       x => 
         x.empty()

   def fillMany(bottles : List[Bottle], volume: Float) : List[Bottle] =
     for bottle <- bottles 
       yield bottle.fill(volume)


{% endhighlight %}

Zwar haben die beiden Code-Beispiele viele Ähnlichkeiten, jedoch sind die
Unterschiede visuell sofort deutlich. Die Scala 3 "quiet Variante" kommt ohne
geschweifte Klammern aus.  Bei der Übersetzung passieren viele Dinge, die wir
nun im Detail betrachen werden. 

## Template-Bodies

Template-Bodies sind diejenigen Coderegionen, die beispielsweise den Rümpfen von
Klassen-, Trait- und Objektdefinitionen folgen. In Scala 2 waren diese von
geschweiften Klammer eingeschlossen, jetzt reicht ein Doppelpunkt nach dem
Rumpf, wie im Beispiel in der Case-Class- und Objektdefinition zu sehen ist. Die folgende
Einrückung macht die Zugehörigkeit zur Definition deutlich. 

## Einrückungen

Einrückungen bestimmen, wie die Zugehörigkeit von nachfolgenden Codezeilen zu einem
Template-Body, aber auch einer Funktion, eines Zweiges oder einer Schleife
bestimmt wird.  Wir sehen dies in den Funktionsdefinitionen im Codebeispiel.
Zudem finden wir in allen Kontrollstrukturen Code-Einrückungen, die
Codezugehörigkeit definieren.  Einrückungen in Scala 3 können sowohl durch
Leerzeichen als auch Tabs beschrieben werden. Dabei können Leerzeichen und Tabs
gemischt verwendet werden. Vier Tabs und zwei
Leerzeichen zählen weniger als vier Tabs und drei Leerzeichen. Vier Tabs und zwei
Leerzeichen sind äquivalent zu sechs Leerzeichen. Mancher kennt diese Möglichkeit 
der Kombination von Leerzeichen und Tabs zur Einrückung eventuell [aus
Haskell](https://www.youtube.com/watch?v=uKpPJV0hhCY). Die
[Scala-Dokumentation](https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#spaces-vs-tabs)
selbst
erwähnt, dass das Mischen der beiden nicht praktikabel ist und vermieden werden
sollte.

Wem bei langen Regionen die Einrückung zu unübersichtlich ist, weil man das Ende
der Region schwer identifizieren kann, der kann eine Ende-Markierung setzen.
Dies geht mit dem Schlüsselwort `end` gefolgt von z.B. dem Identifier des
Blocks. Um die Definition der Klasse Bottle aus dem Code-Beispiel abzuschließen,
würden wir vor der Objektdefinition ohne Einrückung ein `end Bottle` einfügen.
Die genauen Regeln können
[hier](https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#the-end-marker)
nachgelesen werden.

## Kontrollstrukturen

Im obigen Beispiel sieht man im _Scala 3_-Code ein neues Schlüsselwort: `then`.
Dieses Schlüsselwort ist nötig, um eine If-Anweisung klammerfrei eindeutig zu
formulieren. Von diesen Schlüsselwörtern gibt es noch mehr:

* die Bedingung einer while-Schleifen wird von einem `do` abgeschlossen
* der Rumpf einer for-Schleife kann sowohl von `yield` als auch `do`
  abgeschlossen werden. `yield` funktioniert wie bisher, beschreibt also eine
  monadische Anwendung, `do` hingegen definiert eine klassische for-Schleife
  ohne Rückgabewert (bzw. `unit`), wie im folgendes Beispiel skizziert:


{% highlight scala %}

// for-Schleife als monadische Anweisung 
for a <- List(1, 2, 3) 
   yield a * 2

// for-Schleife als imperative for-Schleife mit Rückgabewert unit
for a <- List(1, 2, 3) do
   val twiceA = a * 2
   println(twiceA)
   

{% endhighlight %}



## Noch weniger Klammern

Im übersetzten Code sieht man im Companion-Object in der Methode `emptyMany`
den Import des Packages `fewerBraces`. Dieses Feature ist ausgelagert, da es im
Vorfeld noch kontroverser diskutiert wurde als die Syntaxumstellung ohnehin
schon.

In Scala zwei gibt es die Möglichkeit, Statement-Blöcke und Definitionen von
anonymen Funktionen in geschweiften Klammer vorzunehmen, wie im Code-Beispiel
für Scala 2 in der `emptyMany`-Methode zu sehen. Mit dem Import von
`fewerBraces` in der _Scala 3_-Variante dieser Methode können alle durch
geschweifte Klammern definierten Blöcke von nun an mit einem Doppelpunkt
eingeleitet und durch Einrückungen vom Rest abgegrenzt werden. Wird
dieser Import nicht verwendet gibt es keine Möglichkeit die Definition der
`empty-many`-Methode ohne geschweifte Klammern zu beschreiben.


## Compiler, to the rescue!

Der _Scala 3_-Compiler kommt mit einer Handvoll Werkzeuge, um mit der neuen Syntax
umzugehen. Der Compiler warnt den Benutzer, wenn Einrückungen inkonsistent sind,
insbesondere, wenn die Klammersyntax verwendet wurde. Wer dieses Verhalten
unterbinden und die klammerfreie Syntax abschalten möchte, kann dem
Compiler die Flag `-no-indent` mitgeben.

Um _Scala 2_-Programme in neuer Syntax erstrahlen zu lassen, kann man den Compiler
anweisen, diese umzuschreiben. Ausgeführt mit den Flags `-rewrite` und `-indent`
ersetzt der Compiler wo möglich Klammern durch Einrückungen. Da dies nur in
Verbindung mit der neuen Kontrollstrukturensyntax funktioniert, muss vorher
ein Lauf mit `-rewrite -new-syntax` durchgeführt werden.


## Fazit

Scala 3 ist nach langer Arbeit erschienen und das ist auch gut so. Allerdings
haben wir die Reise in Scala 3 mit einem wenig motivierenden Thema begonnen, der
optionalen neuen Syntax. Zwar ist die Syntax an sich nicht schlecht, doch Scala
bietet wieder alle Möglichkeiten gleichzeitig an: die neue Syntax verwenden oder
bei der alten bleiben? Leerzeichen verwenden oder Tabs oder beides? Optionale Features
zuschalten? 

Dies wird nicht für mehr Uniformität im Code führen sondern ohne
strikten Formatierer viele Ausprägungen von _Scala 3_-Code hervorrufen, bis sich ein
Quasistandard etabliert hat. Allerdings ist dies sehr unwahrscheinlich
hinsichtlich der Kontroverse, zu der dieses Thema bereits im Vorfeld geführt
hat.

In den folgenden Blogposts zu Scala 3 werden wir sinnvolle und spannende
Features diskutieren, wie etwa Enums, Intersection- und Union-Types
und die neuen implicits.
