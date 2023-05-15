---
layout: post
description: Monaden in Kotlin
title: "Monaden in Kotlin"
author: michael-sperber
tags: ["Kotlin", "monad", "Monade", "continuation", "Continuation", "DSL"]
---

Dieser Post ist ein Teil der Reihe über *funktionale
Softwarearchitektur in Kotlin*.  Im ersten ging es um [funktionale
Validierung](https://funktionale-programmierung.de/2023/01/19/kotlin-validation.html),
in diesem Teil geht es um *Monaden*.  Diese sind in Kotlin vor allem
praktisch, wenn es um die Beschreibung von Abläufen in der Domäne
geht, die von technischer Logik zur Ausführung dieser Abläufe getrennt
werden soll – und zwar unter Verwendung von kleinen
domänenspezifischen Sprachen (DSLs).

In dieser Folge geht es darum, wie Monaden überhaupt funktionieren.
Kotlin hat nämlich – wie viele funktionale Sprachen auch – eine
spezielle Syntax dafür, auch wenn man sie nicht unter dem "M-Wort" in
der Dokumentation findet.  Sie versteckt sich hinter dem Schlüsselwort
`suspend`.

<!-- more start -->

## Der `Option`-Typ

Wir versuchen es mit einer der einfachsten Monaden, nämlich dem Typ
`Option`, aus getypten funktionalen Sprachen auch gelegentlich als
[`Maybe`](https://wiki.haskell.org/Maybe) bekannt, in Java als
[`Optional`](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html).
Er ist dafür zuständig, wenn eine Operationen manchmal ein Ergebnis
liefert, manchmal aber auch nicht.

Wir bauen den Typ einfach selber, damit wir zeigen können, wie wir auf
Grundlage einer Typdefinition die dazu passende Monade definieren
können.  Wir bauen ihn als kleine Typhierarchie mit einem Interface
und den obligatorischen zwei Fällen – `Some`, falls ein Wert da ist und
`None`, falls nicht:

```kotlin
sealed interface Option<out A> {
    companion object {
        fun <A> some(value: A): Option<A> = Some(value)
        fun <A> none(): Option<A> = None

        fun <A> Option<A>.get(): A =
            when (this) {
                is None -> throw AssertionError("found None where Some was expected")
                is Some -> value
            }
    }
}

object None : Option<Nothing>
data class Some<out A>(val value: A) : Option<A>
```

(Wer `out` und `Nothing` in Kotlin noch nicht gesehen hat: Nicht
schlimm, die klären die Interaktion zwischen Generics und
Typhierarchien, haben aber keine besondere inhaltliche Signifikanz.)

Um `Option` zu einer Monade zu machen, brauchen wir eine
[`bind`-Methode](https://de.wikipedia.org/wiki/Monade_(Informatik)) mit folgender Signatur:

```kotlin
sealed interface Option<out A> {
    fun <B> bind(next: (A) -> Option<B>): Option<B>
}
```

Die implementieren wir folgendermaßen für `None` und `Some`:

```kotlin
object None : Option<Nothing> {
    override fun <B> bind(next: (Nothing) -> Option<B>): Option<B> = None
}
data class Some<out A>(val value: A) : Option<A> {
    override fun <B> bind(next: (A) -> Option<B>): Option<B> = next(this.value)
}
```

Das `bind` können wir nun benutzen, um eine Art "Poor Man's Exception
Handling"  zu implementieren, also viele Operationen hintereinander,
die jeweils schiefgehen (also `None` liefern) können, und wo ein
Endergebnis nur herauskommt, wenn alles gutgeht.  Das macht leider
syntaktisch keine Freude, noch nicht mal eingefleischten
Klammer-Fans wie mir:

```kotlin
some(5).bind { o1 ->
some(7).bind { o2 ->
  some(o1 + o2) } }
```

Das ist effektiv [Callback Hell](http://callbackhell.com/), und auch
wenn wir eine Club-Mate-Flatrate drauflegen, werden wir damit
OO-Programmierys kaum hinterm Ofen hervorlocken können, um
routinemäßig monadisch zu programmieren.

In [Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation),
[Scala](https://docs.scala-lang.org/tour/for-comprehensions.html) und
[F#](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)
beispielsweise ist jeweils syntaktischer Zucker eingebaut, mit dem wir
Programme als Folge von "Statements" schreiben können, den der
Compiler dann in Aufrufe von `bind` und verwandten Funktionen
übersetzt.  In Haskell zum Beispiel sähe der Code so aus:

```haskell
do o1 <- Just 5
   o2 <- Just 7
   return (o1+o2)
```

(In Clojure zum Beispiel können wir solchen [syntaktischen Zucker auch
selbst
definieren](https://funktionale-programmierung.de/2023/04/27/clojure-monads.html).)

## Coroutinen und Continuations

So schicke Syntax wie in Haskell gibt es in Kotlin auch, aber
versteckt in einem Mechanismus, der
[Coroutinen](https://kotlinlang.org/docs/coroutines-guide.html).  Die
Dokumentation lässt das Lesy glauben, dass es bei Coroutinen primär um
"asynchrone" beziehungsweise nebenläufige Programmierung geht, aber
dahinter steckt ein allgemeiner Mechanismus, der bei Funktionen
aktiviert ist, deren Definition mit dem Schlüsselwort `suspend`
markiert ist.  Dieses versetzt den Compiler in einen anderen Modus, der
daraufhin eine sogenannte
[CPS-Transformation](https://de.wikipedia.org/wiki/Continuation-Passing_Style)
durchführt.

"CPS" steht für *Continuation-Passing Style* und ist eine bestimmte
Art, Funktionen zu schreiben.  Normalerweise schreiben wir ja
Programme "verschachtelt", indem das Ergebnis eines Funktionsaufrufs
zurückgegeben wird.

```kotlin
f(g(h(x)))
```

Bei CPS geht es niemals zurück: Wenn eine Funktion fertig ist, gibt
sie kein Ergebnis "zurück", sondern ruft stattdessen eine Funktion
auf, die weitermacht – eben die Continuation, englisch für
"Fortsetzung".   In CPS sieht der obige geschachtelte Funktionsaufruf
so aus:

```kotlin
h(x) { g(it) { gr -> f(it) { ... } } }
```

Das Programm wird also linearisiert – die Funktionsaufrufe stehen in
der Reihenfolge, in der sie auch zur Laufzeit passieren.  Außerdem
bekommt jedes Zwischenergebnis einen Namen.  Das und die geschweiften
Klammern hat schonmal gewisse Ähnlichkeit zu den Aufrufen von `bind`
weiter oben.

Warum macht der Kotlin-Compiler also eine CPS-Transformation?
Motiviert ist dies tatsächlich durch "asynchrone Programmierung", wo
es darum geht zu vermeiden, dass ein JVM-Thread blockiert, weil er zum
Beispiel auf I/O wartet.  Warum das vermeiden?  Weil JVM-Threads viel
Speicher verbrauchen und eine JVM nur eine begrenzte Anzahl davon
erzeugen darf.  Es wäre also besser, wenn ein Thread erstmal was
anderes machen könnte, wenn eine Berechnung blockieren würde und sie
dann reaktiviert, wenn das I/O fertig ist.  Dafür müsste das Programm
speichern, wo es weitergeht.

Normalerweise weiß die JVM, wo es nach einem
Methodenaufruf weitergeht, indem sie den *Stack* konsultiert, ein
implizites Ding, welches ein Java-Programm nicht direkt manipulieren
kann.  In CPS jedoch ist "wie es nach einem Methodenaufruf weitergeht"
eine Continuation, also ein *Objekt*, das gespeichert und benutzt
werden kann, um eine Berechnung zu reaktivieren.

Um damit asynchron zu programmieren, braucht das Programm Zugriff auf
jenes Continuation-Objekt, und dafür gibt es eine Funktion namens
[`suspendCoroutine`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.coroutines/suspend-coroutine.html),
die eine übergebene Funktion aufruft mit eben der aktuellen
Continuation, für die es eine
[Extra-Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.coroutines/-continuation/)
in Kotlin gibt.  Die Continuation wiederum hat eine Methode `resume`,
welche die Ausführung fortführt, bis sie wieder `suspendCoroutine`
aufruft.  Das können wir nutzen, um einen Aufruf von `bind`
einzuschmuggeln und so aus einer "ganz normalen" `suspend`-Funktion
ein monadisches Programm zu machen.

Das ist eine ziemliche Frickelei, aber es geht -
[hier](https://github.com/active-group/kotlin-free-monad/blob/main/src/main/kotlin/de/activegroup/FreeMonad.kt)

404 File not found

ist der Code in unserer kleinen Library, die wir für den Zweck
geschrieben haben.  Sie stellt ein Objekt `MonaDSL` zur Verfügung mit
einigen hilfreichen Funktionen, deren Definition wir aber gerade wegen
dieser Frickeligkeit hier weglassen.

## Monaden-Syntax als Kotlin-DSL

Die Abstraktionen aus unserer Library können wir benutzen, um zunächst
aus einem `Option<A>`-Wert eine Berechnung zu
machen, die wir in einer `suspend`-Funktion benutzen können:

```kotlin
sealed interface Option<out A> {
    suspend fun susp(): A = MonadDSL.susp<Option<A>, A>(this::bind)
}
```

Diese Methode benutzen wir nun, um eine kleine DSL zu definieren.
"DSL" heißt in Kotlin in der Regel, dass wir für einen Lambda-Ausdruck
einen Satz Funktionen lokal zur Verfügung stellen, indem wir einen
[Funktionstyp "with receiver
type"](https://kotlinlang.org/docs/lambdas.html#function-types)
definieren.  So sieht das aus:

```kotlin
fun <A> optionally (block: suspend OptionDSL.() -> A): Option<A> =
	MonadDSL.effect(OptionDSL(), block)
```

Beim Typ `OptionDSL.() -> A` ist `OptionDSL` ein solcher "receiver
type" und er sorgt dafür, dass der Kotlin-Compiler jedem
Lambda-Ausdruck, der an `optionally` übergeben wird, automatisch aus
von der Klasse `OptionDSL` erbt, also dessen Funktionen benutzen
kann.  Außerdem bekommen sie automatisch ein `suspend` angehängt und
werden damit vom Kotlin-Compiler CPS-transformiert.

Diese Klasse `OptionDSL` enthält nur eine einzige Operation:

```kotlin
class OptionDSL() {
    suspend fun  <A> pure(result: A): A = MonadDSL.pure(result) { Some(it) }
}
```

Größere Monaden haben hier natürlich mehr Operationen.  Dies erlaubt
uns nun, Programme so zu schreiben:

```kotlin
optionally {
  val o1 = pure(5)
  val o2 = pure(7)
  pure(o1 + o2)
}
```

Herauskommt `Some(12)` – und fertig ist die `Option`-Monade inklusive
schöner Syntax!

Um größere Monaden und welche Rolle sie in der Software-Architektur
spielen können, geht es in einem zukünftigen Post.
