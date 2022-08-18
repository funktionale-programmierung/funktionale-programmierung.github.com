---
layout: post
description: Typ-Lambdas in Scala 3
title: "Scala 3: Typ-Lambdas"
author: michael-sperber
tags: ["Scala", "types", "type"]
---

Nach 8 Jahren, 28000 Commits und 7400 Pull-Requests war es am 14. Mai
2021 endlich so weit: Scala 3 wurde veröffentlicht. Neben dem neuen
Compiler „Dotty“ haben es eine neue Syntax sowie einige Neuerungen an
der Sprache in Scala 3 geschafft. In diesem Blogpost der Serie über
interessante Neuerungen werden wir über Typ-Lambdas sprechen.

<!-- more start -->

## Weitere Posts zu Scala 3

- [Scala 3: Scala im neuen Gewand](https://funktionale-programmierung.de/2021/07/13/scala-3-intro.html)
- [Eins für zwei - Scala 3 Enums](https://funktionale-programmierung.de/2021/07/19/scala-3-enum.html)
- [Scala 3: Explizite Implicits](https://funktionale-programmierung.de/2022/02/18/scala-3-implicits.html)
- [Scala 3: Über Vereinigungen und Schnittmengen](https://funktionale-programmierung.de/2022/03/21/scala-unions.html)

## "Higher-Kinded Types" in Scala

Als getypte funktionale Sprache konnten wir in Scala schon immer über
Typen abstrahieren durch den Einsatz von *Typvariablen*, auch
*Generics* genannt. Zum Beispiel gibt es in der Collections-Library
von Scala eine Definition von `List[A]`, die über den Typ der
Listenelemente abstrahiert. Das macht `List` zu einem *Typkonstruktor*
(einer Art Funktion auf Typebene), in Scala häufig auch als `List[_]`
geschrieben, um zu kennzeichnen, dass `List` einen Parameter braucht.

`List[_]` abstrahiert über "einfache" Typen.  Manchmal wollen wir aber
über Typen wie `List[_]` abstrahieren.  Das gilt zum Beispiel bei
*Funktoren*, also Datentypen mit einer `map`-artigen Funktion.  Davon
gibt es ziemlich viele, weswegen es sich lohnt, das Muster in einem
Trait festzuhalten.  Hier ist der erste Versuch:

{% highlight scala %}
trait Functor[F] {
  def map[A, B](x: F[A])(f: A => B): F[B]
}
{% endhighlight %}

Wir wollen dann später diesen Trait instanzieren können mit
Definitionen wie dieser hier, wo für `F` `List` eingesetzt wird:

{% highlight scala %}
def listFunctor = new Functor[List] {
  def map[A, B](x: List[A])(f: A => B): List[B] =
    x.map(f)
} 
{% endhighlight %}

Bei der Definition von `Functor` nörgelt aber der Scala-Compiler: "`F`
does not take type parameters". Wenn bei einem Typparameter nichts
dabeisteht, bedeutet dies, dass der Typparameter so ein "einfacher"
Typ ist, also selbst keine Parameter hat. Das können wir ausdrücken
mit der `[_]`-Notation, so:

{% highlight scala %}
trait Functor[F[_]] {
  def map[A, B](x: F[A])(f: A => B): F[B]
}
{% endhighlight %}

`Functor` und viele andere ähnliche Definitionen sind übrigens in der
unverzichtbaren [Cats-Library](https://typelevel.org/cats/)
vordefiniert.  Wir definieren das hier nur zu didaktischen Zwecken
selbst.

Damit ist `Functor` ein sogenannter "Higher-Kinded Type", also das
Pendant zu einer Funktion höherer Ordnung auf der Werteebene.  (In
diesem Begriff taucht der Begriff "Kind" auf, sozusagen der "Typ eines
Typs", an dessen Form man diese Eigenschaft ablesen könnte.)  Wir
können den Kind-Begriff aber für die Zwecke dieses Blog-Posts
ansonsten ignorieren.  Mehr dazu im gerade erschienen
[Post](https://funktionale-programmierung.de/2022/07/29/higher-kinded-data.html)
meines Kollegen Felix Leitz.

## Wenn's nicht passt, wie passend machen?

`Functor`-Definitionen können wir jetzt ganz einfach für jeden Typ `T`
angeben, der einen Typparameter hat, also die Form `T[_]` hat.  Es
gibt aber Funktoren, die diesen Typ nicht haben, zum Beispiel
[`Either`](https://dotty.epfl.ch/api/scala/util/Either.html).  Das hat
zwei Parameter, und zumindest in Bezug auf einen von denen bildet
`Either` auch einen Funktor, und das wollen wir natürlich auch mit
einer `Functor`-Instanz bekanntgeben.

In Scala 2 ging das so:

{% highlight scala %}
def eitherFunctor[Error] = new Functor[({ type T[A] = 
  Either[Error, A] })#T] {
    def map[A, B](e: Either[Error, A])(f: A => B)
     : Either[Error, B] =
      e match {
        case Left(error) => Left(error)
        case Right(a) => Right(f(a))
      }
  }
{% endhighlight %}

Das entscheidende Stück Code ist das spektakulär hässliche:

{% highlight scala %}
({ type T[A] = Either[Error, A] })#T
{% endhighlight %}

Zur Erinnerung: `Either` ist zweistellig, `Functor` braucht aber eine
einstellige Funktion.  In Scala 2 geht das nur, indem wir einen
passenden einstelligen Typ mit `type ... = ...` definieren.  Die
geschweiften Klammern betten den in eine Umgebung mit Namen ein, aus
der wir das `T` noch mit `#T` rausfrickeln müssen.  (Für den Fall,
dass in den geschweiften Klammern mehrere Typdefinitionen stehen.)
Die runden Klammern sind leider auch notwendig, ohne nörgelt der
Compiler schon wieder.

## Scala 3 to the rescue

Wer will so hässlich leben?  Nicht nur ist das hässlich, sondern auch
unintuitiv: Auf der Werteebene können wir doch anonyme Funktionen
jederzeit konstruieren, warum geht das auf Typebene nicht?  In Scala 3
ist das zum Glück behoben und es gibt "Lambda-Ausdrücke" auf
Typebene.  So sieht das aus:

{% highlight scala %}
def eitherFunctor[Error] = 
  new Functor[[A] =>> Either[Error, A]] { ... }
{% endhighlight %}

Diese sogenannten [*Type
Lambdas*](https://docs.scala-lang.org/scala3/reference/new-types/type-lambdas.html)
machen Higher-Kinded Types schon deutlich angenehmer im Umgang.  

Aber wenn wir schon einmal soweit sind: Auf der Werteebene erlaubt Scala
doch, Lambda-Ausdrücke kompakt mit `_` als Parameter hinzuschreiben,
also zum Beispiel `_ + 1` als Kurzform für `{ x => x + 1 }`.  

In Scala 2 war der Schmerz mit der alten Notation so groß, dass viele
das Compiler-Plugin [Kind
Projector](https://github.com/typelevel/kind-projector) installiert
haben, das genau diese Syntax auch auf Typebene unterstützt.  Dieses
Plugin war so populär, dass auch die Scala-3-Macher Unterstützung für
dessen Syntax eingebaut haben, allerdings [super verwirrend
dokumentiert](https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html).
Da ist die Rede von mindestens drei Syntax-Varianten – auch nach
mehrmaligem Lesen wusste ich nicht, was ich in welcher Version tun
muss, um welche Syntax zu bekommen.

Unser Gastautor [Lars Hupel](https://lars.hupel.info/),
Scala-Rockstar, konnte mir zum Glück erklären, was wir sinnvollerweise
tun sollten, nämlich die Compiler-Option
`-Ykind-projector:underscores` aktivieren.  Also zum Beispiel im
`build.sbt` sagen:

{% highlight scala %}
lazy val root = (project in file("."))
  .settings(
    ...
    scalacOptions ++= Seq(
      ...
      "-Ykind-projector:underscores"
      )
    )
{% endhighlight %}

Alles andere macht nur Kopfschmerzen.  Und Tatsache, dann geht's
ganz einfach:

{% highlight scala %}
def eitherFunctor[Error] =
  new Functor[Either[Error, _]] { ... }
{% endhighlight %}

Super Sache!
