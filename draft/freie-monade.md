---
layout: post
description: "Freie Monaden selbst gemacht"
title: "Uhrwerk Freie Monade: Hinter den Kulissen" 
author: simon-haerer
tags: ["Praxis", "freie", "Monade", "Scala"]
---

Im Artikel ["Freie Monaden oder: Wie ich lernte, die Unabhängigkeit zu lieben"](https://funktionale-programmierung.de/2018/01/22/freie-monade.html) wurde gezeigt,
wie freie Monaden eingesetzt werden können, um die Ausführung eines Programmes von dessen Beschreibung zu entkoppeln. In diesem Folgeartikel werden wir in einfachem Scala die freie Monade von Grund auf selbst implementieren, um deren Funktionsweise besser zu verstehen. Dabei wird erneut deutlich, dass die Formulierung von Operationen als Daten dabei hilft, über Probleme strukturiert nachdenken zu können.

<!-- more start -->

## Voraussetzungen

Dieser Artikel beschreibt die Implementierung der freien Monade in [Scala](https://www.scala-lang.org). Daher wird eine gewisse Grundkenntnis in Scala vorausgesetzt. Zudem sollte der [vorausgegangene Artikel](https://funktionale-programmierung.de/2018/01/22/freie-monade.html) über freie Monaden gelesen und verstanden worden sein. Zusätzlich ist gute Kenntnis von _Monaden_ für das Verständnis erforderlich. 


## Monaden in Scala

Was in Haskell das monadische `do` ist, ist in Scala `for`. Der for-Ausdruck ermöglicht es, monadische Operationen eleganter und verständlicher zu beschreiben. Betrachten wir ein Beispiel mit einer Option-Monade:
 
{% highlight scala %}

val res : Option[Int] = for {
  a <- Some(3)
  b <- Some(10)
} yield (a + b)

{% endhighlight %}

Der Option-Typ hat zwei Subtypen, `Some` und `None`. Er ist das Äquivalent zu Haskells `Maybe`. Der obige for-Ausdruck ist syntaktischer Zucker für folgende Berechnung:
 
{% highlight scala %}

val res : Option[Int] = Some(3).flatMap(a => Some(10).map(b => a + b))

{% endhighlight %}

Um in Scala eine Monade vom Typ `M[A]` zu implementieren, benötigen wir also eine Funktion `flatMap` mit der Signatur `def flatMap[B](f: A => M[B]) : M[B]` und eine Funktion `map` mit der Signatur `def map[B](f: A => B) : M[B]`. Dabei wird davon ausgegangen, dass sich die Funktionsdefinitionen in der Klasse selbst befinden und somit wird die explizite Nennung des Parameters vom Typ `M[A]` bei beiden Funktionen übersprungen. Dieses Wissen über die Grundfunktionen einer Monade in Scala wird im Folgenden verwendet, um die freie Monade mit einem einfachen Trick zu implementieren.

## Operationen als Daten

Im vorherigen Artikel über freie Monaden haben wir einen Trick kennengelernt: Beim Erstellen der Algebra haben wir Operationen von Funktionen in Daten übersetzt, um diese nicht direkt auszuführen. Dabei ist folgende Domain Specific Language (DSL) für ein Adressbuch entstanden, die wir in diesem Artikel weiterhin verwenden:

{% highlight scala %}

sealed trait AddressBookOp[A]

case class Put(address: Address) extends AddressBookOp[Unit]
case class Get(id : Long) extends AddressBookOp[Option[Address]]
case class Delete(address : Address) extends AddressBookOp[Unit]
case class Filter(foo : Address => Boolean) extends AddressBookOp[List[Address]]
    
{% endhighlight %}

Jedoch ist hier nicht Schluss! Dieses Prinzip lässt sich auf einer zusätzlichen Ebene erneut anwenden. Wir wissen, dass die Algebra-Operationen, die mit Hilfe der freien Monade komponiert werden, später ausgeführt und mittels natürlicher Transformation sogar in eine andere Monade überführt werden können. Dazu müssen wir allerdings die strukturelle Information über die Zusammensetzung des monadischen Programms erhalten. Und dies wird wiederrum durch die Formulierung der monadischen Funktionen als Daten erreicht.

Nun folgt ein kleiner Exkurs in die Kategorientheorie. Eine besondere Form von Monoiden sind [freie Monoiden](https://en.wikipedia.org/wiki/Free_monoid). Im Gegensatz zu Monoiden, lässt sich die Abfolge der monoidischen Operationen jederzeit rekonstruieren. Formal bedeutet dies, das für einen freien Monoid stets einen Isomophismus zu einem beliebigen anderen Monoiden existiert. Konstruiert man nun einen Funktor auf Basis eines freien Monoiden, ist dies ein freier Funktor und letztendlich lässt sich mit diesen Grundlagen die freie Monade definieren. Eine Erklärung anhand von Scala-Code ist [hier](http://blog.higher-order.com/blog/2013/08/20/free-monads-and-free-monoids/) zu finden.

Das war nun etwas abstrakt. Wer dies nicht verstanden hat, kann trotzdem weiterlesen, denn es genügt folgende Erkenntnis: Um die monadische Komposition der Algebra später nachvollziehen zu können, müssen wir die Daten und deren Komposition derart konstruieren, dass die Struktur der angewandten monadischen Funktionen wiederhergestellt werden kann. Wir erinnern uns, diese haben die Signaturen
`def flatMap[B](f: A => M[B]) : M[B]` und `def map[B](f: A => B) : M[B]`. Insbesondere müssen wir die Klasse, die die beiden Funktionen beinhaltet, explizit als Parameter (`param`) festhalten:

{% highlight scala %}

sealed trait Free[F[_], A]

case class Map[F[_], I, A](param : I, continuation: I => A) extends Free[F, A] 

case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A])
                                                            extends Free[F,A]

{% endhighlight %}

Der algebraische Datentyp `Free` hat zwei Typparameter: einen Container-Typen `F[_]` (zum Beispiel `AddressBookOp`), der unsere Algebra repräsentiert, und einen Ergebnistypen `A`&mdash;dazu später mehr. Die Typen für die Funktionen `map` und `flatMap` lassen sich geradewegs aus den Definitionen der dazugehörigen Funktionen ableiten und erweitern `Free`. Beide Typen werden mit einem Parameter `param` und einer Funktion `continuation` konstruiert, wobei `param` jeweils der Eingabeparameter der `continuation` ist. Der Typ `I` des `params` wird aus dem übergebenen Wert inferiert. Natürlich muss die übergebene _continuation_ einen Wert dieses Typs als Eingabeparameter entgegennehmen. Konstruieren wir zum Beispiel ein `Map` mit einem `param` vom Typ `Int`, benötigen wir auch eine `continuation`, die Werte vom Typ `Int` auf einen Zielwert abbildet. Anhand dieser aufgeschobenen Auführung, können wir die Struktur der monadischen Funktionen derart abbilden, dass sie später rekonstruiert werden kann. Wir können bereits den entfalteten for-Ausdruck aus dem Option-Monaden-Beispiel zu beschreiben:


{% highlight scala %}

FlatMap(Some(3), a => Map(Some(10), b => a + b))

{% endhighlight %}

Allerdings ist die Beschreibung auf diese Weise sehr mühsam. Es wäre einfacher, diese Beschreibung wieder rum mit Hilfe eines for-Ausdrucks formulieren zu können. 

## Meta-Monadisch

Jetzt nicht den Kopf verlieren: Von nun an ist es wichtig, zwischen dem Typ `FlatMap` und der Funktion `flatMap` aufmerksam zu unterscheiden (respektive `Map`). Um `Free` monadisch kombinieren zu können, muss es nämlich die Funktionen `map` und `flatMap` implementieren:

{% highlight scala %}

sealed trait Free[F[_], A]{

  def flatMap[B](f : A => Free[F, B]) : Free[F,B]

  def map[B](f: A => B): Free[F,B]}
}

{% endhighlight %}

Auch diese Funktionssignaturen sind aus den bereits oben beschriebenen generischen Signaturen der jeweiligen Funktionen einfach abzuleiten. Lediglich der Typparameter `F` muss zusätzlich unverändert mitgeführt werden; die zugrunde liegende Algebra bleibt stets vom Typ `F[_]`. `A` und `B` beschreiben dabei den gekapselten Typ, der später bei der Interpretation unserer Algebra-Operationen zurückgegeben wird. Beispielsweise gibt `Get` der Adressbuch-Algebra eine `Option[Address]` zurück. Die jeweiligen Funktionen (hier mit `continuation` bezeichnet) überführen diese Rückgabewerte dementsprechend in den Bildraum der übergebenen Funktionen. 

Doch wie sieht die Implementierung dieser Funktionen aus? Betrachten wir zunächst `flatMap`:

{% highlight scala %}


case class Map[F[_], I, A](param : I, continuation: I => A) extends Free[F, A] {
  def flatMap[B](f : A => Free[F, B]) : Free[F,B] =
        (continuation andThen f)(param)
  ...
}

case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A])...{
  def flatMap[B](f: A => Free[F, B]) = 
        FlatMap(param, continuation andThen (x => x.flatMap(f)))
  ...
}
{% endhighlight %}

Wird `flatMap` auf einen Wert vom Typ `Map` angewandt, wird zunächst die `continuation` aus `Map` angewandt und dann die Funktion `f`. Hier lassen wir uns durch das Typsystem führen: `continuation` hat die Typsignatur `I => A`, während `f` von `A` nach `Free[F, B]` abbildet. Durch die Komposition beider wird also der in `Map` enthaltene Parameter vom Typ `I` nach `Free[F, B]` überführt.

Wird `flatMap` auf einen Wert vom Typ `FlatMap` angewandt, wird ein neuer Wert vom Typ `FlatMap` mit unverändertem Parameter konstruiert. Die dazugehörige Funktion ist eine Verkettung aus der `continuation` und einem selbstrekursiven Aufruf der Funktion `flatMap` mit der übergebenen Funktion `f` als Parameter.

Konzeptuell arbeiten wir also mit Funktionskompositionen, um den Eingabeparameter später in den gewünschten Typen zu übersetzen.

Betrachten wir nun die Implementierung von `map`:

{% highlight scala %}

 case class Map[F[_], I, A](param : I, continuation: I => A) extends Free[F, A] {
   ...
   def map[B](f: A => B): Free[F,B] = Map(param, continuation andThen f)
 }

 case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A])...{
   ...
   def map[B](f: A => B): Free[F, B] = flatMap(x => Map(x, f))
 }

{% endhighlight %}

Wird `map` auf `Map` angewandt, ist das Vorgehen wieder denkbar einfach. Es wird ein neues `Map` konstruiert und beide Funktionen komponiert. 

Wird `flatMap` auf `Map` angewandt, spiegeln wir genau dies im Quelltext wieder: Es wird `flatMap` aufgerufen mit einer Funktion, die anhand des Eingabeparameters das gewünschte `Map` mit `f` als `continuation` erzeugt. `Map` wird sozusagen im `FlatMap` verpackt.

## Die ersten Schritte

Es ist jetzt möglich, Werte vom Typ `Free` mithilfe eines for-Ausdrucks zu kombinieren. Doch wie verbinden wir dies mit unserer Addressbuch-Algebra? Im vorherigen Artikel wurde die Funktion `liftF` der _cats_-Bibliothek verwendet, um die Algebra in die Monade zu heben. Eine Funktion, die Container-Typen in `Free` hebt, sieht wie folgt aus:

{% highlight scala %}
  def liftF[F[_], A](operation: F[A]): Free[F, A] =
         FlatMap(operation, (a : A) => Map(a, identity[A]))
{% endhighlight %}


Dabei wird die übergebene Operation mit Rückgabetyp `A` mithilfe von `FlatMap` und `Map` in einen Wert vom Typ `Free` übersetzt. Der Rückgabewert wird dabei einfach durch die Identitätsfunktion zurückgegeben. Unter Verwendung dieser Funktionen können die Operationen unserer Algebra in die freie Monade gehoben werden. Dazu definieren wir uns noch einen zusätzlichen Typ, um über den Summentypen `AddressBookOp` im `Free`-Container abstrahieren zu können. Im Anschluss werden die aus dem vorherigen Artikel bekannten _smart-constructors_ definiert:

{% highlight scala %}

  type AddressBookDSL[A] = Free[AddressBookOp, A]

  def put(address: Address) : AddressBookOpDSL[Unit] =
               liftF[AddressBookOp, Unit](Put(address))
  def get(id: Long) : AddressBookOpDSL[Option[Address]] =
               liftF[AddressBookOp, Option[Address]](Get(id))
  def delete(address: Address) : AddressBookOpDSL[Unit] = 
               liftF[AddressBookOp, Unit](Delete(address))
  def filter(foo: Address => Boolean) : AddressBookOpDSL[List[Address]] = 
               liftF[AddressBookOp, List[Address]](Filter(foo))

{% endhighlight %}

Mit Hilfe dieser smart-constructors können in for-Ausdrücken bereits Programme kombiniert werden. Wir erinnern uns: Die Beschreibung von Programmen ist unabhängig von deren Ausführung.


{% highlight scala %}

  val updateEntry: Address => AddressBookOpDSL[Unit] = address => for {
    _ <- delete(address) // delete old address
    _ <- put(address)
  } yield ()

{% endhighlight %}


## Das Uhrwerk anstoßen

Programme der freien Monade bestehen aus Werten vom Typ `Free`, die jeweils einen Parameter und eine Funktion (`continuation`) beinhalten. Um ein Programm abzuspielen, muss nun lediglich der Parameter, also unser Algebra-Kommando, ausgewertet und die `continuation` mit diesem Wert aufgerufen werden. Die Auswertung des Parameters übernimmt ein Interpreter. Dieser bekommt ein Kommando unserer DSL und evaluiert dieses. Die Idee bei freien Monaden ist, diesen Interpreter austauschbar zu implementieren. Daher wird zuerst ein simples _trait_ implementiert und im Anschluss eine explizite Implementierung unseres Addressbuch-Interpreters beschrieben, der dieses _trait_ implementiert. Zu Gunsten des Umfangs definieren wir diesen Interpreter auf Basis einer mutierbaren Variable (nicht zuhause nachmachen):


{% highlight scala %}

  trait Interpreter[F[_]] {
    def apply[A](program : F[A]) : A
  }

  val addressBookInterpreter = new Interpreter[AddressBookOp] {
    var m = scala.collection.Map[Long, Address]()
    def apply[A](program: AddressBookOp[A]): A = program match {
      case Put(address) =>
        println("Putting address with id: " + address.id)
        m = m + (address.id -> address)
      case Get(id) =>
        println("Getting address with id: " + id)
        m.get(id)
      case Delete(address) =>
        println("Deleting address with id: " + address.id)
        m = m - address.id
      case Filter(foo) => m.values.filter(foo).toList
    }
  }

{% endhighlight %}

Die letztendliche Ausführung des Programmes ist unter Zuhilfenahme eines Interpreters trivial. Ein Kommando wird ausgewertet und die `continuation` damit aufgerufen. `run` wird mit dem Ergebnis so lange rekursiv aufgerufen, bis wir auf einen Wert vom Typ `Map` treffen.

{% highlight scala %}


  // generische run-Funktion

  def run[F[_], A](program: Free[F, A], interpreter: Interpreter[F]): A = 
    program match {
      case FlatMap(param, continuation) =>
          val ret = interpreter(param)
          run(continuation(ret), interpreter)
      case Map(param, continuation) => 
          continuation(param)
    }



  // Beispiel

  val newAddress = Address(1, "Santa Clause", "Snow Valley", "Santa's Town", 
                           "0", "Santa Deliveries")

  updateEntryProgram(updateEntry(newAddress), addressBookInterpreter)

  /* Deleting address with id: 1
   * Putting address with id: 1
   *
   * $ addressBookInterpreter.m
   * Map(1 -> Address(1, "Santa Clause", ...))
   */

{% endhighlight %}



## Résumé

Freie Monaden sind nicht kompliziert. Implementiert man sie, indem man über bereits bekannte Konzepte anhand geschickt gewählter Datenstrukturen abstrahiert, stellt man sogar das Gegenteil fest. In diesem Artikel haben wir mit wenigen Zeilen Code die Beispiele aus dem ersten Artikel zu freien Monaden nachimplementiert. Statt einem Interpreter könnten wir ohne Probleme eine natürliche Transformation der Form `F[_] => G[_]` implementieren, um wie im ersten Teil auf die State-Monade abzubilden. Warum also eine Bibliothek verwenden? Insbesondere, wenn man in einem nächsten Schritt beispielsweise mehrere Algebren kombinieren möchte, wird es haarig. Dann sind wir froh, wenn wir eine Bibliothek zur Hand haben, die über fundamentale Konzepte abstrahiert und somit die nötigen Hilfsmittel bereits ausliefert&mdash;dazu in einem zukünftigen Artikel mehr.

Der hier entwickelte Source Code ist auf [GitHub zu finden](https://github.com/smoes/blogpost-free-2).


## Nachwort: Terminologie in Haskell

Um den monadischen for-Ausdruck in Scala verwenden zu können, mussten zwei Funktionen implementiert werden, nämlich `flatMap` und `map`. Während `flatMap` das äquivalent zu Haskells `bind` ist, darf das üblicherweise verwendete `return` nicht mit `map` gleichgesetzt werden. `return` nimmt lediglich einen Wert entgegen, während `map` zusätzlich eine Funktion entgegennimmt. `Return` könnte also wie folgt mithilfe der Identitätsfunktion definiert werden:

{% highlight scala %}

object Map {
  def Return[F[_], I](param: I)  = new Map[F[_], I, I](param, identity[I])
}

{% endhighlight %}

Spricht man von freien Monaden wird `bind` beziehungsweise `flatMap` oft als `Suspend` bezeichnet.
