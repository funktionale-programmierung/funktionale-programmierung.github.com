---
layout: post
description: "Freie Monaden selbst gemacht"
title: "Uhrwerk Freie Monade: Hinter den Kulissen" 
author: simon-haerer
tags: ["Praxis", "freie", "Monade", "Scala"]
---

Im Artikel ["Freie Monaden oder: Wie ich lernte, die Unabhängigkeit zu lieben"](https://funktionale-programmierung.de/2018/01/22/freie-monade.html) wurde gezeigt,
wie freie Monaden eingesetzt werden können, um die Ausführung eines Programmes von dessen Beschreibung zu entkoppeln. In diesem Folgeartikel werden wir in einfachem Scala die freie Monade algebraisch konstruieren. Dabei wird erneut deutlich, dass die Formulierung von Operationen als Daten dabei hilft, über Probleme strukturiert nachdenken zu können. 

<!-- more start -->

## Voraussetzungen

Dieser Artikel beschreibt die Implementierung der freien Monade in [Scala](https://www.scala-lang.org). Daher wird eine gute Grundkenntnis in Scala vorausgesetzt. Zudem sollte der [vorausgegangene Artikel](https://funktionale-programmierung.de/2018/01/22/freie-monade.html) über freie Monaden gelesen und verstanden worden sein. Auf die praktischen Aspekte und Anwendungen der freien Monade wird in diesem Artikel nicht eingegangen. Zusätzlich ist gute Kenntnis von _Monaden_ für das Verständnis erforderlich. 


## Monaden in Scala

Was in Haskell das monadische `do` ist, ist in Scala `for`. Der for-Ausdruck ermöglicht es, monadische Operationen eleganter und verständlicher zu beschreiben. Betrachten wir ein Beispiel mit einer Option-Monade:
 
{% highlight scala %}

val res : Option[Int] = for {
  a <- Some(3)
  b <- Some(10)
} yield (a + b)

{% endhighlight %}

Der Option-Typ hat zwei Klassen, `Some` und `None`. Er ist das Äquivalent zu Haskells `Maybe`. Der obige for-Ausdruck ist syntaktischer Zucker für folgende Berechnung:
 
{% highlight scala %}

val res : Option[Int] = Some(3).flatMap(a => Some(10).map(b => a + b))

{% endhighlight %}

Um eine Monade vom Typ `M[A]` in einem for-Ausdruck komponieren zu können, muss `M[A]` eine Methode `flatMap` mit der Signatur `def flatMap[B](f: A => M[B]) : M[B]` und eine Methode `map` mit der Signatur `def map[B](f: A => B) : M[B]` implementieren. Dieses Wissen wird im Folgenden verwendet, um die freie Monade mit einem einfachen Trick zu implementieren.

## Operationen als Daten

Im [vorherigen Artikel](https://funktionale-programmierung.de/2018/01/22/freie-monade.html) über freie Monaden haben wir einen Trick kennengelernt: Beim Erstellen der Algebra haben wir Operationen von Funktionen in Daten übersetzt, um diese nicht direkt auszuführen. Dabei ist folgende Domain Specific Language (DSL) für ein Adressbuch entstanden, die wir in diesem Artikel weiterhin verwenden:

{% highlight scala %}

sealed trait AddressBookOp[A]

case class Put(address: Address) extends AddressBookOp[Unit]
case class Get(id : Long) extends AddressBookOp[Option[Address]]
case class Delete(address : Address) extends AddressBookOp[Unit]
case class Filter(foo : Address => Boolean) extends AddressBookOp[List[Address]]
    
{% endhighlight %}

Durch das Heben solcher algebraischer Datentypen in die freie Monade, schafft man die Möglichkeit, die Operationen monadisch zu kombinieren. Dabei ist die Definition der freien Monade unabhängig von einem speziellen Typen, es muss nur ein Container-Typ der Form `F[_]` sein.

Aus dem vorherigen Artikel wissen wir zudem, das die Algebra-Operationen, die mit Hilfe der freien Monade komponiert werden, später interpretiert und mittels natürlicher Transformation sogar in eine andere Monade überführt werden können. Dazu muss der Interpreter jedoch die Struktur des zu interpretierenden Programmes ablaufen können. Dies wird erreicht, indem die monadische Komposition der freien Monade Datenstrukturen aufbaut, die eben diese Komposition widerspiegeln. Daher repräsentieren wir im folgenden die monadischen Methoden durch Daten. Wie oben gezeigt, implementiert eine Monade in Scala zwei Methoden: `def flatMap[B](f: A => M[B]) : M[B]` und `def map[B](f: A => B) : M[B]`. Ersetzen wir diese durch Daten, könnten wir ein Programm unserer Addressbuchalgebra, das eine Adresse liest und diese löscht, wie folgt modellieren:

{% highlight scala %}

FlatMap(Get(1), a => Map(Delete(a), () => ()) 

{% endhighlight %}


Dazu führen wir den algebraischen Datentypen `sealed trait Free[F[_], A]` ein. Dieser nimmt den Algebratypen `F[_]` und dessen Rückgabetyp `A` (beispielsweise gibt `Get` der obigen Algebra `Option[Address]` zurück). Nun implementieren wir zwei Klassen, die `Free` erweitern, nämlich wie im Beispiel `FlatMap` und `Map`. Diese repräsentieren die Methoden `flatMap` und `map`.

Beginnen wir mit `map`. Da es sich um eine Methode handelt, muss ein expliziter Parameter hinzugefügt werden. Dieser repräsentiert den Wert auf den die `map` Methode angewandt wird. In der Regel sind dies Werte vom Typ der Algebra, im Beispiel oben `Delete(1)`. Anhand der Signatur sehen wir, dass `map` eine Funktion entgegen nimmt, welche den im Eingabeparameter gekapselten Typen auf einen Zieltypen abbildet. Dieser entspricht dem neuen Rückgabewert der Algebraoperation. Wir nennen diese Funktion `continuation` und kommen zu folgender Definition:

{% highlight scala %}
case class Map[F[_], I, A](param : F[I], continuation: I => A) extends Free[F, A] 
{% endhighlight %}

Für `flatMap` verhält es sich ähnlich, nur konstruiert die `continuation` einen neuen Wert vom Typen `Free`:


{% highlight scala %}
case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A])
                                                            extends Free[F,A]
{% endhighlight %}

Es gibt nun also die Möglichkeit anhand von Algebraoperationen und _continuations_ eine monadische Struktur zu beschreiben, ohne dass die Algebra monadisch ist. Allerdings ist die Beschreibung auf diese Weise sehr mühsam. Es wäre einfacher, diese Beschreibung wieder rum mit Hilfe eines for-Ausdrucks formulieren zu können. 

## Meta-Monadisch

Jetzt nicht den Kopf verlieren: Von nun an ist es wichtig, zwischen dem Typ `FlatMap` und der Funktion `flatMap` aufmerksam zu unterscheiden (respektive `Map`). Um `Free` monadisch kombinieren zu können, muss es nämlich die Funktionen `map` und `flatMap` wie folgt implementieren:

{% highlight scala %}

sealed trait Free[F[_], A]{

  def flatMap[B](f : A => Free[F, B]) : Free[F,B]

  def map[B](f: A => B): Free[F,B]}
}

{% endhighlight %}

Doch wie sieht die Implementierung dieser Funktionen aus? Betrachten wir zunächst `Map`:

{% highlight scala %}

case class Map[F[_], I, A](param : F[I], continuation: I => A) extends Free[F, A] {

  def flatMap[B](f : A => Free[F, B]) : Free[F,B] = 
    FlatMap(param, continuation andThen f)

  def map[B](f: A => B): Free[F,B] = Map(param, continuation andThen f)
}

{% endhighlight %}

Die monadischen Methoden in `Map` implementieren die einfache Hintereinanderausführung von Funktionen. Unterstützt durch die Signaturen der Methoden, kann hier nicht viel falsch gemacht werden. Bei `flatMap` führt eine Verkettung von `f` und `continuation` zu einer Funktion mit der Signatur `I => Free[F, B]`, was der `continuation` eines `FlatMap`s entspricht. Bei `map` führt eine Verkettung der beiden Funktionen zu einer Funktion von `I => A`, was der `continuation` eines `Map`s entspricht.

Betrachten wir nun `FlatMap`:

{% highlight scala %}

case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A]) ... {

  def flatMap[B](f: A => Free[F, B]): Free[F,B] = 
    FlatMap(param, continuation andThen (x => x.flatMap(f)))

  def map[B](f: A => B): Free[F,B] = 
    FlatMap(param, continuation andThen (x => x.map(f)))
}

{% endhighlight %}

Hierbei fällt auf, dass beide Methoden wiederrum rekursive Aufrufe beinhalten. Das liegt daran, dass die übergebene Funktion `f` zum Ende des Programms propagiert wird. Erst, wenn ein `Map` erreicht wird, wird die Rekursion abgebrochen und `f` in einer neuen `continuation` verkettet. 

Bei beiden Implementierungen wird der Parameter `param` nicht angetastet und lediglich die Transformation, gegen durch `continuation` erweitert.



## Die ersten Schritte

Es ist jetzt möglich, Werte vom Typ `Free` mithilfe eines for-Ausdrucks zu kombinieren. Doch wie verbinden wir dies mit unserer Addressbuch-Algebra? Im vorherigen Artikel wurde die Funktion `liftF` der _cats_-Bibliothek verwendet, um die Algebra in die Monade zu heben. Eine Funktion, die Container-Typen in `Free` hebt, sieht wie folgt aus:

{% highlight scala %}
  def liftF[F[_], A](operation: F[A]): Free[F, A] = Map(operation, identity[A])
{% endhighlight %}

Dabei wird aus der übergebenen Operation mithilfe von `Map` ein Wert vom Typ `Free` konstruiert. Als zweiten Parameter übergeben wir dem Konstruktor die Identitätsfunktion. Somit wird der Rückgabewert bei der Auswertung der Operation durch den Interpreter einfach unverändert zurückgegeben. Dieses `liftF` entspricht dem `Return`, dass wir aus Haskell kennen. Im Anschluss werden die aus dem vorherigen Artikel bekannten _smart-constructors_ definiert:

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
          val ret = interpreter(param)
          continuation(ret)
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

Um den monadischen for-Ausdruck in Scala verwenden zu können, mussten zwei Funktionen implementiert werden, nämlich `flatMap` und `map`. Während `flatMap` das äquivalent zu Haskells `bind` ist, darf das üblicherweise verwendete `return` nicht mit `map` gleichgesetzt werden. `return` nimmt lediglich einen Wert entgegen, während `map` zusätzlich eine Funktion entgegennimmt. `Return` entspricht also `Map` mit der Identitätsfunktion. Diese Funktion haben wir bereits implementiert und sie `liftF` genannt. Spricht man von freien Monaden, wird `bind` beziehungsweise `flatMap` oft als `Suspend` bezeichnet.
