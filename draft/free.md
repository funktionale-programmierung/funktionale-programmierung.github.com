---
layout: post
description: "Einführung in freie Monaden"
title: "Freie Monaden oder: Wie ich lernte, die Unabhängigkeit zu lieben"
author: Simon-Schulz
tags: ["Praxis", "freie", "Monade", "Scala"]
---

Eine enge Kopplung der Beschreibung von Programmteilen und deren Ausführung führt unweigerlich
zu Problemen, spätestens beim Testen der Software. Daher soll in diesem Artikel anhand von 
praktischem Code erklärt werden, wie uns das Konzept der freien Monade dabei hilft,
Beschreibung und Ausführung sauber und elegant voneinander zu trennen.


<!-- more start -->

## Voraussetzungen

Alle Code-Beispiele in diesem Artikel sind mit [Scala](https://de.wikipedia.org/wiki/Scala_(Programmiersprache)) implementiert. Da dieser Artikel praxisorientiert ist, werden die Beispiele unter Zuhilfenahme der Bibliothek [Cats](https://typelevel.org/cats/) implementiert. Zwar wird die freie Monade nicht theoretisch erklärt, dennoch sollte das 
Konzept der Monaden geläufig sein. Eine tolle und einfache Einführung hierzu bietet [Functors, Applicatives, And Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html). Für den theoretisch interessierten Leser werden an einigen Stellen weiterführende Links bereitgestellt.

Um einen einfachen Einstieg zu ermöglichen, sehen wir uns ein typisches Problem in Verbindung mit Seiteneffekten an.


## So nicht!

Der folgende Scala-Code zeigt eine einfache Implementierung eines Adressbuchs. Hierzu definieren
wir eine case-Klasse, die eine Adresse repräsentiert und ein Objekt, das Funktion für die Persistierung selbiger anbietet: 
 
{% highlight scala %}

case class Address(id: Long, name : String, street: String, town: String, zip: String, company: String)

object AddressBookRepo {
  def put(address : Address) : Future[Either[Throwable, Unit]] = ???
  def get(id : Long) : Future[Either[Throwable, Option[Address]]] = ???
  def delete(address : Address) : Future[Either[Throwable, Unit]] = ???
  def filter(foo : Address => Boolean) : Future[Either[Throwable, List[Address]]] = ???

  def removeBielefeld : Future[Either[Throwable, List[Address]]] = {
    val bielefeldFE = filter(_.town == "Bielefeld")
    for {
      addressesE <-bielefeldFE
    } yield {
      addressesE.map(_.foreach(address => delete(address)))
      addressesE
    }
  }
}
{% endhighlight %}

Die einfach Verwendung des Repos direkt aus der Businesslogik heraus, führt unmittelbar zu mehreren Problemen. Was stimmt damit nicht?

* *Seiteneffekt werden sofort ausgeführt*: Ein Grundgedanke der funktionalen Programmierung ist die Erstellung von referentiell transparenten Programmen, also ohne Seiteneffekte. Diese Seiteneffekt sollen möglichst erst "am Ende des Universum" ausgeführt werden. Im Beispiel würden Effekte jedoch verstreut an vielen Stellen im Programm ausgeführt werden. Insbesondere würde die Funktion `removeBielefeld` bei Mehrfachausführung unterschiedliche Ergebnisse liefern.
* *Die Schnittstelle ist zu speziell:* Die Trennung von Beschreibung und Ausführung ist ein wichtiges Konzept, das zu modularem, exzellent wartbarem und testbarem Code führt. Bei der Beschreibung unserer Adressbuch-Operationen sollten wir uns nicht mit Futures, Eithers und Throwables abmühen müssen. Mehr noch, setzen wir damit ein bestimmtes Datenbank voraus (in diesem Fall ein asynchrones, das Exceptions verwendet), das damit zu stark in der Business-Logik unserer Anwendung verzahnt und somit nur schwer austauschbar wäre.

Ein erster Schritt, um derartige Aspekte zu entkoppeln, ist die Formulierung der Operationen als eigene Entitäten. Damit ist es uns möglich, die nötigen Operationen als Daten zu repräsentieren, die dann erst später ausgeführt werden.

## Unsere kleine Sprache

Um die Operationen als Entitäten zu formulieren, erstellen wir für jede Operation eine Datenrepräsentation. Diese Repräsentation fassen wir als algebraischen Summentyp zusammen:

{% highlight scala %}

sealed trait AddressBookOp[A]
case class Put(address: Address) extends AddressBookOp[Unit]
case class Get(id : Long) extends AddressBookOp[Option[Address]]
case class Delete(address : Address) extends AddressBookOp[Unit]
case class Filter(foo : Address => Boolean) extends AddressBookOp[List[Address]]
    
{% endhighlight %}

Der Typ-Parameter `A` des traits `AdressBookOp[A]` bestimmt dabei den Rückgabetyp unserer Operationen. Zwar lassen sich mit diesen Operationen einfache Programme beschreiben, etwa durch Listen von Befehlen, es lassen sich allerdings keine komplexe Zusammenhänge formulieren. Es ist nicht möglich gelesene Daten zu referenzieren, ohne einen Seiteneffekt auszuführen, wie zum Beispiel in der Funktion `removeBielefeld`. Gerne hätten wir hier monadische Syntax, um Rückgabewerte (ohne Ausführung des Effekts) binden zu können.


## Freie Monade, zur Rettung!

Wir verwenden in diesem Artikel die freie Monade aus Cats. Cats ist eine Scala-Bibliothek die mächtige Abstraktionen aus der Funktionalen Programmierung bereitstellt. Wir werden neben der Funktionalität zu freien Monaden weitere Abstraktionen, wie die Natürliche Transformationen aus dieser Bibliothek verwenden. Die freie Monade wird uns helfen, die Einschränkungen unserer kleinen Sprache zu überwinden.

Um unsere Befehle mit der freien Monade aus Cats verwenden zu können, müssen wir diese in die Monade heben. Dazu erstellen wir für jede Funktion einen sogenannten Smart-Konstruktor für jeden Befehl:


{% highlight scala %}

import cats.free.Free
import cats.free.Free.liftF

object FreeAddressBookOps {

  // Typ-Alias für lifted AddressBookOps
  type AddressBookOpF[A] = Free[AddressBookOp, A]

  def put(address: Address) : AddressBookOpF[Unit] =
               liftF[AddressBookOp, Unit](Put(address))
  def get(id: Long) : AddressBookOpF[Option[Address]] =
               liftF[AddressBookOp, Option[Address]](Get(id))
  def delete(address: Address) : AddressBookOpF[Unit] = 
               liftF[AddressBookOp, Unit](Delete(address))
  def filter(foo: Address => Boolean) : AddressBookOpF[List[Address]] = 
               liftF[AddressBookOp, List[Address]](Filter(foo))
}

{% endhighlight %}
    
Die Smart-Konstruktoren nehmen jeweils die Parameter der `AddressBookOps` entgegen, erzeugen die Operation und verwenden `liftF`, um die Operation in die freie Monade zu heben. Zurück kommt ein Wert vom Typ `AddressBookOp[A]` (Alias für `Free[AddressBookOp, A]`), wobei `A` der erwartete Rückgabewert der Operation ist. Die Definition dieser Smart-Konstruktoren macht die Beschreibung der späteren Programme einfacher. Die Funktion `removeBielefeld` ist mithilfe der Definition einfach monadisch beschreibbar, insbesondere lassen sich nun Zwischenergebnisse binden:


{% highlight scala %}

import cats.instances.list._
import cats.syntax.traverse._
import FreeAddressBookOps._


object FreeApp {

  val removeBielefeld: AddressBookOpF[List[Address]] = for {
    addresses <- filter(_.town == "Bielefeld")
    _ <- addresses.traverse(delete(_))
  } yield addresses
}

{% endhighlight %}

Durch die Bindung der Zwischenwerte lassen sich nun komplexe Programme formulieren, ohne über Details der Ausführung sprechen zu müssen: Bei der Formulierung des Programmes wird weder über Throwables noch über Futures nachgedacht. `traverse` ist eine weitere kleine Hilfestellung aus der Cats-Bibliothek, für die wir die zusätzlichen Cats-Imports benötigen. Das Ablaufen der Zwischenergebnisliste liese sich auch durch `fold` Beschreiben.

Unsere neue Version von `removeBielefeld` gibt ein Wert vom Typ `AddressBookOpF[List[Address]]` zurück. Diese Programmbeschreibung ist kombinier- und damit wiederverwendbar:


{% highlight scala %}

def renameBielefeld: AddressBookOpF[Unit] = for {
  for {
    bielefelders <- removeBielefeld
    _ <- bielefelders.traverse(address => put(address.copy(town="not existant")))
  } yield ()
}

{% endhighlight %}

## Die Ausführung ##

Wir haben nun also eine mächtige Repräsentation, um Operationen zu repräsentieren, Zwischenergebnisse zu referenzieren und Abstraktionen zu kombinieren. Was fehlt, ist die Ausführung. Dazu benötigen wir einen Interpreter, der `AddressBookOpF[List[Address]]` entgegen nimmt und interpetiert (oder kompiliert). Dazu definieren wie eine [Natürliche Transformation](https://de.wikipedia.org/wiki/Kategorientheorie#Nat.C3.BCrliche_Transformation). Salopp gesagt, überführen wir unsere Programme aus der freien Monade in eine Zielmonade. Im folgenden Beispiel wird die [State-Monade](https://typelevel.org/cats/datatypes/state.html) als einfacher Adressspeicher verwendet:

{% highlight scala %}

import cats.data.{State}
import cats.~>

object StateInterpreter {

  type AddressBook = Map[Long, Address]
  type AddressState[A] = State[AddressBook, A]

  val interpet = new (AddressBookOp ~> AddressState) {
    def apply[A](fa: AddressBookOp[A]) : AddressState[A] = fa match {
      case Put(address) => State.modify[AddressBook]{_ + (address.id -> address)}
      case Get(id) => State.inspect[AddressBook, Option[Address]]{_.get(id)}
      case Delete(address) => State.modify[AddressBook]{_ - address.id}
     case Filter(foo) => State.inspect{_.values.filter(foo).toList}
    }
  }
}

{% endhighlight %}

In Cats wird die Natürliche Transformation von `A` nach `B` durch den Typ `A ~> B` beschrieben (eine Funktor-Transformation vom Typ `FunktionK`). In unserem Fall überführen wir `AddressBookOps` nach `AddressStatate`. Der Interpreter lässt sich nun durch die Verwendung von `foldMap` auf Programm vom Typ `AddressBookOpF` anwenden:


{% highlight scala %}

// Anwendung des State-Interpreters
val state: AddressState[List[Address]] = FreeApp.removeBielefeld.foldMap(StateInterpreter.interpet)
// Ausführung der State-Monade
val initialStorage = Map(1 -> Address(1, "Santa Clause", "Santa Street", "Bielefeld", "77777", "Santa Inc"))
val (storage, bielefelder) = state.run(initialStorage)

{% endhighlight %}


Ein Interpreter für unser ursprüngliches Adressbuch-Repo könnte dann wie folgt aussehen:

{% highlight scala %}

object DatabaseInterpreter {

  type DatabaseReturnType[A] = Future[Either[Throwable, A]]

  val interpret = new (AddressBookOp ~> DatabaseReturnType) {
    def apply[A](fa: AddressBookOp[A]) : DatabaseReturnType[A] = fa match {
      case Put(address) => AddressBookDatabase.insert(address)
      case Get(id) => AddressBookDatabase.get(id)
      case Delete(address) => AddressBookDatabase.delete(address)
      case Filter(foo) => AddressBookDatabase.where(foo)
    }
  }
  
  // Ausführung
  val result : Future[Either[Throwable, A]] = FreeApp.removeBielefeld.foldMap(interpret)
}

{% endhighlight %}

Wie zu sehen ist, werden wir erst bei der Intepretation unseres Programmes mit den technischen Aspekten wie Futures und Throwables konfrontiert. Es existiert eine klare Trennung zwischen Beschreibung und Ausführung. Diese geht so weit, dass wir während der Formulierung der Logik nicht wissen müssen, wer unsere Beschreibung in welcher Form ausführt. Bisher haben wir zwar von Datenbanken oder Repos gesprochen. Doch nichts hindert uns daran, einen aktorbasierten Service als Adressbuch zu implementieren. Die entstehenden Beschreibungen können problemlos in Form von Nachrichten zwischen Aktoren ausgetauscht werden. In dieser Unabhängigkeit liegt der Charme freier Monaden.

## Worte zur Testbarkeit

Ein großer Vorteil der Trennung von Beschreibung und Ausführung ist die separate Testbarkeit der Beschreibungen. So können Beschreibungen auch außerhalb von Integrationstests getestet werden ohne Datenbanken bereitstellen zu müssen. Das hört sich zuerst charmant an, ist allerdings bei freien Monaden schwieriger, als zunächst angenommen. Beschreibungen repräsentieren zwar abstrakte Syntax, diese liegen jedoch in Form einer Funktion vor und lassen sich zunächst nicht einfach inspizieren. Zwar können wir Test-Interpreter, wie den State-Interpreter aus dem obigen Beispiel, implementieren. Allerdings besteht hier die Gefahr, dass sich der Test- und der "echte" Interpreter abweichend verhalten. 

Denkbar wäre jedoch die Implementierung eines sehr einfachen Testinterpreters, der die ausgeführten Anweisungen beispielsweise lediglich traced. Allerdings muss hierzu in einigen Fällen trotzdem das Verhalten implementiert werden, insbesondere dann, wenn bestimmte Operationen nur bedingt durch gebundene Daten ausgeführt werden. Wollen wir die enstehenden Programme also inhaltlich testen, müssen wir ein Trade-Off zwischen Genauigkeit und Fehleranfälligkeit der Interpretation eingehen.

Wird konsequent zwischen Beschreibung und Ausführung getrennt, können dennoch große Codeteile in Unit-Tests abgedeckt werden, ohne eine integrierte Umgebung bieten zu müssen. Diese Entkopplung macht das Testen unserer Businesslogik oft schon um vieles einfacher. Eine mögliche Synergie, die dies verdeutlicht, ist in Kombination mit dem aktorbasierten [Observer-Pattern](https://alexn.org/blog/2015/12/15/avoid-javaisms-code-smell.html)


## Resumé 

In diesem Blog-Artikel haben wir entlang eines einfachen Beispiels eine kleine Sprache mit Hilfe der freien Monade implementiert. Diese Sprache hat es uns ermöglicht, kombinierbarer Beschreibungen ohne Wissen über die spätere Ausführung zu auszudrücken. Erst durch Intepreter (oder Compiler) werden die Beschreibungen ausgeführt und Details über die Ausführung bekannt. 

Der in diesem Artikel verwendete Code ist auf [Github](https://github.com/smoes/blogpost-free-1) verfügbar.

Im nächsten Post werden wir sehen, wie sich die entworfene Sprache erweitern lässt.

<!-- more end -->

