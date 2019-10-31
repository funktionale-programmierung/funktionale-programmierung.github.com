---
layout: post
description: "Algebraische-Effekte"
title: "Einführung in algebraische Effekte"
author: simon-haerer
tags: ["algebraische Effekte", "Effekte", "algebraische", "Koka", "Monadentransformer", "Seiteneffekt", "Seiteneffekte", "algebraic effects", "algebraic", "effects"]
---

Algebraische Effekte ermöglichen es, Seiteneffekte elegant auszudrücken und
ausführende Operationen zu kombinieren. Mit algebraischen Effekten können viele
"pain points" der funktionalen Programmierung gelöst werden. Insbesondere
reduzieren sie die Komplexität, die aus dem Umgang mit Seiteneffekten entsteht.
Dieser Artikel gibt anhand der Programmiersprache Koka eine Einführung in
algebraische Effekte und erläuert dem Leser die Vorteile und möglichen
Anwendungen.

<!-- more start -->


# Warum algebraische Effekte?

Eine Komponente der funktionalen Programmierung ist das Bestreben,
seiteneffektfrei, also "pure" zu programmieren. Natürlich ist das in realen
Anwendungen nicht vollständig durchsetzbar. In der Praxis wird daher versucht,
zumindest große Teile einer Anwendung seiteneffektfrei zu implementieren.

Um das zu ermöglichen, werden Konzepte wie zum Beispiel Monaden dazu
verwendet, seiteneffektbehaftete Berechnungen zuerst lediglich zu beschreiben
und die Ausführung auf sorgfältig gewählte Stellen der Anwendung zu
beschränken. So können große Teile getestet werden, ohne beispielsweise
Umgebungen wie Datenbanken bereitstellen zu müssen.

Leider kommen Monaden mit einem deutlichen Zusatzaufwand. In stark typisierten
Sprachen behindern Typen oft das Formulieren der Domänenlogik. Kommen mehrere
Monaden zum Einsatz, wird es besonders knifflig: Für die Kombination von Monaden
existiert keine allgemeingültige Abstraktion. Die Kombination erfolgt in der
Regel mit sogenannten Monadentransformatoren und diese müssen für jeden
Monadenstapel neu gedacht und implementiert werden. Ist der Monadenstapel einmal
programmiert, erfordern dessen Anwendung und Transformationen viele
Hilfsfunktionen.

Algebraische Effekte hingegen sind einfach zu kombinieren. Das derzeit für
Aufsehen sorgende Konzept wurde bereits [2002 das erste Mal
beschrieben](http://homepages.inf.ed.ac.uk/gdp/publications/alg_ops_gen_effects.pdf).
Algebraische Effekte lassen sich am besten als Exceptions beschreiben, die
zusätzlich die Möglichkeit bieten, wieder an die Codestelle, an der sie
ausgelöst wurden, zurückzuspringen. Wie das die oben beschriebene Problematik
der seiteneffektfreien Programmierung löst, wird im folgenden anhand von
Codebeispielen in der Programmiersprache Koka verdeutlicht.

# Koka

[Koka](https://github.com/koka-lang/koka) ist eine funktionale
Programmiersprache, die ursprünglich von Microsoft Research zur Forschung an
algebraischen Effekten entwickelt wurde. Heute gibt es bereits [ernstzunehmende
Anwendungen](https://www.madoko.net/), die in Koka implementiert sind. In Koka
ist die Trennung von Werten und seiteneffektbehafteten Operationen als
Besonderheit direkt in die Sprache implementiert.

Eine sichere Division in Koka könnte wie folgt aussehen:

    fun div(a : int, b: int) : <exn> int {
      if(b == 0) then throw("Division by Zero")
      else a / b
    }
    
    public fun main() : <console, exn> () {
      println("We are just dividing numbers! Or, aren't we?")
      println(div(6,2))
    }

Eine Sache fällt sofort auf: Der Rückgabetyp der `div`-Prozedur ist nicht etwa
nur `int`, sondern `<exn> int`. `exn` ist ein algebraischer Effekt, der im
Koka-Core mitgeliefert wird. Dieser beschreibt, dass die Prozedur eine Exception
auslösen könnte. Analog beschreibt der `console` Effekt die Ausgabe von
Werten in die Konsole. Wie zu sehen ist, hat die `main`-Prozedur auch `exn` in
ihrer Typsignatur, da `div` diesen Typ zurückgibt. Um zu verstehen, wie
Exceptions in Koka funktionieren, implementieren wir sie einmal selbst.



# Exceptions

Koka macht es uns einfach, eigene algebraische Effekte zu definieren.
Ein algebraischer Effekt ist ein Name und eine Menge an Operationen:

    effect my-exception {
      fun my-throw(msg : string) : a
    }

Unser Effekt `my-exception` besteht demnach aus einer `my-throw` Methode, parallel
zur obigen `throw`-Methode. Diese nimmt eine Zeichenfolge entgegen und gibt
etwas vom Typ `a` zurück. Die obige `div`-Prozedur kann
damit wie folgt umgebaut werden:

    
    fun div(a : int, b: int) : <my-exception> int {
      if(b == 0) then my-throw("Division by Zero")
      else a / b
    }
    
    public fun main() : <console, my-exception> () {
      println("We are just dividing numbers! Or, aren't we?")
      println(div(6,2))
    }

Die eingebauten Effekte `exn` und `console` werden außerhalb der Main-Prozedur
abgehandelt. Der Koka-Compiler wird sich jedoch beschweren, dass der Effekt
`my-exception` nicht abgehandelt wird. 

    (1, 0): error: there are unhandled effects for the main expression
      inferred effect: <b/my-exception,console>
      hint           : wrap the main function in a handler

Wir müssen also für den `my-exception`-Effekt einen Handler implementieren. Ein
Handler ist eine Methode, die angibt, was passiert, wenn ein Effekt auftritt.
Gerne wird hier auch von Kontext gesprochen. Bei Exceptions bedeutet das, dass
wir ein try-catch-Konstrukt definieren werden. Das kann wie folgt aussehen:

    
    fun my-catch(try-fun, catch-fun)  {
      handle(try-fun) {
        my-throw(msg) -> catch-fun(msg)
      }
    }

Die Prozedur `my-catch` nimmt eine nullstellige Funktion `try-fun` und
eine einstellige Funktion `catch-fun`, die den Catch-Block repräsentiert und
der die Nachricht, die `my-throw` übergeben wird, entgegen. `handle` ist
eine Koka-Core-Prozedur. Sie nimmt eine Funktion entgegen und wertet diese aus.
`handle` erlaubt dabei auf mögliche Effektoperationen zu hören, in unserem
Fall `my-throw` und darauf zu handeln. Eingesetzt wird dieser Handler wie folgt:

    
    fun safediv(a : int, b: int) : <> int {
      my-catch({div(a,b)}, fun(_msg){0})
    }

    public fun main() : <console> () {
      println("We are just dividing numbers! Or, aren't we?")
      println(safediv(6,0))
    }
    
    ;; "We are just dividing numbers! Or, aren't we?"
    ;; 0

Der `div`-Aufruf in `safediv` steht in geschweiften Klammern. Das ist in Koka
syntaktischer Zucker für anonyme Funktionen ohne Argumente. Die Catch-Funktion
ignoriert die Nachricht in `my-throw` und der Handler gibt in diesem Fall einfach 0
zurück.

Wie man sieht, gibt die `safediv`-Prozedur lediglich einen Wert vom Typ `int`
zurück, die Effektliste ist leer. In diesem Fall spricht man von einer totalen
Funktion. Auf diese Weise lassen sich Effekte in Koka behandeln. Das Typsystem
hilft uns dabei, die nicht behandelten Effekte zu verfolgen.

Im Folgenden werden wir die wahre Stärke von algebraischen Effekten
kennenlernen. Wie eingangs erwähnt, kann aus einem Effekthandler zurück in den
Code gesprungen werden.


# Eine Datenbank als Effekt

Algebraische Effekte sind dazu da, Seiteneffekte abzubilden. In den
allermeisten Anwendungen spielt Datenhaltung eine Rolle. Eine
Benutzerverwaltung könnte durch Effekte wie folgt beschrieben werden:

    struct user (id: int, name: string)
    
    effect user-repository {
      fun add-user(name : string) : int
      fun get-user(id : int) : user
    }

    fun add-user-with-name(firstname: string, lastname: string): <user-repository> int {
      val name = firstname + " " + lastname
      add-user(name)
    }
    
    fun get-user-name(id : int) : <user-repository> string {
      val user = get-user(id)
      user.name
    }

Bei beiden Effektoperationen soll das Programm, nachdem der Effekt ausgeführt
wurde, mit deren Ergebnissen dort fortgeführt werden, wo der Effekt ausgelöst
wurde. Ein Effekthandler, in der Art wie er Exceptionsbeispiel implementiert wurde,
kommt also nicht infrage, da im Falle einer Exception der Programmfluss
unterbrochen wird. Effekthandler ermöglichen es jedoch, dass an die Stelle, an
der der Effekt im Programm auftritt, zurückgesprungen wird:


    val database-handler = handler {
      return x -> x
      add-user(user-name) -> resume( database/add-user(...) )
      get-user(id) -> resume( database/get-user(id, ...) )
    }
    
    fun main() {
      database-handler({add-user-with-name("Bob", "Bobbig")})
      () // the main-function always returns unit
    }


Die Definition dieses Handlers unterscheidet sich von der des Exceptionhandlers: 

-   Der Handler enthält eine `return`-Anweisung. Diese sagt in diesem Beispiel
    lediglich, dass ein Wert, der kein Effekt ist, vom Handler einfach
    zurückgegeben wird.

-   Die `resume`-Anweisung ermöglicht das gewünschte Verhalten des
    Wiedereinstiegs in die Codestelle, an der der Effekt ausgelöst wurde. Die
    Anweisung nimmt einen Wert entgegen, der an Stelle des Effekts zurückgegeben
    werden soll.

-   Es wird die `handler`-Prozedur, statt der `handle`-Prozedur verwendet.
    Dabei handelt es sich um eine Hilfsfunktion, die wiederrum eine Funktion
    zurückgibt. Die zurückgegebene Funktion kann, wie in der `main`-Prozedur zu
    sehen ist, als Handler verwendet werden.

Dieses Anwendungsbeispiel macht deutlich, wie algebraische Effekte in der
Praxis eingesetzt werden können, um Seiteneffekt abzubilden. Sie erlauben uns,
Programme zu schreiben, in denen wir Seiteneffekte auf Typebene beschreiben,
auf Werteebene jedoch weitgehend ignorieren können.

Bringt man algebraische Effekthandler an wenigen, sorgfältig ausgewählten
Punkten ins Programm ein, können sie mit wenig Aufwand ausgetauscht werden. Dann
ist es ein Leichtes etwa die Datenbanktechnologie durch einen neuen Handler
auszutauschen oder gar die Benutzerverwaltung durch einen externen Service zu
ersetzen. Möchte man in Unit-Tests ohne echte Datenbank testen, ersetzt man sie
durch einen Key-Value-Store, indem ein geeigneter Handler implementiert wird.


# Effekte kombinieren

Eingangs wurde darauf hingewiesen, dass Monaden mithilfe von Monadentransformern
kombiniert werden können. Dies ist oft mühsam, die Implementierung kostet Zeit
und zusätzliche Komplexität kommt in die Anwendung. Die Kombination von Effekten
hingegen ist denkbar einfach. Sehen wir uns dazu im obigen Beispiel eine sichere
Implementierung der `add-user-with-name`-Prozedur an:


    fun add-user-with-name(firstname: string, lastname: string): 
                                            <user-repository, my-exception> int {
      if(firstname.vector.length == 0 || lastname.vector.length == 0) then
        my-throw("first- or lastname empty")
      else
        add-user(firstname + " " + lastname)
    }
    
Wir sehen, das der Exceptioneffekt in die Typsignatur aufgenommen wird. Nun
müssen wir lediglich einen Handler implementieren, der die Exception abarbeitet:

    public fun main() {
      my-catch({ 
        database-handler({add-user-with-name("Bob", "Bobbig")})
      }, fun(_msg){ -1 })
      () // the main-function always returns unit
    }


Das Kombinieren von algebraischen Effekten ist sehr einfach.


# Fazit

In diesem Blogartikel haben wir algebraische Effekte als Alternative zu Monaden
und Monadentransformatoren zur Abbildung von Seiteneffekten kennengelernt. Wir
haben am Beispiel der Programmiersprache Koka gesehen, wie Effekte implementiert
werden und warum dieses Konzept sehr vielversprechend ist. 

Algebraische Effekte sind bisher in wenigen Sprachen als natives Feature
implementiert. Der populärste Vertreter ist Multicore-OCaml. Dort sind Effekte
jedoch _unchecked_. Das bedeutet, dass der Compiler nicht darauf hinweist, dass
ein Effekt unbehandelt ist. Für andere Sprachen wie Haskell und Scala gibt es
Bibliotheken, die die Implementierung mit algebraischen Effekten ermöglichen.
Allerdings sind die Effekte hier monadisch auf Werteebene implementiert. Daher
leben diese Programme oft vollständig in den jeweiligen Monaden. Eine
einfache Behandlung von Werten ohne monadische Kommandos, wie bei Koka, ist
nicht möglich.

Am Beispiel Koka sehen wir jedoch, wie einfach und intuitiv die Programmierung
mit algbraischen Effekten in Zukunft sein kann. Algebraische Effekte lösen viele
Schwierigkeiten, die in der funktionalen Programmierung entstehen und für
zusätzliche Komplexität sorgen. Wir hoffen, dass algebraische Effekte als
Sprachfeature Einzug in weitere Programmiersprachen finden.

In einem kommenden Blogpost werden wir weitere Möglichkeiten, die Effekte bieten, am
Beispiel Koka aufzeigen. Dazu zählen spannende Mechanismen, wie zum Beispiel
die Implementierung von Iterators und mehrfaches Zurückspringen innerhalb eines
Handlers.

Alle Beispiele können als vollständig lauffähiger Code auf
[Github](https://github.com/smoes/algebraic-effects-blogpost/blob/master/example.kk)
gefunden werden.
