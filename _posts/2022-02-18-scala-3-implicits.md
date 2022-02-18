---
layout: post
description: Scala 3 Implicits
title: "Scala 3: Explizite Implicits"
author: simon-haerer
tags: ["Scala", "Scala 3", "Implicits", "Implicit Conversions", "Extension Methods", "Type Classes"]
---


Nach 8 Jahren, 28000 Commits und 7400 Pull-Requests war es am 14. Mai
2021 endlich so weit: Scala 3 wurde veröffentlicht. Neben dem neuen
Compiler „Dotty“ haben es eine neue Syntax sowie einige Neuerungen an
der Sprache in Scala 3 geschafft. In diesem Blogpost der Serie über
interessante Neuerungen werden wir über Implicits sprechen. Implicits
sind eines der Hauptcharakteristika für Scala und kommen in fast allen
Projekten zum Einsatz. Nicht nur deshalb haben sich die meisten
Scala-Programmierer:innen schon über Implicits geärgert: Implicits
sind zu implizit, zu vielseitig und gleichzeitig zu einfach zu
implementieren. Scala 3 versucht nun, die einzelnen Einsatzzwecke von
Implicits explizit zu definieren und damit das Keyword `implicit`
schließlich loszuwerden.


## Implicit Conversions

Tauchen wir direkt ein und betrachten einen beliebten
Anwendungsfall von Implicits aus Scala 2: Implicit Conversions. Im
folgenden Codebeispiel wird eine Funktion `stringToParrot` als
implizit definiert, um einen String automatisch in einen Papageien
umwandeln zu können. 


    object Animals {
    
      case class Parrot(sentence: String) {
        def say : Unit = println(sentence)
      }
    
      implicit def stringToParrot(sentence: String) : Parrot = Parrot(sentence)
    
      "arrrrrr!".say
    }
      

Beim Aufruf der Methode `say` versucht der Scala-Compiler durch
in der Umgebung vorhandene Implicit Conversions einen Typ zu
finden, der `say` implementiert. In diesem Fall wird mithilfe von
`stringToParrot` also ein Papagei erzeugt und die Methode `say` des
resultierenden Objekts aufgerufen. Warum ist diese Implementierung
gefährlich?

Ist der Code komplexer als in unserem Beispiel, oder greifen mehrere
Implicit Conversions ineinander, ist es oft sehr schwierig,
den Code nachzuvollziehen. Viele, die in Scala-Projekten
gearbeitet haben, haben bereits mit großen Fragezeichen auf den Code
gestarrt und sich beispielsweise gewundert, warum ein String auf
magische Weise das Verhalten eines Papageis aufweist. In Scala 2
können Implicit Conversions einfach mit einem Wildcard-Import
eingebunden werden und oft ist es ohne sehr gute IDE und lange Suche
unmöglich herauszufinden, warum auf einem Objekt
Methoden aufrufbar sind, die dieses eigentlich nicht implementiert.

Darüber hinaus wird durch die Syntax selbst nicht sofort ersichtlich,
was das eigentliche Vorhaben ist: nämlich eine Konvertierung zu
implementieren. Man kann also sagen, Implicit Conversions sind in
Scala 2 zu einfach und zu implizit definierbar, gemessen daran, was für
Risiken und Probleme sie in Projekten einführen.

Scala 3 führt hierfür einen gesonderten Typ ein:
`Conversion`. Dieser wird mit dem neuen Keyword `given` als implizit
deklariert und drückt das Vorhaben deutlich expliziter aus:

    object Animals:
    
      case class Parrot(sentence: String):
        def say : Unit = println(sentence)
    
    
      given stringToParrotConversion : Conversion[String, Parrot] = 
        new Conversion[String, Parrot]:
          def apply(sentence : String) : Parrot = Parrot(sentence + "!!!!!")
    
      "arrrrrr!".say

Diese neue Syntax soll das Implementieren von impliziten
Konvertierungen deutlich bewusster und reflektierter gestalten.

Darüber hinaus kann man Instanzen, die mit `given` definiert werden,
nicht mehr einfach nebenbei mit einer Wildcard importieren, sondern
muss dies explizit tun, wie im Folgenden erklärt wird.


## Importieren von `given`-Instanzen

Wollen wir die Implicit Conversions aus dem Objekt `Animals` in einem
anderen Objekt verwenden, müssen wir diese importieren. Das geht in
Scala 2 sehr einfach:

    object Main {
      import Animals._
      "arrrrrr!".say
    }

Durch den Wildcard-Import ist jedoch nicht direkt klar, dass die
Konvertierung aus diesem Modul kommt, insbesondere, wenn mehrere
solcher Imports passieren. Will man `given`-Instanzen in Scala 3
importieren, muss man dies nun explizit angeben:

    object Main:
      // importiert die given-Instanz nicht:
      // import Animals._ 
      
      // importiert alle given-Instanzen aus Animal
      import Animals.given
      "arrrrr!".say

Dies kann und sollte man noch expliziter machen, indem man den
genauen Typ der zu importierenden `given`-Instanz definiert:

    object Main:
      import Animals.Parrot
      import Animals.{given Conversion[String, Parrot]}
      "arrrrr!".say

Dieses Vorgehen sorgt für eine wesentlich bessere Nachvollziehbarkeit
der bereitgestellten Implicits und macht Debuggen um vieles
einfacher. Zudem ist das Fehlerpotential durch ungewolltes Importieren
verringert.
  
  
## using in Parametern

Implicit Conversions sind nur ein Anwendungsfall des
`implicit`-Keywords. Die Übergabe von impliziten Argumenten in
Funktionen, Methoden und Konstruktoren ist ein beliebtes Mittel, um
Typklassen, Kontext und Dependency-Injection zu realisieren. Die
Definition solcher Objekte funktioniert nun auch über das
`given`-Keyword. Implizite Parameter werden mit dem `using`-Keyword
annotiert:

    object Animals:
    
      case class Parrot(sentence: String):
        def say : Unit = println(sentence)
    
      given parrotOrdering : Ordering[Parrot] with
        override def compare(parrot1 : Parrot, parrot2 : Parrot) : Int =
          -1 * parrot1.sentence.length.compareTo(parrot2.sentence.length)
    
      def doSomethingWithParrots(parrots : List[Parrots])(using ordering : Ordering[Parrot]) = ???
    
      List(Parrot("hello!"), Parrot("bye!")).sortBy {x => x}


Im Beispiel implementieren wir eine Typklasse, die eine Ordnung für
Papageien anhand ihrer Satzlänge definiert. Dazu implementieren wir
das bestehende Trait `Ordering`, das wir in vielen Methoden der
Standard-Bibliothek verwenden können, die eine Ordnung für einen
bestimmten Typ erwarten. `sortBy` ist eine solche Methode und hat
hier einen im Aufruf nicht sichtbaren zweiten Parameter `using
Sorting[...]`. (Anmerkung: Das Keyword `with` ist eine Abkürzung für
`= new Ordering[Parrot]`. Im vorangegangenen Codebeispiel der Implicit
Conversions haben wir `new Conversion[String, Parrot]:` noch
ausgeschrieben.)

Die Definition eines `using`-Parameters sehen wir beispielhaft in
`doSomethingWithParrots`.

Wollen wir den impliziten Parameter explizit übergeben, müssen wir auch
das `using`-Keyword verwenden:

    List(Parrot("hello!"), Parrot("bye!")).sortBy({x => x})(using parrotOrdering)

Dies löst einen Syntax-Konflikt, der mit Implicits und
`apply`-Funktionen in Scala 2 auftritt:

    def someMap(implicit i : Int) : Map[String, Int] = ???
    
    implicit val a : Int = 3
    
    // Erzeugt die Map:
    val m = someMap
    
    // Ruft einen Schlüssel ab:
    val value = someMap(a)("hello")
    
    // Erzeugt einen Compiler-Fehler
    // Der Compiler erwartet hier den impliziten Parameter
    val doesntWork = someMap("hello")




  
## Extension Methods

In Scala sind Methoden das gängige Mittel, um Funktionalität für ein
Objekt oder eine Klasse bereitzustellen. Manchmal hat man keinen
Zugriff auf die Implementierung selbiger oder möchte diese nicht mit
anwendungsfallspezifischem Code verwässern und hat daher das
Bedürfnis, nachträglich Methoden hinzuzufügen. In Scala 2 gibt es
hierzu ein Pattern, das Implicit Conversions verwendet:


    object Main {
      import Animals.Parrot

      implicit class ParrotWithIQ(p : Parrot()) {
         def iq() : Int = p.sentence.length
      }
      
      Parrot.iq()
    }


Zwar verwenden wir hier einen Sonderfall, nämlich implizite Klassen,
jedoch lässt sich das ganze auch mit Konvertierungen, wie beim
`stringToParrot`-Beispiel realisieren.

Da bei der Definition dieses Patterns wieder Mechanik über Vorhaben
steht, hat Scala 3 sogenannte Extension Methods eingeführt. Das
sieht dann so aus:

    object Main:
      import Animals.Parrot
      
      extension (p: Parrot)
        def iq() : Int = p.sentence.length
    
    
      Parrot("Hello!").iq()

Diese Syntax abstrahiert die Implicit-Logik und macht unser Vorhaben
explizit: Wir wollen den Papageien nachträglich erweitern. Dies ist
zum Beispiel wichtig in Typklassen, bei denen man durch das Übergeben
der Typklasse weitere Methoden für bestimmte Typen bereitstellt. Bei
der Implementierung solcher muss in Zukunft keine Implicit Conversion
mehr verwendet werden.

  
## Résumé

Auch bei den Implicits macht Scala 3 wieder einiges besser: Durch
explizitere Syntax und schärfere Trennung wird nachvollziehbarer, was
die Intentionen sind. Mögliche Fehlerquellen werden damit
reduziert. Ob dies auch ausreicht, um die Komplexität in großen
Projekten beherrschbar zu halten, wird sich herausstellen. Langfristig
soll das Keyword `implicit` aus der Sprache verschwinden.

Warum man mit Scala 3 nicht noch expliziter wurde und nur den halben
Weg gegangen ist, ist unverständlich. Wie bei den Extension Methods,
die nun in der Syntax klar ausgetrennt wurden, hätte man auch bei
Conversions eine explizitere Syntax wählen können. Stattdessen findet
die Unterscheidung auf Typebene statt und syntaktisch werden diese
erneut mit den übrigen `given`-Instanzen vermischt. Auch Typklassen
sind in der Implementierung ein wohl definiertes Muster und auch hier
hätte Scala 3 noch expliziter sein dürfen, um diese weiter
abzugrenzen.

Im nächsten Blogpost dieser Reihe werden wir eine weitere spannende
Neuerung von Scala 3 unter die Lupe nehmen: Union Types.

