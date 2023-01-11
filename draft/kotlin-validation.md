---
layout: post
description: Funktionale Validierung in Kotlin
title: "Funktionale Validierung in Kotlin"
author: michael-sperber
tags: ["Kotlin", "funktor", "functor", "applikativ", "applicative", "validierung", "validation"]
---

Dieser Post ist der Beginn eine Reihe über *funktionale
Softwarearchitektur in Kotlin*.  Sie entstammt ursprünglich einer
Zusammenarbeit der Active Group mit
[Blume2000](https://www.blume2000.de/), die wir bei der Entwicklung
ihres Webshops beraten haben.  Diesen Post habe ich zusammen mit
[Benedikt Stemmildt](https://benedikt.stemmildt.com/) geschrieben,
seinerzeit CTO bei Blume2000.

Es geht um die Validierung von Daten.  Wir wollen sicherstellen, dass
Objekte in unserem Programm "valide" sind, also beliebig definierbare
Konsistenzkriterien erfüllen, ohne die unsere Software nicht
funktioniert.

Mein Kollege Marco Schneider hatte schon in einem [früheren
Post](https://funktionale-programmierung.de/2022/04/26/validierung-mit-applikativen-funktoren.html)
über Abstraktionen dafür in Haskell geschrieben.

Die gleichen Ideen sind auch - mit Abstrichen - nach Kotlin
übertragbar.  In diesem Post rollen wir das Thema noch einmal neu auf,
und zwar wie wir aus objektorientierter Sicht mit funktionalen
Techniken helfen können. Es ist also nicht notwendig, das
Haskell-Posting zu lesen.  (Wir empfehlen den Post trotzdem wärmstens,
da er insbesondere das Konzepts des *Applicatives* beschreibt, das in
Kotlin unpraktikabel ist.)  Kotlin-Grundkenntnisse werden allerdings
vorausgesetzt.

<!-- more start -->

# Prämisse

Das Validierungsproblem beschreibt Martin Fowler schön in dem Blog-Post
[Martin Fowler: Replacing Throwing Exceptions with Notification in Validations](https://martinfowler.com/articles/replaceThrowWithNotification.html).
Er zitiert dort folgendes Java-Code-Fragment aus einer Klasse, deren
Attribute `date` und `numberOfSeats` bestimmte Eigenschaften haben müssen, damit
die Methoden der Klasse funktionieren:

```java
public void check() {
   if (date == null) throw new IllegalArgumentException("date is missing");
   LocalDate parsedDate;
   try {
     parsedDate = LocalDate.parse(date);
   }
   catch (DateTimeParseException e) {
     throw new IllegalArgumentException("Invalid format for date", e);
   }
   if (parsedDate.isBefore(LocalDate.now())) throw new IllegalArgumentException("date cannot be before today");
   if (numberOfSeats == null) throw new IllegalArgumentException("number of seats cannot be null");
   if (numberOfSeats < 1) throw new IllegalArgumentException("number of seats must be positive");
}
```

Dieser Code ist zwar schnell programmiert und leicht lesbar, hat aber
mindestens zwei Probleme:

* Er wirft eine Exception, wenn ein Problem auftritt.  Stilistisch ist
  es aber sinnvoll, Exceptions primär für Situationen zu benutzen, in
  denen etwas unerwartetes und unabwendbares in der Umgebung der
  Softare passiert ("Datei nicht gefunden").  Hier geht es aber um
  zwar unangenehme aber *erwartbare* Situationen.

* Schwerer wiegt, dass die Methode auf *mehrere* mögliche Probleme
  prüft, beim ersten aber schon aussteigt, also nur *ein* Problem
  melden kann.

Im Java-Umfeld sind deshalb Frameworks etabliert, die ohne Exceptions
auskommen und alle festgestellten Probleme aufsammeln und melden, wie
zum Beispiel der [Hibernate Validator](https://hibernate.org/validator/).
Hier ist ein Code-Beispiel für Validierung mit solche einem Validator,
aus dem Kotlin-Code des Webshops bei Blume2000:

```kotlin
data class Position(
    @field:Min(1, message = "ANZAHL_ZERO") var anzahl: Int,
    @field:Valid var preis: Preis,
    @field:Valid var produkt: Produkt
)
```

Die Dinger mit `@` sind Annotationen, die der Kotlin-Compiler in den
Objektcode für die `Position`-Klasse überträgt, wo sie vom Validator
zur Laufzeit ausgelesen werden.  Zum Beispiel bedeutet die
`@field:Min`-Annotation, dass der Wert des Felds `anzahl` mindestens 1
betragen muss.  Die Annotation `@field:Valid` verweist auf die
Validierungs-Annotationen, die in der `Preis`- beziehungsweise der
`Produkt`-Klasse stehen.

Der Validator wird erst aktiv, wenn er explizit auf einem
`Position`-Objekt aufgerufen wird:

```kotlin
val position: Position = ...
val factory = Validation.buildDefaultValidatorFactory()
val validator = factory.getValidator()
val violations: Set<ConstraintViolation<Position>> = validator.validate(position)
```

Das grundsätzliche Vorgehen ist also folgendes:

1. erstmal ein Objekt erzeugen und
2. checken, ob es valide ist

Funktionalen Programmierys stellen sich bei diesem Vorgehen die
Nackenhaare auf.  Warum denn ein invalides Objekt überhaupt erzeugen?
Wir fühlen uns da an die Szene in
[Alien](https://www.imdb.com/title/tt0078748/?ref_=fn_al_tt_1)
erinnert, wo das Alien ins Raumschiff gelassen wird und erst
*dann* untersucht wird.  (Es geht nicht gut aus.)

Die "Validator-Methode" macht auch ein paar ganz konkrete Probleme:

- Die Validator-Annotationen konstituieren effektiv eine kleine eigene
  Programmiersprache, die man erst lernen muss.
- Die Annotationen koppeln den Code an das Validator-Framework.
- Valide und invalide Objekte haben denselben Typ, das Typsystem kann
  also nicht helfen, sie auseinanderzuhalten.

# Validierung in der funktionalen Programmierung

Entsprechend verfolgen wir in der funktionalen Programmierung einen
anderen Ansatz für Validierung.  Der prominente funktionale
Programmierer [Yaron
Minsky](https://blog.janestreet.com/author/yminsky/) (Technikchef bei
[Jane Street](https://www.janestreet.com/)) hat folgenden Ausspruch
geprägt:

"Make illegal states unrepresentable."

Minsky spricht nicht direkt von Validierung sondern von der Verwendung
des Typsystems: In stark getypten Sprachen sollten wir unsere Typen so
gestalten, dass möglichst nur konsistente, "legale" Daten überhaupt
erzeugt werden können.

Das nur mit dem Typsystem durchzusetzen ist nicht immer möglich, zum
Beispiel bei Zahlen, die aus einem bestimmten Zahlenbereich kommen
müssen.  Oft benötigen wir zum Beispiel einen Typ für natürliche (also
nicht-negative ganze) Zahlen, es gibt aber in vielen Sprachen als Typ
für ganze Zahlen nur `int`.  Um sicherzustellen, dass es sich dabei um
eine nicht-negative Zahl handelt, müssen wir deshalb etwas zur
Laufzeit tun.

Aber auch zur Laufzeit können wir Minskys Credo folgen und verhindern,
dass "invalide" Objekte überhaupt erst erzeugt werden.  Um das für den
`Position`-Typ von oben umzusetzen, schreiben wir statt der
"offiziellen" Konstruktor-Funktion `Position:invoke` 
eine spezielle, validierende Konstruktor-Funktion (beziehungsweise
"Factory-Methode" im OO-Sprech) etwa so:

```kotlin
class Position {
  companion object {
    fun of(anzahl: Int, preis: Preis, produkt: Produkt) =
        if (anzahl >= 1)
          ...
        else
          ...
  }
```

Wenn wir nicht - wie in Martin Fowlers abschreckendem Beispiel - im
`else`-Fall eine Exception werfen wollen, müssen wir das Ergebnis der
Validierung im Rückgabetyp von `of` unterbringen.  Zu diesem Zweck
verwenden wir die Kotlin-FP-Library [Arrow](https://arrow-kt.io/), die
allerlei nützliche funktionale Abstraktionen enthält - insbesondere
den Typ
[`Validated`](https://arrow-kt.io/docs/apidocs/arrow-core/arrow.core/-validated/).
(Die Dokumentation von Arrow ist leider zum Zeitpunkt der Drucklegung
dieses Artikels etwas elliptisch.) `Validated` ist folgendermaßen
definiert:

```kotlin
sealed class Validated<out E, out A>
data class Valid<out A>(val value: A) : Validated<Nothing, A>
data class Invalid<out E>(val value: E) : Validated<E, Nothing>
```

Damit kann ein `Validated`-Wert entweder mit der Klasse `Valid` einen
validen Wert vom Typ `A` kapseln oder mit `Invalid` eine Beschreibung
eines Validierungs-Fehlers vom Typ `E`.  Damit können wir `of`
folgendermaßen vervollständigen:

```kotlin
fun of(anzahl: Int, preis: Preis, produkt: Produkt)
    : Validated<List<ValidationErrorDescription>, Position> =
    if (anzahl >= 1)
      Valid(Position(anzahl, preis`, produkt))
    else
      Invalid(listOf(MinViolation(preis, 1)))
```

Der Typ `ValidationErrorDescription` ist hier nicht aufgelistet - er
enthält Klassen mit Beschreibungen möglicher Validierungsfehler.  Wir
benutzen hier eine ganze Liste davon, weil ja bei der Konstruktion
eines einzigen komplexen Objekts *mehrere* Validierungsfehler
auftreten können.

# Pragmatik

In der Praxis würde man aber noch zwei Veränderungen machen:

Zunächst hat Arrow zwei "Extension Functions" definiert, die an *jede*
Klasse Methoden `valid` und `invalid` dranklebt, welche die
Konstruktoren aufrufen:
   
```kotlin
fun <A> A.valid(): Validated<Nothing, A> = Valid(this)
fun <E> E.invalid(): Validated<E, Nothing> = Invalid(this)
```

Damit würde der obige Code idiomatisch so aussehen:

```kotlin
fun of(anzahl: Int, preis: Preis, produkt: Produkt)
    : Validated<List<ValidationErrorDescription>, Position> =
    if (anzahl >= 1)
      Position(anzahl, preis, produkt).valid()
    else
      listOf(MinViolation(preis, 1)).invalid()
```

(Ich persönlich finde das verwirrend.)

Des weiteren enthalten die meisten Benutzungen von `Validated` im
Fehlerfall eine Liste, genauer gesagt eine nichtleere Liste.  Dafür
hält Arrow einen Convenience-Typalias bereit:

```kotlin
public typealias ValidatedNel<E, A> = Validated<NonEmptyList<E>, A>
```

(Der Typ
[`NonEmptyList`](https://arrow-kt.io/docs/arrow/core/nonemptylist/)
ist auch bei Arrow mitgeliefert.)

Die "Extension Functions" `valid` und `ìnvalid` funktionieren für
`ValidatedNel` leider nicht unverändert, da gibt es Extra-Funktionen
`validNel` und `invalidNel`.  Mit denen sieht der Konstruktor `of`
endgültig so aus:

```kotlin
fun of(anzahl: Int, preis: Preis, produkt: Produkt)
    : ValidatedNel<ValidationErrorDescription, Position> =
    if (anzahl >= 1)
      Position(anzahl, preis, produkt).validNel()
    else
      MinViolation(preis, 1).invalidNel()
```

Soweit zur Konstruktion von `Validated`.  Um ein `Validated`-Objekt zu
verarbeiten, reicht ein `when`:

```kotlin
when (Position.of(anzahl, preis, produkt)) {
  is Valid -> ...
  is Invalid -> ...
}
```

(`Validated` hat auch noch eine `fold`-Methode, die aber der
Lesbarkeit nicht unbedingt dienlich ist.)

Wer tiefer in Arrow hineinschaut, sieht, dass - in Anlehung an das
Haskell-Package
[`validation`](https://hackage.haskell.org/package/validation) - es
möglich ist, eine beliebige Halbgruppe für den Typ `E` zu benutzen.
Das ist aber in Kotlin für die Praxis zu umständlich, da die
Halbgruppe nicht automatisch inferiert wird.  Außerdem reicht in aller
Regel eh `ValidatedNel`.

Um Tests zu schreiben, bei denen man schon weiß, dass die Objekte
valide sind, benutzen wir in der Regel eine Convience-Funktion wie
diese hier:

```kotlin
class ValidationException(message: String, val error: Any) : IllegalArgumentException(message)

@Throws(ValidationException::class)
fun <E, A> Validated<E, A>.get(): A = 
  this.valueOr { throw ValidationException("Validated expected to be valid", it as Any) }
```

Die `valueOr`-Methode ist in Arrow eingebaut und liefert entweder den
validen Wert in einem `Validated` oder ruft die übergebene Funktion
auf, die in diesem Fall eine Exception wirft.

Damit können wir in Tests einfach solchen Code schreiben:

```kotlin
val position: Position = Position.of(...).get()
```

# Komposition

Vielleicht hast Du Dich gefragt, warum `of` nicht als Argumente
jeweils `ValidatedNel<..., Preis>` und `ValidatedNel<..., Produkt>`
nimmt.  Schließlich müssen Preis und Produkt wahrscheinlich auch
validiert werden.  Das ist allerdings nicht nötig, da wir ja dem Credo
folgen, invalide `Preis`- und `Produkt`-Objekte gar nicht erst zu
erzeugen - `Preis` und `Produkt` *sind* also implizit bereits
validiert.

Trotzdem müssen wir uns was überlegen, wie wir die Validierungen für
`Preis` und `Produkt` mit der von `Position` zusammenschalten.  (Das
passiert in Haskell mit Hilfe der Applicative-Abstraktion, die aber in
Kotlin zu umständlich zu benutzen wäre.)  Stellen wir uns also vor, es
gebe auch noch Factory-Methoden für `Preis` und `Produkt`:

```kotlin
class Preis {
  companion object {
    fun of(...): ValidatedNel<ValidationErrorDescription, Preis> = ...
  }
}

class Produkt {
  companion object {
    fun of(...): ValidatedNel<ValidationErrorDescription, Produkt> = ...
  }
}
```

Um jetzt ein `Preis`- und ein `Produkt`-Objekt so parallel zu
validieren, dass etwaige Fehler zu kombinieren, stellt Arrow ein Reihe
von "Extension Functions" namens `zip`.  Jede von denen akzeptiert
eine bestimmte Anzahl von `Validated`-Werten und wendet eine Funktion
auf die Ergebnisse an, falls möglich.  Zum Beispiel hier das
dreistellige `zip`:

```kotlin
public inline fun <E, A, B, C, Z> ValidatedNel<E, A>.zip
   (b: ValidatedNel<E, B>, c: ValidatedNel<E, C>, f: (A, B, C) -> Z)
   : ValidatedNel<E, Z>
```

Das ist etwas gewöhnungsbedürftig, weil das erste Argument vor dem
`.zip` steht und alle weiteren in den Klammern danach.  Das könnten
wir so aufrufen:

```kotlin
Preis.of(...).zip(Produkt.of(...))
  { preis, produkt -> Produkt(anzahl, preis, produkt) }
```

Das funktioniert aber leider nur, wenn wir den Konstruktor von
`Produkt` direkt aufrufen.  Wir wollen aber natürlich `Produkt.of`
verwenden, das selbst wieder ein `Validated` zurückliefert.  Leider
bietet Arrow da nicht so die richtig praktische Abstraktion.  Es geht
am einfachsten mit einer eigenen Hilfsfunktion, die `zip` benutzt, um aus den
beiden validierten Zwischenergebnissen ein Paar zu konstruieren:

```kotlin
fun <Z, A, B> validate(
  a: ValidatedNel<Z, A>,
  b: ValidatedNel<Z, B>
): ValidatedNel<Z, Pair<A, B>> = a.zip(b) { validA, validB ->
  Pair(validA, validB)
}
```

Das geht entsprechend auch für Tupel höherer Stelligkeit.

Das "validierte Paar" aus `validate` können wir dann mit der
"Extension Function" `andThen` verarbeiten, die bei Arrow dabei ist:

```kotlin
public fun <E, A, B> Validated<E, A>.andThen(f: (A) -> Validated<E, B>): Validated<E, B> =
  when (this) {
    is Validated.Valid -> f(value)
    is Validated.Invalid -> this
  }
```

So geht das:

```kotlin
validate(Preis.of(...), Produkt.of(...))
  .andThen { (preis, produkt) -> Produkt.of(anzahl, preis, produkt) }
```

(Die `validate`-Funktion hat den zusätzlichen Vorteil, anders als
`zip` symmetrisch zu sein.)

Zu beachten ist bei `andThen` - genau wie in Haskell auch - dass diese
Funktion zwar die gleiche Signatur hat wie ein monadisches `bind`
beziehungsweise `flatMap`, aber damit keine Monade gebildet wird:
`andThen` akkumuliert die Fehler nicht.

# Fazit

Funktionale Validierung benötigt nur einen einfachen Datentyp und ein
paar Methoden darauf und kommt ohne DSL oder Annotationen auf.  Da
"valide" und "invalide" durch unterschiedliche Objekte ausgedrückt
wird, könnte man sie auch "objektorientierter Validierung" nennen.
Eine gute Idee ist sie allemal.

In Kotlin bringt Arrow die richtigen Abstraktionen mit, einzig an
Dokumentation und Convenience fehlt es noch ein bisschen.
