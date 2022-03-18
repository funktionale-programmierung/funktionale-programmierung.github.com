---
layout: post
description: Scala 3 Union- & Intersection-Types
title: "Scala 3: Über Vereinigungen und Schnittmengen"
author: simon-haerer
tags: ["Scala", "Scala 3", "union", "intersection", "types", "type", "with"]
---


Nach 8 Jahren, 28000 Commits und 7400 Pull-Requests war es am 14. Mai
2021 endlich so weit: Scala 3 wurde veröffentlicht. Neben dem neuen
Compiler „Dotty“ haben es eine neue Syntax sowie einige Neuerungen an
der Sprache in Scala 3 geschafft. In diesem Blogpost der Serie über
interessante Neuerungen werden wir über Union- und Intersection-Types
sprechen. Zwar existierte für Intersection-Types bereits ein
eingeschränkter Mechanismus, doch Union-Types sind gänzlich neu in
Scala 3. Wie die neuen Typen verwendet werden können, welche
Möglichkeiten diese bieten und wie sie sich zu aus Scala 2 bekannten
Typen abgrenzen, wird in diesem Blogpost erörtert.

<!-- more start -->

## Union-Types

Einer der zwei Typklassen, die in diesem Blogpost beleuchtet werden
sollen, sind Union-Types oder auch Vereinigungen. Es wird anhand des
`|`-Operators ein neuer Typ definiert, der die Vereinigung mehrerer
Typen repräsentiert:

    object UnionTypes:
      type StringOrInt = String | Int


Ein Union-Type wird durch eine Aufzählung von zwei oder mehreren
Typen, getrennt durch einen Trennstrich, definiert. Der Typ
`StringOrInt` ist entweder ein `Int` oder ein `String`. Ein Wert von
diesem Typ kann mögliche Werte aus der Menge aller Strings oder der
Menge aller Integer annehmen.

Definiert werden Werte vom Typ `StringOrInt` wie folgt:

    val a : StringOrInt = "a"
    val b : StringOrInt = 3

Der Typ von `a` und `b` muss hier explizit als `StringOrInt` angegeben
werden, da der Compiler sonst dessen Subtypen `String` oder `Int`
inferiert. Der Scala 3 Compiler erlaubt es, auf Union-Types
erschöpfend zu matchen. Dabei wird, wie bei algebraischen Summentypen,
eine Compilerwarnung ausgegeben, wenn nicht alle Fälle abgedeckt
wurden:

    def foo(x : StringOrInt) =
        x match
            case _ : String => "Hello " + x



In diesem Beispiel würde gewarnt, dass _non-exhaustive_ gematcht wird und
angezeigt, dass der Typ `Int` dabei nicht abgedeckt ist. 
    
Union-Types vereinigen also Mengen von möglichen Werten, die von den
Subtypen angenommen werden können. Im folgenden Beispiel ist es daher
bei einem vorliegenden Wert auf Typeebene unmöglich zu unterscheiden,
ob ein Benutzername oder eine ID gemeint ist. Der echte Typ
von `UsernameOrId` ist `String`:

    object UnionTypes:
        type Username = String
        type Id = String

        type UsernameOrId = Username | Id


In diesem Fall sollten sogenannte _tagged unions_ oder auch
Summentypen verwenden werden:

    object SumTypes:

       enum UsernameOrId:
           case Username(name: String)
           case Id(id: String)


Der Nachteil hierbei ist, dass sich dieser Typ im Nachhinein nicht
flach erweitern lässt. Wollen wir noch die E-Mail-Adresse als
möglichen Wert an anderer Stelle hinzufügen, muss ein neuer
Summentyp implementiert werden, der entweder Obiges dupliziert, oder
aber schachtelt:

    object OtherSumTypes:
       enum UsernameOrIdOrEmail:
           case EMail(email: ???)
           case UsernameOrId(usernameOrId: UsernameOrId)
    

 Das sieht doch wirklich nicht schön aus! 

 Mit Union-Types ist die flache Erweiterung hingegen kein Problem:

     object OtherUnionTypes:
         type EMail = ???
         type UsernameOrIdOrEmail = UnionTypes.UsernameOrId | EMail

 Union-Types stellen sich also als wesentlich flexibler heraus. So
 können beispielsweise auch in Funktionsdefinitionen die
 Eingabeparameter ad hoc einfach um zusätzliche Typen erweitert werden.

 Union-Types sind kommutativ, das bedeutet, dass `A | B` den gleichen
 Typ beschreibt, wie `B | A`. Zudem ist der Operator assoziativ: `A |
 (B | C)` ist äquivalent zu `(A | B) | C`.


## Intersection-Types

Intersection-Types stellen die Schnittmenge von Typen dar. Das
bedeutet, dass wir neue Typen definieren können, die als Menge aller
möglichen Werte die Schnittmengen der Werte anderer Typen haben. Sinn
ergibt das vorallem in Verbindung mit Traits und Mixins:


    trait Editable:
       def edit : Unit
    
    trait Deletable:
       def delete : Unit
    
    object IntersectionTypes:
       def foo(entity : Editable & Deletable) : Unit =
           entity.edit
           entity.delete

Wir implementieren eine Funktion `foo`, die den Typ des
Eingabeparameters `entity` derart definiert, dass dieser `Editable`
_und_ `Deletable` ist. Dazu wird der neue `&`-Operator
verwendet. Ein Objekt mit dem gewünschten Typ kann zum Beispiel durch 
`case object SomeEntity extends Editable with Deletable` definiert werden.
    
Wie Union-Types sind Intersection-Types assoziativ und
kommutativ. Intersection-Types gab es in Scala 2 bereits, ausgedrückt
durch den `with`-Operator. 


## Resume

Union- und Intersection-Types sind willkommene Erweiterungen in
Scala 3. Eine sprechende Syntax und klar definierte Specs 
erlauben eine intuitive Verwendung und bringen mehr Flexibilität in
die Sprache. Während es in Scala 2 bereits so etwas wie
Intersection-Types gab, freuen wir uns darauf, die neuen Möglichkeiten,
die Union-Types bieten in verschiedenen Projekten auszuschöpfen.

Im nächsten Blogpost dieser Reihe werden wir eine weitere spannende
Neuerung von Scala 3 besprechen: Type Lambdas.
