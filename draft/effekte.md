---
layout: post
title: "Effekte in Ocaml 5"
author: marco-schneider
tags: ["Praxis", "Effekte", "Effects" "Ocaml"]
---

Nach langer Arbeit landete in Version 5 von Ocaml endlich auch die Unterstützung
für sogenannte algebraische Effekte.  In diesem Artikel schauen wir uns an, wie
wir Effekte zur Domänenmodellierung und Entkopplung von der Beschreibung eines
Programms von dessen Auswertung nutzen können.  Obacht: Dieser Artikel setzt ein
bisschen Wissen über den Active Group- und
[funktionale-programmierun.de](https://funktionale-programmierung.de)-Kanon
voraus!

<!-- more start -->

## Ocaml

Obwohl Ocaml eine gut abgehangene und beliebte funktionale Programmiersprache
ist, geht sie außerhalb der Welt der funktionalen Programmierung häufig unter.
Völlig zu Unrecht!  Kollege Sperber hat bereits eine [kleine
Einführung](https://funktionale-programmierung.de/2023/03/30/multicore-ocaml.html)
gegeben, weswegen wir das hier kurz gestalten.

Im verlinkten Artikel wurde schon ein neues Feature von Ocaml 5 besprochen: die
Multicore-Runtime.  Dort ging es um das Programmieren auf mehreren physischen
Kernen.  Heute sehen wir uns einen damit eng verwandten Aspekt von Ocaml 5 an:
das Effektsystem.  Und zwar nicht irgendwie: Wir stellen uns die Frage, wie man
damit ganz konkret Probleme löst.

## Über das *Was* abstrahieren: (freie) Monaden

Wer unseren Blog liest, weiß, dass wir oft und gerne freie Monaden zur
Modellierung von Programmabläufen verwenden:

- [Monaden in Kotline](https://funktionale-programmierung.de/2023/05/22/kotlin-monads.html)
- [Freie Monaden in Clojure](https://funktionale-programmierung.de/2023/04/27/clojure-monads.html)
- [Uhrwerk Freie Monade: Hinter den Kulissen](https://funktionale-programmierung.de/2019/03/04/freie-monade-2.html)
- ...

Wer außerdem aufgepasst hat, der:dem ist vielleicht aufgefallen, dass es
zwischen den Artikeln versteckt ein Thema gab, das wir heute wieder aufgreifen:
die [Einführung in algebraische
Effekte](https://funktionale-programmierung.de/2019/10/24/algebraic-effects.html).

Darum hier nur in aller Kürze: Wir verwenden (freie) Monaden, um unsere
Programmlogik von der Implementierung zu trennen, mit den bekannten Vorteilen.
Es gibt allerdings nicht nur Vorteile.  Gerade freie Monaden bringen ein paar
Eigenheiten mit sich, die (naiv eingesetzt) in der Realität gern zu Problemen
führen.  Meiner Meinung nach ist das vor allem die "horizontale
Komponierbarkeit".  Was meine ich damit?

## Horizontale Komposition von (freien) Monaden

Damit wir alle auf dem selben Stand sind, stellen wir uns folgendes Beispiel
vor.  Wir haben zwei verschiedene Domain Specific Languages (DSLs):

1. unsere [bekannte](https://funktionale-programmierung.de/2023/04/27/clojure-monads.html) Datenbanksprache 
2. eine freie Monad mit der Operation `now`, die uns die aktuelle Zeit liefert

Was passiert nun, wenn ich im selben Programm beide Effekte benutzen will?
Spoiler: Ohne sehr expliziten Aufwand geht das nicht.

``` haskell
prog = do
  timestamp <- now  -- `bind` aus der 'Time'-Monade
  -- `bind` aus der 'Addressbuch'-Monade
  res <- put (Address 23 "Marcus" "Tuebingen"
```

Da wir je einen Interpreter für die beiden Monaden brauchen, können wir das
Programm wie es hier steht, nicht hinschreiben.  Was ich gerne machen würde,
wäre, beide Interpreter nebeneinander zu benutzen -- das meine ich mit
*horizontaler Komposition*.  Hier kommt jetzt das neue Effecs-Feature von Ocaml
ins Spiel.

## Effekte definieren

Wir sind ja alle nicht ganz neu [wenn es um
Ocaml-Syntax](https://funktionale-programmierung.de/2023/03/30/multicore-ocaml.html)
geht.  Trotzdem will ich mein Bestes tun, um euch abzuholen.  Denn: Wir benutzen
tatsächlich fortgeschrittene Funktionaliät, um die man sonst auch gern mal
herumkommt.

### Kleiner Umweg

Wie sein artverwandter Kollege Haskell unterstützt auch Ocaml Generalized
Algebraic Datatypes (GADTs) -- allerdings sind sie in Ocaml immer verfügbar,
nicht erst nach dem Anschalten eine Spracherweiterung.  Dieser Abschnitt soll
nur die Syntax einführen, sodass wir uns weiter unten auf das Wesentliche
konzentrieren können.  Ein Beispiel aus der [Ocaml Dokumentation
(englisch)](https://v2.ocaml.org/manual/gadts-tutorial.html) für eine kleine
DSL für mathematische Ausdrücke:

```ocaml
type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n -> n
  | Add -> (fun x y -> x+y)
  | App(f,x) -> (eval f) (eval x)
```

Was sehen wir hier?  Erst mal das `_` im `type`.  Das bedeutet, dass wir einen
GADT definieren.  Jeder Konstruktor ist explizit wie eine Funktion getypt: `Int`
ist ein Konstruktor, der einen `int` nimmt und einen `int term` zurück gibt.
`Add` ist ein Konstruktor, der einen `(int -> int -> int) term` erstellt, usw.

Wichtig ist auch ein Teil der Typsignatur von `eval`: `type a. ...`.  Das
bedeutet, dass wir hier Allquantifizieren (in Haskell würde hier tatsächlich
`forall a. ...` stehen).  Das bedeutet, dass unser abstrakter Typ `a`
tatsächlich alles sein kann (lies "Für alle Werte A aus `a`, ...").  Wäre das
nicht der Fall, hätten wir eine monomorphe Funktion (gegenüber einer tatsächlich
polymorphen) definiert und der Complier würde sich zurecht über den `App`-Fall
beschweren.  Warum das so ist, ist eine Übung für die:den Leser:in :)

Wie gesagt, es geht nur um die Syntax -- wer mehr darüber wissen will, kann
[hier
(Wikipedia)](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type)
anfangen zu lesen.  Es lohnt sich!

### Back to Topic

Wir kennen also GADTs und die Syntax für explizite polymorphe Definitionen für
lokal abstrakte Typen.  Damit ausgestattet geht es jetzt an die Effekte!

In Ocaml gibt es tatsächlich _einen_ Type für Effekte: `Effect.t`.  Dabei
handelt es sich um eine sogenannte "extensible Variant", also ein Summentyp, dem
man nachträglich weitere Konstruktoren hinzufügen kann.  Konkret sieht das dann
so aus (`Ptime` ist eine verbreitete Bibliothek für Zeitstempel):

```ocaml
open Effect

-- Effecs for generating a timestamp.
type _ Effect.t += Now : unit -> Ptime.t Effect.t

let now () = perform (Now ())
val now : unit -> Ptime.t
```

Wie oben eingeführt sehen wir hier den GADT `Effect.t`, den wir um einen
Konstruktor `Now` erweitern (das *Erweitern* sieht man am `+=`).  Effekte
bringen einen ganzen Werkzeugkoffer mit, von denen `perform` ein wichtiges
Werkzeug ist.  Es sagt nichts weiter als dass, wenn ich `now ()` schreibe, der
Effekt `Now` ausgelöst werden soll.  Wenn ihr an ein `raise` oder `throw` denkt,
liegt ihr richtig, denn in Ocaml sind Effekte eine Generalisierung von
Exceptions.  Und was macht man mit Exceptions (hoffentlich)?  Wir fangen sie
natürlich!

Was ich jetzt noch brauche, ist das Äquivalent eines Interpreters bei freien
Monaden: einen Effekthandler.  Dieser sieht aus oben genanntem Grund einem
Exceptionhandler sehr ähnlich.

In Ocaml sieht das dann so aus:

```ocaml
open Effect
open Effect.Deep

let () =
  let open Effect in
  match_with now ()
    { retc= (fun r -> r)
    ; exnc= (fun e -> raise e)
    ; effc=
	    (* der eigentliche Effekt wird hier behandelt *)
        (fun (type c) (eff : c Effect.t) ->
          match eff with
          | Now () ->
              Some
                (fun (k : (c, _) continuation) ->
                  continue k (Ptime_clock.now ()) )
          | _ -> None ) }
```

Das ist ziemlich viel, also der Reihe nach. `match_with` ist eine Funktion, die
als Argumente eine Funktion (`now`), ein Argument für die Funktion (`()`) und
einen Effekthandler akzeptiert.  Der Effekthandler selbst ist ein Produkt aus
drei Feldern

1. `retc`: Ein Funktion, die ein "normales" Ergebnis bekommt und -- eventuell
     transformiert -- zurück gibt.  Wir geben das Ergebnis unverändert zurück.
2. `exnc`: Ein Funktion, die den Wert einer eventuellen Exception bekommt und
   etwas damit tut.  Auch hier `raise`n wir die Exception nur wieder
   unverändert.  Da kann sich jemand anders drum kümmern.
3. `effc`: Hier ist der interessante Teil.  `effc` ist eine Funktion, die einen
   (allquantifizierten, also *irgendeinen*) Effekt bekommt und ihn auswerten
   kann.  Wir schauen per Pattern Matching nach, um welchen Effekt es sich
   handelt und geben ihm eine Implementierung.  Die Implementierung ist eine
   Funktion `continuation` `k` bekommt, die wir wieder aufnehmen können, indem
   wir sie mit `continue` aufrufen.  Wenn uns der Effekt nicht interessiert (das
   `_`), sagen wir `None`.
   
Das Konzept der delimited continuation ist für viele sicher neu.  Leider haben
wir hier nicht den Platz, das ganze ausführlich zu erklären.  Im Prinzip ist es
aber "ganz einfach".  Bei einer Exception beenden wir den Programmfluss, können
aber nicht wieder dort einsteigen, wo wir aufgehört haben, als die Exception
auftrat.  Eine Continuation ist aber genau das: die Runtime *merkt* sich, wo sie
war (das Stackframe) und erlaubt es uns, nachdem wir den Effekt abgehandelt
haben, dort wieder einzusteigen.

Was ist jetzt der große Vorteil gegenüber freien Monaden?

### Ad-Hoc Effekthandling und horizontale Komposition

Oben habe ich geschrieben, dass wir *irgendeinen* Effekt abhandeln können.  Das
bedeutet, dass wir in jedem einzelnen Effekthandler entscheiden können, für
welche Effekte wir uns genau hier interessieren.  Das bedeutet auch, dass wir
zwar Effekte gerne zu einer DSL bündeln können, so wie wir es mit freien Monaden
auch tun, ihre Interpreter aber völlig problemlos miteinander Komponieren.

```ocaml
let (type c) compose_effect_handlers h1 h2 (eff : c Effect.t) =
  match h2 eff with
  | None -> h1 eff
  | res -> res
```

Wir stellen uns vor, dass `h1` ein Effekthandler für unser `now` ist, `h2` für
unsere Datenbank DSL.  Jetzt können wir sie ohne Probleme *horizontal*
komponieren!

Das bedeutet zwei jetzt im Wesentlichen zwei Dinge:

1. wir können beliebig viele DSLs "nebeneinander" schreiben
2. wir können uns ad-hoc entscheiden, welche Effekte wir ausführen wollen und
   wie

Das macht erhöht den Komport ungemein: bei sich verändernden Anforderungen
können wir sehr leicht Effekte hinzufügen (oder wegnehmen) und müssen dabei viel
weniger anfassen.  Auch haben wir so die Möglichkeit beispielsweise
Defaulteffekthandler zu bauen, denn: Nicht "gefangene" Effekte blubbern wie
Exceptions weiter nach "oben".  Umgekehrt gibt mir das sogar die Möglichkeit,
Effekte lokal anderes zu interpretieren also "weiter oben".

## Fazit

Das kratzt jetzt natürlich nur an der Oberfläche.  Ich hoffe trotzdem, dass
schon hier durchscheint, wie mächtig und flexibel dieses System gegenüber
unseren freien Monaden ist.  Das wird sicher nicht das letzte Mal sein, dass wir
uns hier mit Ocamls Effektsystem beschäftigen.  Für heute haben wir -- so ist
mein Eindruck -- aber erst einmal genug zu verdauen.
