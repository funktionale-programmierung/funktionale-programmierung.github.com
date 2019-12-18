---
layout: post
description: "Algebraische-Effekte-2"
title: "Weihnachtswichteln mit algebraischen Effekten"
author: simon-haerer
tags: ["algebraische Effekte", "Effekte", "algebraische", "weihnachten", "wichteln", "Koka", "Monadentransformer", "Seiteneffekt", "Seiteneffekte", "algebraic effects", "algebraic", "effects"]
---

Weihnachten steht vor der Tür und der Geschenkewahnsinn beginnt. Viele
Feierwillige entscheiden sich jedoch dazu, nicht daran teilzunehmen. Konzepte
wie zum Beispiel das Weihnachtswichteln, bei dem eine Person eine zufällig
ausgeloste Person beschenkt, bieten einen schönen Mittelweg. Wie uns
algebraische Effekte bei der Zuteilung von Schenkenden zu Beschenkten helfen
können, erfahren wir in diesem Blogpost. Dazu lernen wir den Zufallseffekt
kennen und sehen, wie mehrfaches Zurückspringen in den Code funktioniert. Zum
Verständnis dieses Artikels sollte der interessierte Leser bereits [den ersten
Teil zu algebraischen Effekten in
Koka](https://funktionale-programmierung.de/2019/10/24/algebraic-effects.html)
gelesen und verstanden haben.

<!-- more start -->


# Weihnachtswichteln mit Paaren

Beim Weihnachtswichteln geht es darum, anstelle aller Feiernden nur eine
zufällig ausgewählte Person zu beschenken. Oft wird die Auslosung so gestaltet,
dass geheim bleibt, wer wen beschenkt. Nehmen Paare teil, ist es manchmal
erwünscht, dass diese sich nicht gegenseitig beschenken, da sie sich meist
sowieso beschenken. Um die wunderbare Welt der algebraischen Effekt mit
weihnachtlicher Stimmung zu verbinden, werden wir eine automatische Zuteilung
für die Weihnachtswichtel-Zuteilung in Koka programmieren. Es gelten die
folgenden festlichen Anforderungen:

1. Jeder Teilnehmer schenkt genau einmal

2. Jeder Teilnehmer wird genau einmal beschenkt

3. Paare beschenken sich nicht gegenseitig

4. Niemand beschenkt sich selbst

5. Die Zuteilung ist zufällig


Um eine Zuteilung zu finden, implementieren wir zunächst eine sehr einfache
Version, die unter Zuhilfenahme des Zufallseffekts versucht, eine zufällige
Zuteilung zu erstellen. Sollte diese den festlichen Anforderungen genügen, so
gibt sie die Zuteilung zurück, anderenfalls fällt das Weihnachtswichteln aus.


# Frohe Helferlein

Zuerst modellieren wir die passende Domäne:


    // Eine Person ist ein string, der deren Namen repräsentiert
    alias person = string

    // Ein Teilnehmer ist eines der folgenden:
    // - ein Single, das eine Person repräsentiert.
    // - ein Paar, das zwei Personen repräsentiert, 
    //   welche sich gegenseitig nicht beschenken dürfen.
    type participant {
      Single(p : person)
      Pair(p1 : person, p2 : person)
    }

    // Eine Zuordnung besteht aus zwei Personen, 
    // wobei die erste die zweite beschenkt.
    alias matching = (person, person)


Wir definieren nun einige Hilfsfunktionen, die wir später benötigen werden:

    // Überprüft ob eine Zuordnung einem Paar aus `participants` entspricht
    fun is-pair(matching : matching, participants : list<participant>) : <exn, div> bool {
      val (z1, z2) = matching
      match(participants){
        Cons(Pair(p1, p2), xs) -> {
          val matching-is-pair = z1 == p1 && z2 == p2 || z1 == p2 && z2 == p1
          matching-is-pair || is-pair(matching, xs)
        }
        Cons(Single(__), xs) -> is-pair(matching, xs)
        Nil -> False
      } 
    }
   
    // Überprüft ob eine Zuordnung aus nur einer Person besteht
    fun is-self-giving(matching : matching) {
      val (z1, z2) = matching
      z1 == z2
    }
    
    // Überprüft ob die Zuordnung nicht selbst-scheckend ist und
    // kein Paar mit den selben Personen vorhanden ist
    fun is-valid-matching(matching : matching, participants : list<participant>) {
      !is-self-giving(matching) && !is-pair(matching, participants)
    }
    

Die Funktion `is-pair` überprüft, ob eine Zuordnung unter einer gegebenen
Teilnehmerliste Anforderung 3 erfüllt, also eine Zuordnung kein Paar ist.
Dazu ruft sie sich so lange rekursiv auf, bis dies für alle Einträge der
Teilnehmerliste überprüft wurde. In Koka geben möglicherweise nicht endende
Berechnungen, in diesem Fall ein rekursiver Aufruf, den Effekt `div` zurück.
Dieser ist im Core eingebaut und muss nicht gesondert behandelt werden. Die
Funktion `is-self-giving` überprüft, ob Anforderung 4 erfüllt wird.
Schlussendlich überprüft die Funktion `is-valid-matching`, ob eine Zuordnung
beiden Anforderungen genügt.

# Fröhliche Effekte überall

Der einfachste Weg, eine mögliche Zuordnung zu finden, ist es, zufällig Paare zu
generieren und zu überprüfen, ob die Zuordnungen valide sind. Das Generieren von
Zufallszahlen ist seiteneffektbehaftet, in Koka benötigen wir dafür einen
Effekt:


    effect random {
      fun random-pair(a : list<person>, b : list<person>) : (person, person)
    }


Dieser algebraische Effekt nimmt zwei Listen von Personen entgegen und soll ein
zufällig gewähltes Element aus der ersten und eines aus der zweiten zurückgeben.


    // Hilfsfunktion um alle Personen aus einer Liste von Teilnehmern zu extrahieren
    fun flatten-participants(participants: list<participant>) : <> list<person>  {...}

    // Hilfsfunktion um ein Element aus einer Liste zu entfernen
    fun remove-person(l: list<person>, element : person) : list<person> {...}

    // Implementierung der Wichtel-Funktion
    fun find-matching-helper(not-yet-giving: list<person>, 
                             not-yet-receiving: list<person>, 
                             participants: list<participant>,
                             matchings: list<matching>) : <div, random, exn> maybe<list<matching>> {
      match( not-yet-giving ) {
        Cons(_,_) -> {
          val matching = random-pair(not-yet-giving, not-yet-receiving)
          if(is-valid-matching(matching, participants)) then {
             val (giver, receiver) = matching
             find-matching-helper(
                remove-person(not-yet-giving, giver),
                remove-person(not-yet-receiving, receiver),
                participants,
                Cons(matching, matchings)
             )
          } else {
            Nothing 
          }}
        Nil -> Just(matchings)}}

    // Findet vielleicht eine Zuteilung von Weihnachtswichteln
    fun find-matching(participants : list<participant>) : <div, random, exn> maybe<list<matching>>{
      val persons = flatten-participants(participants) 
      find-matching-helper(persons, persons, participants, [])
    }


Die Implementierung der Wichtel-Zuteilung finden wir in der `find-matching-helper`
Funktion. Diese nimmt zwei Listen von Personen entgegen: `not-yet-giving`
beinhaltet alle Personen, die noch nicht schenken, `not-yet-receiving` alle, die
noch nicht beschenkt werden. Aufgrund der Anforderungen 1 und 2 müssen wir beide
Listen mitführen. Die Liste `participants` beinhaltet Informationen darüber, in
welchen Verhältniss die Personen stehen und wird benötigt, um Anforderung 3 zu
überprüfen. In `matchings` werden die gefundenen Zuteilungen mitgeführt. 

Zunächst überprüfen wir, ob `not-yet-giving` noch Elemente beinhaltet. Implizit
nehmen wir an, dass `not-yet-giving` und `not-yet-receiving` die gleiche Länge
haben und somit entweder beide Elemente beinhalten oder beide leer sind.

Beinhalten die Listen Elemente, lösen wir den `random-pair`-Effekt aus. Dieser
gibt eine Zuteilung zurück die wir mithilfe von `is-valid-matching` prüfen. Ist
die Zuteilung valide, wird `find-matching-helper` rekursiv mit den neuen
Zwischenständen aufgerufen. Ist sie invalide, wird die Ausführung einfach
abgebrochen. Es wird `Nothing` zurückgegeben und damit signalisiert, dass keine
Zuteilung gefunden werden konnte.

Haben wir keine Elemente mehr in den beiden Personenlisten, geben wir das
akkumulierte Ergebniss, dass sich in `matches` befindet, verpackt in `Just`, zurück.

# Effekthandler unter dem Christbaum

Mit folgendem Effekthandler für den `random-pair`-Effekt lässt sich nun mit sehr
viel weihnachtlichem Glück am Fest der Wunder ein passendes Wichtel-Setup
finden:


    // Gibt ein zufälliges Element aus einer Liste zurück
    fun random-element(x : list<a>) : <ndet, exn> a {
      val r-idx = random-int() % x.length
      match(x[r-idx]){
        Just(a) -> a
        Nothing -> throw("List is empty")
      }
    }

    // Handler für den random Effekt, gibt bei random-pair 
    // ein Tuple aus einem zufälligen Element der ersten 
    // und einem zufälligen Element der zweiten Liste zurück
    val random-pair-handler = handler {
      return x -> x 
      random-pair(a, b) -> resume((random-element(a), random-element(b)))
    }
    

Die Funktion `random-element` selektiert ein zufälliges Element aus einer Liste.
Die eingebaute Funktion `random-int()` gibt einen zufälligen Integer zurück und
lößt den Effekt `ndet` aus, der im Koka-Core vorhanden ist und nicht von uns
abgehandelt werden muss. Er beschreibt nicht-deterministische Ereignisse, wie
das Generieren von Zufallszahlen. Der `random-pair-handler` springt also mit
einem zufälligen Element beider Listen dahin zurück, wo der Effekt ausgelöst
wurde.

    // Main-Funktion, berechnet eine Weihnachtswichtel-Zuordnung
    public fun main()  {
      val participants = [Pair("Aaron", "Jaqueline"), Pair("Denise", "Blake"),
                          Single("Timothy"), Single("O'Shaughnessy")]

      val r = random-pair-handler({find-matching(participants)})

      print-matching(r) // Implementierung in Github
    }


Mit etwas Wichtel-Glück erhalten wir nach einigen Versuchen möglicherweise die
folgende Zuteilung: `(Jaqueline, Timothy), (Denise, O'Shaughnessy), (Blake, Aaron),
(Timothy, Denise),` `(Aaron, Blake), (O'Shaughnessy, Jaqueline)`, wobei stets die
erste Person die zweite beschenkt.


# Effektwunder

Nun haben wir eine sehr einfache Version der Zuteilung implementiert. Wie könnte
eine intelligente, algorithmische Version aussehen? Eine Möglichkeit wäre, die
Zuteilung anhand eines Graphenalgorithmus zu finden. Allerdings gibt es noch
eine einfachere Methode. Effekthandler erlauben es mehrfach in den Code
zurückzuspringen. Warum also probieren wir nicht alle Wichtel-Zuteilungen durch?
Dazu müssen wir nicht einmal die Logik der Implementierung anpassen, sondern nur
den Effekthandler:

    // Handler für den random Effekt, spring bei random-pair 
    // mehrfach in den Code zurück
    val random-pair-handler-on-steroids = handler {
      return x -> [x]
      random-pair(a, b) -> {
        val x = random-element(a)
        b.foldl([], fun(acc, y){acc + resume(x, y)})
      }
    }

Dieser Handler selektiert ein zufälliges Element aus der ersten Liste und
springt mithilfe der schon bekannten `resume`-Anweisung mehrfach zurück zum
Aufrufer. Dies erreichen wir, indem wir über alle Elemente in `b` falten und mit
diesen gepaart mit dem zufälligen Element aus `a` zurückspringen. 

Ein Handler hat stets einen Rückgabewert. Hier ist dies, abgesehen von Effekten,
eine Liste von optionalen Listen von Zuteilungen (`list<maybe<list<matching>>>`).
Daher verpacken wir den von `return` abgefangenen Wert in eine Liste und hängen in
der `foldl`-Methode die einzelnen Listen anhand des `+`-Operators aneinander.

Verwenden wir anstelle des alten Handlers unseren
`random-pair-handler-on-steroids`, bekommen wir eine Liste mit allen möglichen
validen Zuteilungen zurück.


# Und die Moral von der Geschichte

Natürlich ist das Programm ineffizient. Es berechnet *alle* möglichen
Zuteilungen. Bereits bei 20 Teilnehmern dauert es über 30 Minuten, alle
Zuteilungen zu berechnen. Nichtsdestotrotz sollen in diesem Blogpost
zwei Botschaften illustriert werden:

1. Es ist möglich, mehrfach aus einem Handler zum Aufrufer des Effekts
zurückzuspringen. Das eröffnet ungeahnte Möglichkeiten, in unserem Beispiel in
der Kombinatorik. Auch die Abbildung algorithmischer Kontrollflüsse ist denkbar.
Wie sich mehrfache Rückspringe in der Praxis bewähren werden, bleibt zu zeigen.
Ein übermäßiger Einsatz dieser Technik führt möglicherweise zu schwer
nachzuvollziehenden Kontrollflüssen.

2. Der einfache Austausch unseres Effekthandlers hatte große Auswirkungen auf
das Programm. Dazu mussten wir nicht einmal die eigentliche Programmlogik
abändern. Das bedeutet, dass wir anhand von Handlern über Logik abstrahieren
können, die dann einfach austauschbar ist. Dies ist hilfreich beim Testen oder
beim Austausch von Technologien. Generell lässt sich sehr einfach Beschreibung
und Implementierung entkoppeln.

Der vollständige Code ist wie immer [auf
Github](https://github.com/smoes/algebraic-effects-blogpost-2/blob/master/code.kk)
zu finden---damit steht Weihnachten nichts mehr im Wege!
