---
layout: post
description: 
title: "Die GUI-Monade: Monaden in der Praxis"
author: andreas-bernauer
tags: ["Praxis", "Monade", "Scala"]
---

<!-- Kommentare
links: [text](link)

    Code1
    Code2

{% highlight xml %}
   <payresult>
     <rc>300</rc>
   </payresult>
{% endhighlight %}
-->

Dieser Artikel zeigt den Einsatz von Monaden in der Praxis um
GUI-Fenster zu beschreiben.

Für einen Kunden, der in der Sozialpädagogik tätig ist, entwickelt die
ActiveGroup eine Software zur Dokumentation von Therapiemaßnahmen.
Die Software erfasst die Stammdaten von Personen, die sich einer
Therapie unterziehen, lässt die Personen Fragebägen ausfüllen, um den
aktuellen Zustand und die Fortschritte der Therapie zu dokumentieren,
und erlaubt das Erstellen zahlreicher Berichte.

Zur Entwicklung der Fragebögen verwenden wir eine Monade, welche die
Darstellung des Fragebogens beschreibt und die Antworten einsammelt.
Wieso eine Monade?  Wir sehen folgende Vorteile:

- **Komponierbarkeit** Die Seiten des Fragebogens lassen sich unabhängig
  voneinander beschreiben und anschließend beliebig miteinander
  kombinieren.  Damit lassen sich unterschiedliche Logiken im
  Ablauf eines Fragebogens darstellen, z.B. wenn Frage 1 mit 'ja'
  beantwortet wurde, geht es weiter auf Seite 2, ansonsten auf Seite
  3.

- **Testbarkeit** Da die Monade den Fragebogen nur *beschreibt*, lässt
  er sich relativ leicht testen.  Statt Maus-Klicks zu synthetisieren,
  wertet man die Callbacks der Knöpfe innerhalb der Monade aus und
  testet ihre Auswirkungen auf die Beschreibung.

Als Beispiel implementieren wir im Folgenden einen kleine Fragebogen,
der festellt, ob der Befragte am Montag Zeit hat und falls ja, um
wieviel Uhr.

<!-- more start -->

## Beschreibung einer Seite eines Fragebogens

Die erste Seite des Fragebogens stellt fest, ob der Befragte überhaupt
am Montag Zeit hat:

{% highlight scala %}
   val seite1 =
     put(1, 1, Text("Haben Sie am Montag Zeit?")) >>
     put(1, 2, Button("Ja", addAnswer(montagZeitQ, ChosenOne(Ja)))) >>
     put(1, 3, Button("Nein", addAnswer(montagZeitQ, ChosenOne(Nein))))
{% endhighlight %}

Diese vier Zeilen setzen in die erste Zeile den Fragetext,
gefolgt von zwei Knöpfen für "Ja" und "Nein".

Die `put`-Methode setzt das Inhaltselement in die angegebene Zeile und
Spalte.  Das zugrunde liegende Layoutmodell zur Darstellung des
Fragebogens basiert auf Swings'
[GridBagLayout](http://docs.oracle.com/javase/tutorial/uiswing/layout/gridbag.html),
welches in Scala mit einem Panel zu einem
[GridBagPanel](http://www.scala-lang.org/api/2.10.0/scala/swing/GridBagPanel.html)
kombiniert ist.

Die `>>`-Methode ist eine der beiden `bind`-Methoden der Monade: sie
verknüpft zwei Monaden, wobei sie das Ergebnis der ersten verwirft.
Die `put`-Methode setzt also nicht wirklich etwas, sondern liefert
eine Monade, welche das Setzen eines Inhaltelements darstellt.  Man
spricht auch von einem Monadenoperator, da die Methode in der Monade
agiert.  Mit `>>` verknüpfen wir das Setzen mehrerer Inhaltselemente.

Als Inhaltselemente tauchen im obigen Beispiel `Text` und `Button`
auf.  Wie die Namen schon suggerieren, stellt ersteres einen
(nicht-editierbaren) Text dar, während letzteres einen Knopf mit einer
Beschriftung und einem Callback darstellt.

Als Callback verwenden wir im obigen Beispiel `addAnswer`.  Die
`addAnswer`-Methode fügt der Monade eine Antwort hinzu, ist also
selbst eine Monade.  Im obigen
Beispiel geschieht dies für die Frage `montagZeitQ`.  `montagZeitQ`
ist als Auswahlfrage definiert mit den Auswahlmäglichkeiten `Ja` und
`Nein`:

{% highlight scala %}
  case object Ja extends AnswerChoice(0)
  case object Nein extends AnswerChoice(1)

  val montagZeitQ = ChooseOneQ(1, Set(Ja, Nein))
{% endhighlight %}

Die fertige "Umfrage" können wir dann wie folgt definieren und
starten:

{% highlight scala %}
    val kleineUmfrage1 =
      setTitle("Kleine Umfrage") >>
      seite1 >>
      ask
     
    showSurvey(emptySurveyGUI(new UI.Dimension(400,150)), kleineUmfrage1)
{% endhighlight %}

Hier setzen wir noch den Titel der Umfrage (`setTitle` ist wieder ein
Monadenoperator) und teilen der Monade mit `ask` mit, dass wir nun
fertig mit der Beschreibung sind und die GUI aktualisert und
dargestellt werden kann.  `showSurvey` macht auch genau dies: sie
wertet die Manipulationen aus, welche wir mit `put` und `setTitle` an
der Monade vorgenommen haben und stellt die resultierende GUI dar:

![Erste Seite der Umfrage](/code/gui-monade-seite1.png "Erste Seite der
 Umfrage")

## Logik im Ablauf eines Fragebogens

Wenn die erste Frage mit "ja" beantwortet wurde, soll auf der zweiten
Seite nach der Uhrzeit gefragt werden:

{% highlight scala %}
    val uhrzeitQ = EnterTextQ(3)
    val seite2 =
      put(1, 1, Text("Um wieviel Uhr?")) >>
      put(1, 2, TextField(onValueChanged=({t => addAnswer(uhrzeitQ, EnteredText(t)) >> ask})),
        fill=Fill.Horizontal, weightX=1.0) >>
      put(1, 3, Button("Weiter", doNothing))
{% endhighlight %}

Die zweite Seite stellt die Freitext-Frage (`EnterTextQ`) für
`uhrzeitQ` und gibt ein Textfeld (`TextField`) an, in dem die
Text-Antwort (`EnteredText`) dafür eingegeben werden kann. Das
Textfeld hat einen Callback, der immer aufgerufen wird, wenn sich der
Inhalt des Textfeldes ändert.  Als Parameter erhält er den aktuellen
Textinhalt und als Ergebnis liefert er eine Monade; hier fügt sie die
Antwort auf die Freitext-Frage ein und bleibt mit `ask` auf der
aktuellen Frageseite.  Weiter geht es mit dem "Weiter"-Knopf, der
nichts tut (Monaden-Operator `doNothing`) und damit die Kontrolle an
die Stelle zurückgibt, die `seite2` irgendwo eingefügt hat.

Die zweite Seite wollen wir jedoch nur darstellen, falls die erste
Frage mit "ja" beantwortet wurde.  Wie die erste Frage beantwortet
wurde, können wir über den Monadenoperator `getAnswer` feststellen.
Die kleine Umfrage erweitern wir also wie folgt:

{% highlight scala %}
    val kleineUmfrage2 =
      kleineUmfrage1 >>
      (getAnswer(montagZeitQ) >>= {
        case ChosenOne(Ja) =>
          clear >> seite2 >> ask
        case ChosenOne(Nein) => doNothing
      })
{% endhighlight %}

Falls die erste Frage mit "Ja" beantwortet wurde, löschen wir mit
`clear` die GUI und zeigen die zweite Seite an.  Andernfalls tun wir
nichts.  `kleineUmfrage1` konnten wir also sehr leicht erweitern,
ohne dass wir deren Definition ändern mussten.  `showSurvey` sollte
nun natürlich `kleineUmfrage2` darstellen.

## Interaktion mit der Umgebung

Obschon wir mit Monaden arbeiten, hindert uns nichts daran,
Seiteneffekt zu produzieren, falls gewünscht.  Zum Beispiel könnten
wir am Schluss der Umfrage das Ergebnis in eine Datenbank schreiben
oder, wie im folgenden Beispiel, auf der Konsole ausgeben:

{% highlight scala %}
  val printResults =
    getAnswers >>= {answers =>
      answers(montagZeitQ) match {
        case ChosenOne(Ja) =>
          val uhrzeit = answers(uhrzeitQ) match {
            case EnteredText(t) => t
          }
          println("Hat am Montag um %s Uhr Zeit.".format(uhrzeit))
        case ChosenOne(Nein) => println("Hat am Montag keine Zeit.")
      }
      doNothing
    }
{% endhighlight %}

Mit `getAnswers` erhalten wir alle Antworten als eine `Map` von Frage
auf Antwort.  Dabei ist `>>=` der `bind`-Operator der Monade, welche
das Ergebins der vorherigen Monade liefert.  Falls die Antwort auf die
erste Frage, ob man am Montag Zeit habe, mit "Ja" beantwortet wurde,
holen wir uns die eingegebene Uhrzeit von Seite zwei mit
`answers(uhrzeitQ)` und geben einen entsprechenden Text aus.
Andernfalls geben wir aus, dass der Befragte am Montag keine Zeit
hatte.

Auch diese Operation lässt sich leicht mit dem bestehenden Fragebogen
kombinieren zu:

{% highlight scala %}
     val kleineUmfrage3 =
       kleineUmfrage2 >> printAnswers
{% endhighlight %}

## Testen

WIP

{% highlight scala %}
{% endhighlight %}


In einem weiteren Blogpost werde ich die Implementierung der Monade
besprechen.



