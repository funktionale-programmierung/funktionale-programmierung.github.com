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

Dieser Artikel zeigt den Einsatz von
[Monaden](http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html)
in der Praxis um GUI-Fenster zu beschreiben.

Für einen Kunden, der in der Sozialpädagogik tätig ist, entwickelt die
ActiveGroup eine Software zur Dokumentation von Therapiemaßnahmen.
Die Software erfasst die Stammdaten von Personen, die sich einer
Therapie unterziehen, lässt die Personen Fragebägen ausfüllen, um den
aktuellen Zustand und die Fortschritte der Therapie zu dokumentieren,
und erlaubt das Erstellen zahlreicher Berichte.

Zur Entwicklung der Fragebögen verwenden wir eine Monade, welche die
Darstellung des Fragebogens beschreibt und die Antworten einsammelt.
Wieso eine Monade?  Wir sehen folgende Vorteile:

- **Komponierbarkeit** Die Elemente des Fragebogens lassen sich unabhängig
  voneinander beschreiben und anschließend beliebig miteinander
  kombinieren.  Damit lassen sich unterschiedliche Logiken im
  Ablauf eines Fragebogens darstellen, z.B. wenn Frage 1 mit 'ja'
  beantwortet wurde, geht es weiter auf Seite 2, ansonsten auf Seite
  3.

- **Testbarkeit** Da die Monade den Fragebogen nur *beschreibt*, lässt
  er sich relativ leicht testen.  Statt Maus-Klicks zu synthetisieren,
  wertet man die Callbacks der Knöpfe innerhalb der Monade aus und
  testet ihre Auswirkungen auf die Beschreibung.

In diesem Artikel möchte die Komponierbarkeit anhand eines Beispiels
illustrieren; die Testbarkeit und Implementierung überlasse ich
späteren Artikeln. Als Beispiel dient ein kleiner Fragebogen, der
festellt, ob der Befragte am Montag Zeit hat und falls ja, um wieviel
Uhr.

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

Diese vier Scala-Zeilen setzen in die erste Zeile den Fragetext,
gefolgt von zwei Knöpfen für "Ja" und "Nein":

![Erste Seite der Umfrage](/files/gui-monade/seite1.png "Erste Seite der
 Umfrage")

Die `put`-Methode setzt das Inhaltselement in die angegebene Zeile und
Spalte.  Das zugrunde liegende Layoutmodell zur Darstellung des
Fragebogens basiert auf Swings'
[GridBagLayout](http://docs.oracle.com/javase/tutorial/uiswing/layout/gridbag.html),
welches in Scala mit einem Panel zu einem
[GridBagPanel](http://www.scala-lang.org/api/2.10.0/scala/swing/GridBagPanel.html)
kombiniert ist.  Anderer Render-Möglichkeiten sind durchaus denkbar
(z.B. als HTML-Seite), doch die haben wir bisher nicht umgesetzt.

Die `>>`-Methode ist eine der beiden `bind`-Methoden der Monade: sie
verknüpft zwei Monaden, wobei sie das Ergebnis der ersten verwirft.
Die `put`-Methode setzt also nicht wirklich etwas, sondern liefert
eine Monade, welche das Setzen eines Inhaltelements darstellt.  Man
spricht auch von einem Monaden-Operator, da die Methode in der Monade
agiert.  Mit `>>` verknüpfen wir das Setzen mehrerer Inhaltselemente.

Hier sieht man schon die ersten Anzeichen der Komponierbarkeit: jeder
`put`-Aufruf steht gewissermaßen für sich und erst `bind` komponiert
die `put`.  Ganz stimmt das natürlich nicht, denn die `put` achten
ja darauf, Elemente nicht in die selbe GridBag-Zelle zu platzieren.
Deutlicher wird es, wenn wir die optionale Frageseite nach der Uhrzeit
implementieren.

Als Inhaltselemente tauchen im obigen Beispiel `Text` und `Button`
auf.  Wie die Namen schon suggerieren, stellt ersteres einen
(nicht-editierbaren) Text dar, während letzteres einen Knopf mit einer
Beschriftung und einem Callback darstellt.  Beides sind nicht Teil von
Swing, sondern Platzhalter unser GUI-Monade.  Erst bei der Darstellung
werden die Swing-Elemente erzeugt.

Als Callback verwenden wir im obigen Beispiel `addAnswer`.  Die
`addAnswer`-Methode fügt der Monade eine Antwort hinzu.  Ich habe also
die GUI-Monade um die Funktionalität erweitert, sich Antworten auf
Fragen zu merken.  Im obigen Beispiel geschieht dies für die Frage
`montagZeitQ`.  `montagZeitQ` ist als Auswahlfrage definiert mit den
Auswahlmäglichkeiten `Ja` und `Nein`:

{% highlight scala %}
  case object Ja extends AnswerChoice(0)
  case object Nein extends AnswerChoice(1)

  val montagZeitQ = ChooseOneQ(1, Set(Ja, Nein))
{% endhighlight %}

Die Zahlen geben die Repräsentation der Auswahlmöglichkeiten und der
Frage an, wenn sie in einer Datenbank gespeichert werden.

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
Monaden-Operator) und teilen der Monade mit `ask` mit, dass wir nun
fertig mit der Beschreibung sind und die GUI aktualisert und
dargestellt werden kann.  `showSurvey` macht auch genau dies: sie
wertet die Manipulationen aus, welche wir mit `put` und `setTitle` an
der Monade vorgenommen haben und stellt die resultierende GUI dar.


## Logik im Ablauf eines Fragebogens

Wenn die erste Frage mit "ja" beantwortet wurde, soll auf der zweiten
Seite nach der Uhrzeit gefragt werden:

{% highlight scala %}
    val uhrzeitQ = EnterTextQ(3)
    val seite2 =
      put(1, 1, Text("Um wieviel Uhr?")) >>
      put(1, 2, TextField(onValueChanged={t => addAnswer(uhrzeitQ, EnteredText(t)) >> ask}),
        fill=Fill.Horizontal, weightX=1.0) >>
      put(1, 3, Button("Weiter", doNothing))
{% endhighlight %}

Die zweite Seite stellt die Freitext-Frage nach der Uhrzeit
(`uhrzeitQ` vom Typ `EnterTextQ`) und gibt ein Textfeld (`TextField`)
an, in dem die Text-Antwort (`EnteredText`) dafür eingegeben werden
kann:

![Zweite Seite der Umfrage](/files/gui-monade/seite2.png "Zwetie Seit
 der Umfrage")

Das Textfeld hat einen Callback `onValueChanged`, der immer
aufgerufen wird, wenn sich der Inhalt des Textfeldes ändert.  Als
Parameter erhält der Callback den aktuellen Textinhalt und als
Ergebnis liefert er eine Monade.

In diesem Beispiel trägt der Callback die Antwort auf die
Freitext-Frage ein und bleibt mit `ask` auf der aktuellen Frageseite.
Weiter geht es mit dem "Weiter"-Knopf, der nichts tut
(Monaden-Operator `doNothing`) und damit die Kontrolle an die Stelle
zurückgibt, die `seite2` irgendwo eingefügt hat.

Hier sehen wir einen weiteren Aspekt der Komponierbarkeit: wo es nach
`seite2` weitergeht ergibt sich erst durch entsprechende Komponierung
mit dem `bind`-Operator.  Im bekannten
[Model-View-Controller](http://de.wikipedia.org/wiki/Model_View_Controller)-Pattern
(MVC) würde dies im Controller stattfinden.  Statt eines expliziten
Controllers ergibt sich der Programablauf hier durch das
Zusammenstellen der Komponenten mit `bind`.

Nun zur Programmablauf-Logik: Die zweite Seite wollen wir ja nur
darstellen, falls die erste Frage mit "ja" beantwortet wurde.  Wie die
erste Frage beantwortet wurde, können wir über den Monaden-Operator
`getAnswer` feststellen.  Die kleine Umfrage erweitern wir also wie
folgt:

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
das Ergebnis der vorherigen Monade liefert.  Falls die Antwort auf die
erste Frage, ob man am Montag Zeit habe, mit "Ja" beantwortet wurde,
holen wir uns die eingegebene Uhrzeit von Seite zwei mit
`answers(uhrzeitQ)` und geben einen entsprechenden Text aus.
Andernfalls geben wir aus, dass der Befragte am Montag keine Zeit
hatte.

Auch diese Operation lässt sich mit dem bestehenden Fragebogen
kombinieren zu:

{% highlight scala %}
     val kleineUmfrage3 =
       kleineUmfrage2 >> printAnswers
{% endhighlight %}

Nach diesem Prinzip haben wir der Software zur Dokumentation von
Therapiemaßnahmen vier weitere Fragebögen hinzugefügt, die teilweise
über 100 Fragen auf entsprechend vielen Frageseiten stellen.  Die
Fragebögen haben wir dabei von "unten nach oben" aufgebaut: die
einzelnen Seiten werden programmatisch sowohl erzeugt als auch
anschließend zum gesamten Fragebogen zusammengesetzt.  Obschon wir MVC
nicht klassisch umgesetzt haben, sind das Modell (in der Monade), die
Präsentation (GridBagLayout) und die Kontrolle (Callbacks und `>>=`)
voneinander getrennt und lassen sich relativ einfach erweitern.

Mit dem vereinfachten Beispiel aus der Praxis habe ich den Vorteil der
Komponierbarkeit unserer Architektur mit GUI-Monaden illustriert.
Falls gewünscht, kann ich in späteren Artikeln noch auf die
Testbarkeit und die Implementierung eingehen.  Wer Lust hat, kann
gerne in den Kommentaren eine alternative Architektur in der
Programmiersprache seiner Wahl skizzieren.  Über Fragen, Anregungen
oder Anmerkungen freue ich mich ebenfalls, sei es per Kommentar oder
per Email.

