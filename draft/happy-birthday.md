---
layout: post
description: Jahresrückblick
title: "Jahresrückblick"
author: stefan-wehr
tags: ["Rückblick"]
---

Vor ziemlich genau einem Jahr haben wir das Blog "Funktionale Programmierung" gestartet.
Zusammen mit einigen Gastautoren berichten in diesem Blog die Firmen
[Active Group](http://www.active-group.de/) und [Factis Research](http://www.factisresearch.com/)
über Aspekte der funktionalen Programmierung, die für unser Arbeitsleben von Bedeutung sind. 

In diesem Artikel zum Jahrestag möchten wir ein kurzes Résumé ziehen und einen
Überblick über die Artikel des zurückliegenden Jahres geben. Hoffentlich finden Sie dadurch
noch den einen oder anderen für Sie interessanten Artikel, der Ihnen im Lauf des Jahres "durch die Lappen"
gegangen ist.

Am Schluss des Artikels finden Sie auch einige interessante Zahlen zum Blog und seinen Besuchern.
Wir möchten uns an dieser Stelle schon einmal ganz herzlich bei Ihnen, unseren vielen Lesern bedanken
und Sie um Ihr Feedback bitten. 

* Was gefällt Ihnen an diesem Blog?
* Was kann noch verbessert werden? 
* Worüber würden Sie gerne mehr erfahren? 

Schreiben Sie Antworten auf diese Fragen doch einfach in einen kurzen Kommentar am Ende des Artikels. Danke!
Jetzt aber viel Spaß beim Lesen dieses Überblicks.

<!-- more start -->

In unserem [Archiv](/archive.html) finden Sie ja bereits eine chronologische Übersicht aller Artikel
des Blogs, wahlweise auch nach [Autoren sortiert](/author-archive.html). Ich möchte an dieser
Stelle die Artikel des zurückliegenden Jahres daher auch mal inhaltlich einordnen.

## Grundlagen der funktionalen Programmierung ##

Wir haben einige Artikel, die sich an Einsteiger der funktionalen Programmierung richten.
So gibt [David Frese](http://www.active-group.de/unternehmen/frese.html) in seinem Artikel
eine Antwort auf die Frage [Was ist funktionale Programmierung?](http://funktionale-programmierung.de/2013/08/23/was-ist-funktionale-programmierung.html)
Mit vielen Beispielen versehen präsentiert [Michael Sperber](http://www.active-group.de/unternehmen/sperber.html) eine
dreiteilige [Einführung in die rein funktionale Programmierung](http://funktionale-programmierung.de/2013/03/12/rein-funktional.html),
hier finden Sie den [2.](http://funktionale-programmierung.de/2013/04/10/rein-funktional-2.html)
und [3.](http://funktionale-programmierung.de/2013/04/25/rein-funktional-3.html) Teil.
    
## Java und Sprachen auf der JVM ##

Viele industrielle Softwareentwickler sind mit objektorientierter Programmierung in Java und der JVM als Ausführungsplatform
vertraut. Auch auf dieser Plattform kann man funktional programmieren!
In einem [Artikel](http://funktionale-programmierung.de/2013/02/26/scala-java-ant.html) sehen
Sie anhand eines konkreten Beispiels aus dem Quellcode des Buildsystem [Apache Ant](http://ant.apache.org/),
wie Java-Code wesentlich lesbarer in der funktionalen JVM-Sprache [Scala](http://www.scala-lang.org/)
aufgeschrieben werden kann. [Andreas Bernauer](http://www.active-group.de/unternehmen/bernauer.html)
zeigt, ebenfalls in Scala, wie man in dieser Sprache [übersichtlich und performant](http://funktionale-programmierung.de/2013/03/26/scala-java-performance.html)
programmieren kann.
In einem [Beispiel aus der Praxis](http://funktionale-programmierung.de/2013/06/13/funktionale-api-jasper.html) 
zeigen wir, wie mit Hilfe funktionaler Programmierung eine schöne, verständliche und saubere API für
[JasperReports](http://www.jaspersoft.com/reporting), eine bekannte Bibliothek aus der Java-Welt
zum Erzeugen von Berichten, erstellt werden kann.
 
Neben Scala gibt es noch eine Reihe anderer
funktionaler Sprachen für die JVM. Ein [Artikel](http://funktionale-programmierung.de/2013/06/27/dsl-clojure.html) stellt die Sprache
[Clojure](http://clojure.org/) vor und zeigt, wie man dort einfach kleine Spezialsprachen entwicklen kann.
Unser Gastautor [Ingo Wechsung](http://www.contexo.de/) stellt die Sprache [Frege](http://funktionale-programmierung.de/2013/10/10/frege.html)
vor, ein [Haskell](http://haskell.org)-Dialekt für die JVM.

## Funktionales in imperativen Sprachen ##

Auch wenn Sie selbst in der täglichen Arbeit keine Möglichkeit haben, funktionale Sprachen einzusetzen,
können Sie in imperativen und objekt-orientierten Sprachen von funktionalen Denk- und Programmiertechniken
profitieren, wie [Stefan Wehr](http://www.factisresearch.com/company.html#swehr) in seinem Artikel
[Warum funktional?](http://funktionale-programmierung.de/2013/03/20/warum-funktional.html)
zeigt. Auch enthält Java 8 einige längst überfällige
[funktionale Features](http://funktionale-programmierung.de/2013/09/19/java8.html).
[Niklas Baumstark](http://www.factisresearch.com/company.html#nbaumstark) demonstriert, dass man selbst in C++ dank
[persistenter Datenstrukturen](http://funktionale-programmierung.de/2013/06/21/persistente-datenstrukturen.html)
von funktionaler Programmierung profitieren kann.

Unveränderbare ("immutable") Datentypen sind eine wichtige Technik der funktionalen Programmierung, so wichtig
dass wir auch in [Objective-C](http://funktionale-programmierung.de/2013/12/05/datentypen-objectivec.html)
nicht darauf verzichten möchten. Sollten Sie aber mal in der funktionalen Sprache Haskell des Bedürfnis
verspüren, imperativ zu programmieren, hilft Ihnen [dieser Artikel](http://funktionale-programmierung.de/2013/08/01/haskell-imperativ.html)
unseres Gastautors [Joachim Breitner](http://www.joachim-breitner.de/blog/) bestimmt weiter.

## Industrielle Anwendungen ##

Funktionale Programmierung wird auch vermehrt in industriellen Anwendungen eingesetzt.
So zeigen [zwei](http://funktionale-programmierung.de/2013/05/16/praxis-myownsafe.html) 
[Artikel](http://funktionale-programmierung.de/2013/09/05/praxis-myownsafe-2.html), wie wir ein Webprojekt
zur elektronischen Nachlassverwaltung schnell und kostensparend mit funktionaler Programmierung
umsetzen konnten. Auch lässt sich [medizinische Datenverarbeitung](http://funktionale-programmierung.de/2013/07/17/medizin-funktional.html)
hervorragend mit funktionaler Programmierung realisieren.

Doch nicht nur unsere Firmen setzen auf funktionale Programmierung. Auch Banken wie [Credit Suisse](https://www.credit-suisse.com/de/de/)
und [Standard Chartered](https://www.sc.com/de/) oder Internetriesen wie [Twitter](http://twitter.com) und
[Facebook](http://facebook.com) setzen inzwischen auf diese Technik. Warum Facebook
auf die funktionale Sprache [Haskell](http://haskell.org) setzt, erfahren Sie in
[diesem Artikel](http://funktionale-programmierung.de/2013/10/02/haskell-facebook.html).

## Testen ##

Obwohl wir hier in diesem Blog immer wieder über die Vorzüge funktionaler Programmierung in 
Bezug auf Softwarequalität und geringe Fehlerraten berichten, muss man auch in funktionalen
Projekten selbstverständlich testen. Wir haben uns in diesem
Blog mit randomisierten Tests im Sinne von [QuickCheck](http://funktionale-programmierung.de/2013/07/10/randomisierte-tests-mit-quickcheck.html)
beschäftigt. Außerdem stellen wir in einem weiteren Artikel ein
[Testframework für Haskell](http://funktionale-programmierung.de/2014/02/06/testing-haskell.html) vor,
welches in unserer Arbeit täglich zum Einsatz kommt.
    
## Web-Programmierung ##

Natürlich zollt dieser Blog auch dem Thema Web-Programmierung Tribut. Zum einen stellt
[Alexander Thiemann](http://www.factisresearch.com/company.html#nthiemann) in
einem Artikel Techniken für
[moderne Webanwendungen mit Haskell](http://funktionale-programmierung.de/2013/04/04/webanwendung-haskell.html)
vor und vertieft diese in einem
[zweiten Teil](http://funktionale-programmierung.de/2013/06/05/webanwendung-haskell2.html).
Zum anderen diskutiert ein andere Artikel
[erste Schritte in ClojureScript](http://funktionale-programmierung.de/2014/02/14/clojurescript-react.html)
und beschäftigt sich dabei hauptsächlich mit Web-Programmierung.
Dass man auch in JavaScript, der Lingua franca des Webs, funktional programmieren kann (wenn auch mit gewissen
Einschränkungen), zeigt das [Curry-Buch](http://www.currybuch.de/), welches wir in einer
[Buchbesprechung](http://funktionale-programmierung.de/2013/07/25/curry-buch.html) vorstellen.

## Parallelität und Nebenläufigkeit ##

Funktionale Programmiersprachen sind besonders gut für paralleles und nebenläufiges Programmieren
geeignet. Dies liegt insbesondere am sparsamen und disziplinierten Umgang mit Seiteneffekten sowie
am expliziten Datenfluss über Funktionsparameter statt globaler Variablen.
[Ein Artikel](http://funktionale-programmierung.de/2013/03/06/parallel-haskell.html) zu diesem
Thema erklärt, wie man in Haskell Parallelität einführen kann und die Programme dabei trotzdem
deterministisch bleiben. Ein [weiterer Artikel](http://funktionale-programmierung.de/2013/05/08/haskell-nodejs.html)
demonstriert, dass man mit Haskell auf 
ganz natürlich Art und Weise nebenläufige Programme schreiben kann, die performancemäßig und auch
softwaretechnisch dem beliebten Framework [node.js](http://nodejs.org/) überlegen sind.

## Praktische Beispiele ##

In einem weiteren Beispiel aus der Praxis erklärt unser Gastautor [Johannes Weiß](http://www.johannesweiss.eu/)
wie einfach die Interaktion zwischen Haskell und Aufrufen von C-Bibliotheksfunktionen realisiert werden kann.
Außerdem demonstriert [ein anderer Artikel](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html)
den Einsatz von Haskell zur Implementierung eines Buildsystems, welches Tag für Tag unsere Software kompiliert.
Das Für und Wider für die Platform [Eclipse Xtend](http://funktionale-programmierung.de/2014/01/23/eclipse-xtend.html)
wird ebenfalls aus praktischer Sicht beleuchtet.

## Monaden ##

Kein Blog über funktionale Programmiersprachen ohne Monaden! Unser Gastautor [Uwe Schmidt](http://www.fh-wedel.de/~si/)
bringt den Lesern in einer dreiteiligen, didaktisch hervorragend aufbereiteten Artikelserie die Funktionsweise
und Vorzüge von Moden näher ([Teil 1](http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html), 
[Teil 2](http://funktionale-programmierung.de/2013/05/22/haskell-monaden2.html), 
[Teil 3](http://funktionale-programmierung.de/2013/07/03/haskell-monaden3.html)).
Praktische Einsatzmöglichkeiten von Monaden werden außerdem in einem
[Artikel](http://funktionale-programmierung.de/2013/05/29/gui-monade.html) über eine Monade zur GUI-Programmierung
vorgestellt. Unser Gastautor [Torsten Grust](http://db.inf.uni-tuebingen.de/team/grust/) zeigt außerdem, wie man mit Hilfe von Monaden
[Datenbankabfragen repräsentieren](http://funktionale-programmierung.de/2014/02/19/comprehending-queries.html) kann.

## Klassiker der funktionalen Programmierung ##

Neben Monaden gibt es eine Reihe weiterer Klassiker der funktionalen Programmierung:
[Tail Calls](http://funktionale-programmierung.de/2013/11/08/tail-calls.html), auch bekannt als Endrekursion,
dienen in funktionalen Sprache als mächtige Alternative zu Schleifen.
In eine ähnliche Kerbe schlägt der Artikel über
[Schleifen in Scala](http://funktionale-programmierung.de/2013/10/23/schleifen-scala.html).
[Continuations](http://funktionale-programmierung.de/2013/10/31/continuations-praxis.html) erlauben
den Zugriff auf die "Fortsetzung" eines Programms und eröffnen damit ungeahnte Möglichkeiten. 
Mit Hilfe von Parser-Kombinatoren lassen sich
Parser für Textformate sehr leicht implementieren, wie anhand eines
[Parsers für HL7-Nachrichten](http://funktionale-programmierung.de/2013/08/29/hl7-parser.html)
gezeigt wird. [Ein weiterer Artikel](http://funktionale-programmierung.de/2013/11/21/expression-problem.html)
zeigt, wie man erweiterbare Programme in funktionalen Sprachen schreibt.

## Veranstaltungen ##

Auch im zurückliegenden Jahr gab es eine Vielzahl von Events mit dem Thema
"Funktionale Programmierung". Wir haben in diesem Blog nur über einen Bruchteile dieser Veranstaltungen
berichtet, und zwar über die, an denen wir selbst beteiligt waren. So gibt es einen Artikel über
den [Zurich FP Afternoon / Haskell-Hackathon](http://funktionale-programmierung.de/2013/08/15/haskell-hackathon.html),
welcher im September 2013 in Zürich stattfand. Zum "Commercial Users of Functional Programming" Workshop
gibt es einen [Vorbericht](http://funktionale-programmierung.de/2013/08/07/cufp-2013.html)
sowie eine [Nachbetrachtung](http://funktionale-programmierung.de/2013/12/12/cufp-2013-report.html).
Die in der Nachbetrachtungen verlinkten [Videos](http://www.youtube.com/channel/UCfSUv7I_aHgzcnXMcd8obsw)
demonstrieren sehr gut die Vorteile und die breiten Einsatzmöglichkeiten funktionaler Programmierung.
    
## Ein paar Zahlen zum Blog ##

Elf Autoren, darunter fünf Gastautoren, haben im zurückliegenden Jahr insgesamt 43 Artikel für diesen Blog verfasst.
Jeder Artikel hatte dabei mehr als 70 Leser, die Hälfte der Artikel sogar mehr als 235 Leser.
Die Top-3-Artikel hatten sogar jeweils mehr als 1.000 Leser. Dabei sind die Top-3-Artikel (in dieser Reihenfolge):
[Monaden: Das programmierbare Semikolon](/2013/04/18/haskell-monaden.html),
[Haskell schlägt node.js](/2013/05/08/haskell-nodejs.html) sowie
[Funktional Programmieren am konkreten Beispiel](/2013/02/26/scala-java-ant.html).

Insgesamt haben über 8.000 Menschen das Blog "Funktionale Programmierer" im letzten Jahr gelesen,
insgesamt kamen dabei mehr als 23.000 Seitenaufrufe zusammen.
Wir haben dabei 30-40 Besucher pro Tag, an Tagen an den Artikel veröffentlich werden sind es
mindestens 60 Besucher, oft aber auch 80-120 Besucher. (Die Besucherzahlen bezeichnen dabei immer
*eindeutige* Besucher, mehrfache Aufrufe derselben Person werden also nur einmal gezählt.)

Wir möchten Ihnen, unseren Lesern, an dieser Stelle sehr herzlich für Ihre Treue danken. Wir hoffen, dass Sie auch in Zukunft
unseren Blog lesen und die Inhalte interessant finden. Damit wir unsere Artikel noch besser auf Ihre Interessen abstimmen können,
möchten wir Sie an dieser Stelle um Ihr Feedback bitten.

* Was gefällt Ihnen an diesem Blog?
* Was kann noch verbessert werden? 
* Worüber würden Sie gerne mehr erfahren? 

Schreiben Sie Antworten auf diese Fragen doch einfach in einen kurzem Kommentar. Danke!

<!-- more end -->

