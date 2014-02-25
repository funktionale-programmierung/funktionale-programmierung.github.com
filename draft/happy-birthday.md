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

In diesem Artikel zum Jahrestag möchten wir ein kurzes Resumee ziehen und einen
Überblick über die Artikel des zurückliegenden Jahres geben. Hoffentlich finden Sie dadurch
noch den einen oder anderen für Sie interessanten Artikel, der Ihnen im Lauf des Jahres "durch die Lappen"
gegangen ist.

Am Schluss des Artikels finden Sie auch einige interessante Zahlen zum Blog und seinen Besuchern.
Wir möchten Sie an dieser Stelle schon jetzt um Ihr Feedback bitten. Was gefällt Ihnen an diesem Blog?
Was kann noch verbessert werden? Worüber würden Sie gerne mehr erfahren? Schreiben Sie Antworten
auf diese Fragen doch einfach in einen kurzem Kommentar am Ende des Artikels. Danke!

<!-- more start -->

In unserem [Archiv](/archive.html) finden Sie ja bereits eine chronologische Übersicht aller Artikel
des Blogs, wahlweise auch nach [Autoren sortiert](/author-archive.html). Ich möchte an dieser
Stelle die Artikel des zurückliegenden Jahres daher auch mal inhaltlich einordnen.

## Grundlagen der funktionalen Programmierung ##

Wir haben einige Artikel, die sich an Einsteiger der funktionalen Programmierung richtet.
So gibt [David Frese](http://www.active-group.de/unternehmen/frese.html) in seinem Artikel
eine Antwort auf die Frage [Was ist funktionale Programmierung?](http://funktionale-programmierung.de/2013/08/23/was-ist-funktionale-programmierung.html).
Mit vielen Beispielen versehen gibt [Michael Sperber](http://www.active-group.de/unternehmen/sperber.html) eine
dreiteilige [Einführung in die rein funktionale Programmierung](http://funktionale-programmierung.de/2013/03/12/rein-funktional.html),
hier finden Sie den [2. ](http://funktionale-programmierung.de/2013/04/10/rein-funktional-2.html)
und [3.](http://funktionale-programmierung.de/2013/04/25/rein-funktional-3.html) Teil.
    
## Java und Sprachen auf der JVM ##

Viele industrielle Softwareentwickler sind mit objektorientierter Programmierung mit Java und der JVM
vertraut. Auch auf dieser Platform kann man wunderbar funktional programmieren!
In einem [Artikel](http://funktionale-programmierung.de/2013/02/26/scala-java-ant.html) sehen
Sie anhand eines konkreten Beispiels aus dem Quellcode des Buildsystem [Apache Ant](http://ant.apache.org/),
wie Java-Code wesentlich lesbarer in der funktionalen Sprache [Scala](http://www.scala-lang.org/)
aufgeschrieben werden kann. [Andreas Bernauer](http://www.active-group.de/unternehmen/bernauer.html)
zeigt, ebenfalls in Scala, wie man dort [übersichtlich und performant](http://funktionale-programmierung.de/2013/03/26/scala-java-performance.html)
programmieren kann.

In einem [Beispiel aus der Praxis](http://funktionale-programmierung.de/2013/06/13/funktionale-api-jasper.html) 
zeigen wir, wie mit Hilfe funktionaler Programmierung eine schöne, verständliche und saubere API für
[JasperReports](http://www.jaspersoft.com/reporting), eine bekannte Bibliothek aus der Java-Welt
zum Erzeugen von Reporten, erstellen werden kann. Zum Implementieren einer solchen API
ist es nützlich zu wissen, dass es in der funktionalen Programmierung eine
[mächtige Alternative](http://funktionale-programmierung.de/2013/10/23/schleifen-scala.html) zu den
in Java häufig verwendenten Schleifen gibt.

Neben Scala gibt es noch eine Reihe anderer
funktionaler Sprachen für die JVM. Ein [Artikel](http://funktionale-programmierung.de/2013/06/27/dsl-clojure.html) stellt die Sprache
[clojure](http://clojure.org/) vor und zeigt, wie man dort einfach kleine Spezialsprachen entwicklen kann.
Unser Gastautor [Ingo Wechsung](http://www.contexo.de/) stellt die Sprache [Frege](http://funktionale-programmierung.de/2013/10/10/frege.html)
vor, eine [Haskell](http://haskell.org)-Dialekt für die JVM vor.

## Funktionales in imperativen Sprachen ##

Auch wenn Sie selbst in der täglichen Arbeit keine Möglichkeit haben, funktionale Sprachen einzusetzen,
können Sie in imperativen und objekt-orientierten Sprachen von funktionalen Denk- und Programmiertechniken
profitieren, wie [Stefan Wehr](http://www.factisresearch.com/company.html#swehr) in [seinem Artikel](http://funktionale-programmierung.de/2013/03/20/warum-funktional.html)
zeigt. Auch enthält Java 8 einige längst überfällige
[funktionale Features](http://funktionale-programmierung.de/2013/09/19/java8.html).
[Niklas Baumstark](http://www.factisresearch.com/company.html#nbaumstark) demonstriert, dass man selbst in C++ dank
[persistenter Datenstrukturen](http://funktionale-programmierung.de/2013/06/21/persistente-datenstrukturen.html)
von funktionaler Programmierung profitieren kann.

Unveränderbare ("immutable") Datentypen sind eine wichtige Technik der funktionalen Programmierung, so wichtig
dass wir auch in [Objective-C](http://funktionale-programmierung.de/2013/12/05/datentypen-objectivec.html)
nicht darauf verzichten möchten. Sollten Sie aber mal in der funktionalen Sprache Haskell des Bedürfnis
verspüren, imperativ zu programmieren, hilft Ihnen [der Artikel](http://funktionale-programmierung.de/2013/08/01/haskell-imperativ.html)
unseres Gastautors [Joachim Breitner](http://www.joachim-breitner.de/blog/) bestimmt weiter.

## Industrielle Anwendungen ##

Funktionale Programmierung wird auch vermehrt in industriellen Anwendungen eingesetzt.
So zeigen [zwei](http://funktionale-programmierung.de/2013/05/16/praxis-myownsafe.html) 
[Artikel](http://funktionale-programmierung.de/2013/09/05/praxis-myownsafe-2.html), wie wir ein Webprojekt
zur elektronischen Nachlassverwaltung schnell und kostensparend mit funktionaler Programmierung
umsetzen konnten. Auch lässt sich [medizinische Datenverarbeitung](http://funktionale-programmierung.de/2013/07/17/medizin-funktional.html)
hervorragend mit funktionaler Programmierung umsetzen.

Doch nicht nur unsere Firmen setzen auf funktionale Programmierung. Auch Banken wie [Credit Suisse](https://www.credit-suisse.com/de/de/)
und [Standard Chartered](https://www.sc.com/de/) oder Internetriesen wie [Twitter](http://twitter.com) und
[Facebook](http://facebook.com) setzen inzwischen auf funktionale Programmierung. Warum Facebook
auf die funktionale Sprache [Haskell](http://haskell.org) setzt, erfahren Sie in
[diesem Artikel](http://funktionale-programmierung.de/2013/10/02/haskell-facebook.html).

## Testen ##
    [Randomisierte Tests mit QuickCheck](http://funktionale-programmierung.de/2013/07/10/randomisierte-tests-mit-quickcheck.html)
    [Testen mit Haskell](http://funktionale-programmierung.de/2014/02/06/testing-haskell.html)
    
## Web-Programmierung ##
    [Erste Schritte in ClojureScript](http://funktionale-programmierung.de/2014/02/14/clojurescript-react.html)
    [Moderne Webanwendungen mit Haskell](http://funktionale-programmierung.de/2013/04/04/webanwendung-haskell.html)
    [Moderne Webanwendungen mit Haskell und Javascript: clientseitige Implementierung](http://funktionale-programmierung.de/2013/06/05/webanwendung-haskell2.html)

## Parallelität und Nebenläufigkeit ##
    [Haskell schlägt node.js](http://funktionale-programmierung.de/2013/05/08/haskell-nodejs.html)
    [Parallele Programmierung mit Haskell](http://funktionale-programmierung.de/2013/03/06/parallel-haskell.html)

## Praktische Beispiele ##
    [Haskell FFI](http://funktionale-programmierung.de/2013/09/12/haskell-ffi.html)
    [Buildsysteme mit Haskell](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html)
    [Eclipse Xtend](http://funktionale-programmierung.de/2014/01/23/eclipse-xtend.html)

## Monaden ##
    [Comprehending Queries](http://funktionale-programmierung.de/2014/02/19/comprehending-queries.html)
    [Die GUI-Monade: Monaden in der Praxis](http://funktionale-programmierung.de/2013/05/29/gui-monade.html)
    [Mehr Monaden in Aktion](http://funktionale-programmierung.de/2013/07/03/haskell-monaden3.html)
    [Monaden in Aktion](http://funktionale-programmierung.de/2013/05/22/haskell-monaden2.html)
    [Monaden: Das programmierbare Semikolon](http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html)

## Klassiker der funktionalen Programmierung ##
    [Tail Calls](http://funktionale-programmierung.de/2013/11/08/tail-calls.html)
    [Continuations in der Praxis](http://funktionale-programmierung.de/2013/10/31/continuations-praxis.html)
    [Ein Parser für HL7-Nachrichten in weniger als 180 Zeilen Haskell-Code](http://funktionale-programmierung.de/2013/08/29/hl7-parser.html)
    [Was ist das 'Expression Problem'?](http://funktionale-programmierung.de/2013/11/21/expression-problem.html)

## Veranstaltungen ##
    [Zurich FP Afternoon / Haskell-Hackathon](http://funktionale-programmierung.de/2013/08/15/haskell-hackathon.html)
    [Commercial Users of Functional Programming 2013](http://funktionale-programmierung.de/2013/08/07/cufp-2013.html)
    [Videos zur CUFP 2013 verfügbar](http://funktionale-programmierung.de/2013/12/12/cufp-2013-report.html)
    [Buchbesprechung: Das Curry-Buch](http://funktionale-programmierung.de/2013/07/25/curry-buch.html)
    
## Ein paar Zahlen zum Blog ##

Jeder Artikel hat mehr als 70 eindeutige Leser, mehr als die Hälfte der Artikel hat mehr als 235 Leser. Die Top-3 Artikel
haben jeweils mehr als 1000 Leser. Insgesamt hatte das Blog im letzten Jahr mehr als 8000 Besucher, welche insgesamt
mehr als 23.000 Seiten aufgerufen haben. Wir haben 30-40 Besucher pro Tag, an Tagen an den Artikel veröffentlich werden sind es
mindestens 60 Besucher, oft aber auch 80-120 Besucher.

Wir möchten Ihnen, den Leser, an dieser Stelle sehr herzlich für Ihre Treue danken. Wir hoffen, dass Sie auch in Zukunft
unseren Blog lesen und die Inhalte interessant finden. Damit wir unsere Artikel noch besser auf Ihre Interessen abstimmen können,
möchten wir Sie an dieser Stelle um Ihr Feedback bitten.
Was gefällt Ihnen an diesem Blog?
Was kann noch verbessert werden? Worüber würden Sie gerne mehr erfahren? Schreiben Sie Antworten
auf diese Fragen doch einfach in einen kurzem Kommentar. Danke!

<!-- more end -->

