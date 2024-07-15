---
layout: post
description: Facebook setzt auf Haskell
title: "Facebook setzt auf Haskell"
author: stefan-wehr
tags: ["Haskell"]
---

Vor kurzem haben wir hier in diesem Blog die Veranstaltung
[Zurich FP Afternoon](http://funktionale-programmierung.de/2013/08/15/haskell-hackathon.html) 
vorgestellt. Simon Marlow, einer der Hauptentwickler des weit verbreiteten
[Glasgow Haskell Compilers](http://haskell.org/ghc) und inzwischen bei Facebook
beschäftigt, hat auf dieser Veranstaltung
einen Überblick über sein aktuelles Projekt Haxl gegeben. Ich möchte
hier durch eine Zusammenfassung des Vortrags zeigen, dass inzwischen
auch so große Firmen wie Facebook auf funktionale Programmierung setzen,
um die Probleme der modernen Softwareentwicklung in den Griff zu bekommen.
Die [Folien zum Vortrag](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130829-FPAfternoon_The_Haxl_Project_at_Facebook/The%20Haxl%20Project%20at%20Facebook.pdf?raw=true) 
enthalten alle weiteren technischen Details.

<!-- more start -->

## Das Problem ##

Facebook speichert die verschiedenen Daten seines sozialen Netzwerk in verschiedenen
Persistenzkomponenten. Jede dieser Persistenzkomponenten ist auf einen bestimmten
Zweck hin ausgerichtet und funktioniert am besten, wenn Zugriffe einem bestimmten Muster
folgen. Eine der wichtigsten Persistenzkomponenten ist [TAO ("The Associations and Objects")](https://www.facebook.com/notes/facebook-engineering/tao-the-power-of-the-graph/10151525983993920).
Diese Komponente repräsentiert u.A. die Verbindungen zwischen den
Facebook-Nutzern. Ein effizienter Zugriff auf TAO sollte
dem Batch-Muster folgen, d.h. ein Anwendungsprogrammierer sollte möglichst
viele Anfragen gleichzeitig absetzen und nicht jede Anfrage einzeln hintereinander.
Andere Komponenten folgen anderen Zugriffsmustern, so gibt es z.B. Komponenten
die asynchron angefragt werden, andere stellen jedoch eine synchrone API
zur Verfügung.

Aus software-technischer Sicht möchte man die verschiedenen Zugriffsmuster
am liebsten abstrakt halten und über eine einheitliche API auf alle
Persistenzkomponenten zugreifen. Die wesentlichen Vorteile einer solchen
Abstraktion sind:

* Die Abstraktionsschicht und nicht der Anwendungsprogrammierer kümmert sich darum,
  dass jede Komponente effizient angefragt wird.

* Das bevorzugte Zugriffsmuster einer Komponente kann geändert werden, ohne
  dass der Anwendungsprogrammierer Änderungen vornehmen muss, um weiterhin
  eine gute Performanz zu erzielen.

## Die Lösung ##

Das Haxl-Projekt bei Facebook stellt eine solche Abstraktionsschicht zum Zugriff
auf die verschiedenen Persistenzkomponenten bereit. Haxl ist als eine eingebettete,
[domänenspezifische Sprache](http://funktionale-programmierung.de/2013/06/27/dsl-clojure.html) 
in Haskell implementiert. In anderen Worten: Haxl ist ein in Haskell implementiertes Framework,
welches von Anwendungsprogrammierern benutzt wird, um Haskell-Programme zu entwickeln,
die auf die verschiedenen Persistenzkomponenten von Facebook über eine einheitliche
API zuzugreifen.

Als Beispiel zeigt Simon Marlow in seinem Vortrag ein mit Haxl entwickeltes Programm
zur Entdeckung von Spam. Dieses Haxl-Programm soll in Kürze die bisherige Komponente
zur Entdeckung von Spam ablösen. 

Ich möchte an dieser Stelle nicht auf die technischen Details eingehen, dies sei Simon Marlow
in seinen
[Folien zum Vortrag](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130829-FPAfternoon_The_Haxl_Project_at_Facebook/The%20Haxl%20Project%20at%20Facebook.pdf?raw=true)
vorbehalten. Ich möchte lediglich stichwortartig erwähnen, welche Eigenschaften von
Haskell bei der Realisierung von Haxl wichtig waren:

* Abstraktion über den Kontrollfluss mittels Monaden. Dazu gab es hier im Blog
  auch schon einige Artikel, zu finden im [Archiv](http://funktionale-programmierung.de/tags-archive.html).

* Die Implementierung von Haxl setzt außer Monaden auch noch
  [applikative Funktoren](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors)
  ein. Die applikativen Funktoren werden benutzt, um unabhängige Zugriffe
  auf die Persistenzkomponenten einzusammeln und dann später gemeinsam 
  als Batch-Zugriff abzusetzen.

* Gute Kontrolle über Seiteneffekte erlaubt eine schöne Trennung zwischen den
  eigentlichen Berechnungen und der Persistenzschicht.

* Leichtgewichtige Threads erlauben eine einfache Implementierung der Zugriffe auf die
  verschiedenen Persistenzkomponenten. Auch hierzu gab es schon einen
  [Artikel](http://funktionale-programmierung.de/2013/05/08/haskell-nodejs.html)

## Fazit ##

Das Beispiel Haxl zeigt, dass
es die funktionale Programmierung inzwischen soweit gebracht hat,
dass selbst große Firmen wie Facebook auf das funktionale
Programmierparadigma setzen. Und wann starten *Sie* mit funktionaler
Programmierung?
