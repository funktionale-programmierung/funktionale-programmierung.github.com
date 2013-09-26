---
layout: post
description: Rein funktionale JVM Sprache Frege
title: "Frege - rein funktionale Programmierung in der JVM"
author: ingo-wechsung
tags: ["Java", "Haskell", "Frege", "JVM"]
---

Die Auswahl an Programmiersprachen für die JVM ist riesig. 
Auch für funktionale Programmierer wird einiges geboten, von Scala über verschiedene ML-Dialekte bis hin zu Clojure.
Eine Nische war jedoch bislang nicht besetzt: 
die der Haskell-artigen, also rein funktionalen Sprachen mit Bedarfsauswertung und Typinferenz.
Dabei erfreut sich Haskell außerhalb der JVM-Welt wachsender Beliebtheit.

<!-- more start -->

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## JVM ohne Haskell ##

Eine häufig gestellte Frage im Haskell-Wiki ist folgende: 
[_Warum ist GHC nicht für JVM oder .NET verfügbar?_](http://www.haskell.org/haskellwiki/GHC/FAQ#Why_isn.27t_GHC_available_for_.NET_or_on_the_JVM.3F)

Die Antwort ist recht detailliert, und lautet zusammengefaßt so: 
Es sei ein lohnendes, aber nicht zu unterschätzendes Unterfangen, 
die größten Schwierigkeiten ergäben sich daraus, daß man letztlich die GHC Laufzeitumgebung 
samt aller primitiven Operationen für die jeweiligen Umgebungen neu implementieren und dann auch pflegen müßte.
Diese Herausforderung sei jedoch "mostly broad rather than deep" 
und mit genügend Ressourcen könne man sie durchaus angehen.

Letzteres ist meiner Auffassung nach eine Fehleinschätzung.

Es lauern durchaus schwierige, meines Wissens bisher ungeklärte, technische Probleme,
beispielsweise im Bereich der Nebenläufigkeit: Das Thread-Modell der JVM ist ein ganz anderes
als das der GHC-Umgebung. Man denke an GHC-Features wie _green threads_ und _software transactional memory_. 
Ob so etwas 1:1 in der JVM implementiert werden kann ist durchaus nicht klar.

## Eine Nummer kleiner ##

Wenn 100% Haskell-Kompatibilität so schwer (oder evtl. auch gar nicht) zu haben ist, 
geht es dann vielleicht eine Nummer kleiner? 

Ohnehin ist ja voraussehbar, daß Programme in einem fiktivem JVM-Haskell 
vorrangig der Interoperabilität mit _anderen JVM-Sprachen_ wegen geschrieben würden - 
diejenigen Programme, die so etwas nicht brauchen, laufen ja bisher auch ohne JVM-Ballast wunderbar.
Genau solcher JVM-Haskell Code wäre aber mit der restlichen Haskell-Welt unverträglich in dem Sinne,
daß er nicht einfach in einer anderen Umgebung übersetzt und ausgeführt werden könnte, und umgekehrt
gälte dasselbe für Haskell Programme, die C/C++ Bibliotheken via FFI nutzen: ohne weiteres wären diese
wiederum auf der JVM nicht lauffähig. Nur derjenige Teil, der keinerlei Code aus anderen Sprachen nutzt, 
wäre in beiden Welten nutzbar.

Mit der Programmiersprache [Frege](https://github.com/Frege/frege) wird solch ein "eine Nummer kleiner" Weg beschritten.
Es wird davon ausgegangen, daß die 100%-ige Kompatibilität schwer herstellbar ist, 
aus oben angeführten Gründen aber auch der Mühe weniger wert ist als man gemeinhin glaubt, 
auf jeden Fall aber derzeit schlicht nicht existiert.

Dennoch glauben wir (der Autor dieses Blog-Artikels ist gleichzeitig der Initiator des Projekts) 
mit Frege die Essenz, derentwegen viele Menschen zu Haskell-Fans wurden, auf die JVM gebracht zu haben:

  * die geniale (fast unveränderte) Syntax ohne geschweifte Klammern und Semikolons
  * das Hindley-Milner Typsystem, das vollständige Typinferenz gewährleistet, ergänzt durch Funktionstypen höherer Ordnung und Typklassen (vorerst nur einfache)
  * das Auswertungsmodell mit Bedarfsauswertung (lazy evaluation)
  * die Trennung von rein funktionalem Code von solchem, der mit Seiteneffekten behaftet ist über Monaden

Im Grunde genommen handelt es sich um eine Sprache etwa im Umfang von Haskell 2010, 
mit einigen Erweiterungen, die aus GHC bekannt sind,
allerdings ohne FFI (foreign function interface). 
An dessen Stelle tritt ein Mechanismus, 
der speziell auf das Einbinden von JVM-Typen (also Klassen und Interfaces) und JVM-Methoden zugeschnitten ist.

## Vorteile der Unabhängigkeit ##

Trotz der bewußt aufgegebenen Haskell-Kompatibilität laufen in Frege die Dinge vielfach genauso wie in Haskell. 

Es gibt jedoch Unterschiede, die aus der grundsätzlichen Entscheidung herrühren, 
in erster Linie eine praktische Sprache für JVM Anwendungen sein 
(oder werden) zu wollen und erst in zweiter Linie eine möglichst Haskell-ähnliche. 

So werden beispielsweise für die primitiven Datentypen die entsprechenden der JVM verwendet.
Deshalb ist `Bool` kein algebraischer Datentyp, sondern ein primitiver, und es gibt zwei Literale `true` und `false`
anstelle der Konstruktoren `True` und `False`. Ebenso ist `String` nicht dasselbe wie `[Char]`, 
sondern die eingebundene Klasse `java.lang.String`.

Die Unterschiede setzen sich fort auf der Ebene der Standardbibliotheken (soweit schon vorhanden), 
insbesondere in Bezug auf Ein- und Ausgabe, Ausnahmen, Threads und dergleichen. 
Es wird immer das vorhandene Java-SE API genutzt.
Zum Beispiel werden `MVar`s durch `java.util.concurrent.LinkedBlockingQueue`s mit maximaler Länge von 1 implementiert.
Dies hindert nicht, daß bekannte Funktionen wie `getChar`, `putStrLn` oder eben auch `newMVar` existieren 
und genauso funktionieren wie in Haskell, 
jedoch gibt es zahlreiche von Haskell 2010 spezifizierten systemnahen Pakete, Datentypen und Funktionen nicht. 
Auf der anderen Seite haben wir die Erfahrung gemacht, daß rein funktionaler Code praktisch unverändert von Haskell
übernommen werden konnte, so z.B. Pakete wie `Data.List`.

Der Vorteil dieser Vorgehensweise liegt u.a. darin, daß Frege kaum ein Laufzeitsystem braucht. 
Das wenige, was vorhanden ist, beschäftigt sich hauptsächlich mit der Java fremden 
Bedarfsauswertung und partiell applizierten Funktionen.

Ein weiterer Vorteil der Unabhängigkeit vom Haskell-Standard ist, daß ein Problem, 
über das in Haskell-Kreisen 
[seit vielen Jahren diskutiert wird](http://ghc.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) 
ohne daß sich eine Lösung abzeichnet, vermieden werden kann: 
Ich rede von den Neueinsteigern (vor allem solchen, die aus OO-Gefilden kommen) 
schwer vermittelbaren globalen Feldbezeichnern in Datenstrukturen (record fields).
In Frege hat jeder Datentyp einen eigenen Namensraum, in dem Feldbezeichner, aber auch beliebige weitere Funktionen
definiert sein können, ohne den globalen Namensraum zu beeinträchtigen. 
Dazu kommen "syntactic sugar" für Feldzugriffe und (rein funktionale) Feldänderungen 
sowie Unterstützung vom Typsystem für typgesteuerte Feldnamenauflösung.

## Status und Ausblick ##

Frege ist bislang ein Hobbyprojekt des Autors sowie einer Handvoll Unterstützer. 
Der offene Quellcode steht unter BSD Lizenz.
Frege ist unfertig und in Entwicklung, 
sowohl in Hinblick auf implementierte Bibliotheken, Entwicklungs- und Dokumentationstools als auch die Sprache selbst.

Dennoch ist es kein Spielzeug mehr. 
Außer dem (zu 99.99% in Frege selbst geschriebenen) Compiler existiert ein Plugin für Eclipse, 
sowie ein Interpreter. Eine Version dieses Interpreters läuft als 
[Web-Service](http://try.frege-lang.org/), also nur einen Klick entfernt (möglicherweise jedoch nicht auf Ihrem Telefon).

Meine Hoffnung ist, diesem noch weithin unbekannten Projekt etwas mehr Bekanntheit zu verschaffen.
Ich lade jeden Interessierten ein, sich die Sache einmal anzuschauen, es an Interessierte weiterzuerzählen
und im besten Falle etwas beizutragen.

Quellcode, Downloads inklusive Sprachbeschreibung und weiterführende Verweise sind
[auf der Projektseite](https://github.com/Frege/frege) zu finden.

<!-- more end -->
