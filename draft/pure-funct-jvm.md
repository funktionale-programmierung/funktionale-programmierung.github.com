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
Diese Herausforderung sei jedoch "mostly broad rather than deep". 

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
Der Anspruch ist, die Essenz, derentwegen viele Menschen zu Haskell-Freunden wurden, in die JVM-Welt zu holen:

  * die geniale Haskell-Syntax ohne geschweifte Klammern und Semikolons (fast unverändert)
  * das Hindley-Milner Typsystem, das vollständige Typinferenz gewährleistet, ergänzt durch Funktionstypen höherer Ordnung und Typklassen (vorerst nur einfache)
  * das Ausführungsmodell mit Bedarfsauswertung (lazy evaluation)
  * die Trennung von rein funktionalem Code von solchem, der mit Seiteneffekten behaftet ist mit Hilfe des Typsystems (Monaden)

Frege ist eine Sprache etwa im Umfang von Haskell 2010, 
zuzüglich einiger Erweiterungen, die aus GHC bekannt sind,
allerdings ohne FFI (foreign function interface). 
An dessen Stelle tritt ein ähnlicher Mechanismus, 
der aber speziell auf das Einbinden von JVM-Typen (also Klassen und Interfaces) und JVM-Methoden zugeschnitten ist.

## Vorteile der Unabhängigkeit ##

Trotz der bewußt aufgegebenen Haskell-Kompatibilität laufen in Frege die Dinge vielfach genauso wie in Haskell. 

Es gibt jedoch [Unterschiede](https://github.com/Frege/frege/wiki/Differences-between-Frege-and-Haskell), 
die aus der grundsätzlichen Entscheidung herrühren, 
in erster Linie eine praktische Sprache für die JVM sein 
(oder werden) zu wollen und erst in zweiter Linie eine möglichst Haskell-ähnliche.

So werden beispielsweise für die primitiven Datentypen die entsprechenden der JVM verwendet.
Folglich ist `Bool` in Frege kein algebraischer Datentyp, sondern ein primitiver, und es gibt zwei Literale `true` und `false`
anstelle der Konstruktoren `True` und `False`. Ebenso ist `String` nicht dasselbe wie `[Char]`, 
sondern basiert auf der eingebundenen Klasse `java.lang.String`, 
die wie alle anderen eingebundenen JVM Typen auf Frege-Ebene als abstrakter Datentyp erscheint.

Die Unterschiede setzen sich fort auf der Ebene der Standardbibliotheken (soweit schon vorhanden), 
insbesondere in Bezug auf Ein- und Ausgabe, Ausnahmen, Threads und dergleichen. 
Es werden hier jeweils die entsprechenden Java-SE API genutzt.

Zum Beispiel nutzen Ein- und Ausgabe in Frege Klassen wie `java.io.BufferedReader` und `java.io.PrintWriter`. 
Es wäre natürlich möglich gewesen, dem Haskell Standard penibel zu folgen, 
der Ein- und Ausgabe auf abstrakten Filedeskriptoren (Datentyp `Handle` aus `System.IO`) basieren läßt.
Dies hätte bedeutet, für Frege eine eigene Ein-/Ausgabeschicht bereitzustellen, 
die natürlich sämtlichen anderen JVM Sprachen völlig unbekannt wäre. 
Für die Interoperabilität mit letzteren würde dann doch wieder die von Java bekannte Funktionalität gebraucht, 
sowie Abbildungen zwischen den verschiedenen Modellen. 

Die unterschiedliche Basis für Ein-/Ausgabe hindert am Ende nicht, 
daß bekannte Funktionen wie `getChar` oder `putStrLn` existieren 
und aus Nutzersicht genauso funktionieren wie in Haskell, 
jedoch gibt es zahlreiche von Haskell 2010 spezifizierten systemnahen Pakete, Datentypen und Funktionen nicht. 
Auf der anderen Seite haben wir die Erfahrung gemacht, daß rein funktionaler Code praktisch unverändert von Haskell
übernommen werden konnte, so z.B. Module wie `Data.List`.

Der Vorteil der Nutzung des Java API liegt u.a. darin, daß Frege nur ein minimales Laufzeitsystem benötigt, 
das sich hauptsächlich mit Bedarfsauswertung und Funktionen beschäftigt,
also mit den Dingen, die in Java unbekannt sind.

Ein weiterer Vorteil der Unabhängigkeit vom Haskell-Standard ist, 
daß ein Problem von vornherein vermieden werden konnte, 
über das in Haskell-Kreisen 
[seit vielen Jahren diskutiert wird](http://ghc.haskell.org/trac/ghc/wiki/Records), 
ohne daß eine Lösung absehbar wäre. 
Ich rede von den Feldbezeichnern in Datentypen (record fields). 
In Haskell sind diese Namen global, was mehrere Datentypen im gleichen Modul mit gleichen Feldnamen unmöglich macht.

In Frege hat jeder Datentyp einen eigenen Namensraum, in dem Feldbezeichner, aber auch beliebige weitere Funktionen
definiert sein können, ohne den globalen Namensraum zu beeinträchtigen. 
Dazu kommen syntaktische Unterstützung für Feldzugriffe  
sowie Unterstützung des Typsystems für typgesteuerte Feldnamenauflösung.

Nicht zuletzt erlaubt Freges Eigenständigkeit, Dinge richtig zu machen, 
die ziemlich unumstritten in Haskell korrekturbedürftig sind. 
So in einer Frage der mathematisch korrekten Typklassenhierarchie: 
in Frege war bereits von Anfang an 
[`Applicative` eine Superklasse von `Monad`](http://ro-che.info/ccc/21), 
und demnächst wird die Hierarchie gemäß dem Vorschlag in Edward Kmetts 
[`semigroupoids` Paket](http://hackage.haskell.org/package/semigroupoids) vervollständigt. 

## Status und Ausblick ##

Frege ist bislang ein Hobbyprojekt des Autors dieses Artikels sowie einer Handvoll Unterstützer. 
Der offene Quellcode steht unter BSD Lizenz.
Frege ist unfertig und in Entwicklung, 
sowohl in Hinblick auf implementierte Bibliotheken, Entwicklungs- und Dokumentationstools als auch die Sprache selbst.

Dennoch ist es kein Spielzeug mehr. 
Außer dem (in Frege selbst geschriebenen) Compiler existiert ein Plugin für Eclipse, 
sowie ein Interpreter. Eine Version dieses Interpreters läuft als 
[Web-Service](http://try.frege-lang.org/), 
also nur einen Klick entfernt (möglicherweise jedoch nicht auf Telefonen funktionsfähig).

Meine Hoffnung ist, diesem noch weithin unbekannten Projekt zukünftig mehr Bekanntheit verschaffen zu können.
Jeder ist herzlich eingeladen, sich die Sache einmal näher anzuschauen, 
mögliche Interessenten darauf aufmerksam zu machen
oder gar - im besten Falle - etwas beizutragen.

Quellcode, Downloads inklusive Sprachbeschreibung und weiterführende Verweise sind
[auf der Projektseite](https://github.com/Frege/frege) zu finden.

<!-- more end -->
