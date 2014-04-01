---
layout: post
description: Java to Scala
title: "Wie migriert man Java nach Scala"
author: helmut-dobretzberger
tags: ["Scala", "Java", "Equals"]
meta_description: >
  Scala in einem Java-Programm verwenden
page_title: "Java to Scala"
---

Die ActiveGroup entwickelt die Software [EQUALS](http://www.equals.ch) für das gleichnamige Gescheinschaftsprojekt von [INTEGRAS](http://www.integras.ch)
und der [KJPK Basel](http://www.upkbs.ch/patienten/ambulantes-angebot/kinder-und-jugend/Seiten/default.aspx). 
EQUALS (eine Kurzform für Ergebnisorientierte Qualitätssicherung in sozialpädagogischen Einrichtungen) hilft zur Abklärung
der psychischen Gesundheit von jungen Menschen, sowie zur pädagogischen Dokumentation der (Heim)-Erziehungshilfen.

In diesem Projekt haben wir 2012 begonnen, die Java-Codebasis Schritt für Schritt nach Scala zu migrieren.
Dieser Blogpost zeigt dazu die prinzipellen Schritte die notwendig sind, um Scala-Code in einer Java-Software zu verwenden
<!-- more start --> 

Bevor wir uns aber damit beschäftigen, aber ein Überblick über die Funktionen der Software:

- Fremdsprachigkeit (deutsch, schweizerdeutsch, italienisch, französisch)
- Userverwaltung
- viele unterschiedliche Fragenbögen, die von Jugendlichen, Betreuern, Lehrern oder Eltern ausgefüllt werden
- Datenauswerung inklusive Diagramm-Erstellung, PDF und SPSS-Export, Vergleich von Antworten uvm.
- Erfassung der Anamnese von Jugendlichen
- Ex- und Import der Programmdaten zu anderen EQUALS-Installationen
- Versionen für Windows und MacOS, inkl. Server für die Datebank oder lokaler Installation

EQUALS wurde Ende 2010 entwickelt, damals noch von ukrainischen Entwicklern, die das Programm in Java entwickelten und dabei einen
 idiomatischen Java-Stil verwendeten: Hibernate, Spring und häufiges Überschreiben von Methoden sind nur einige Beispiele hierfür.
 
Die Übernahme der Weiterentwicklung von EQUALS durch Entwickler der ActiveGroup führte dazu, dass Ende 2012 entschieden wurde, 
EQUALS mit von Scala weiterzuentwickeln. 

## Umstellung von Java auf Scala 
Die gemeinsame Verwendung von Java und Scala in einem Projekt wird durch [SBT (Scala Build Tool)](http://www.scala-sbt.org) ermöglicht.
Es sind tatsächlich nur wenige Schritte notwendig, um die Umstellung durchzuführen:

1. SBT installieren: Genaue Infos für ihr jeweiliges System liefert die [Dokumentation von sbt](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html)
2. Die eigene Software an die geforderte Ordner-Struktur von SBT anpassen: 
   - SBT setzt standardmäßig voraus, dass der Java-Programm-Code im Ordner `src/main/java`, sowie der Code der Unit-Tests in `src/test/java` liegt.
   - Analog dazu wird der Scala-Code in `src/main/scala` sowie in `src/test/scala` erwartet.
3. SBT konfigurieren: SBT verfügt über eine Konfigurationsdatei `build.sbt`, die es erlaubt, die gewünschte Scala-Version, Java-Compile-Flags, 
   Abhängigkeiten der Software zu anderen Bibliotheken und ähnliches anzugeben: [http://www.scala-sbt.org/release/docs/Detailed-Topics/Java-Sources.html](http://www.scala-sbt.org/release/docs/Detailed-Topics/Java-Sources.html).
4. Um nun aus dem Java/Scala-Programm ein lauffähiges \*.jar zu erzeugen, muss SBT verwendet werden. Mit dem Befehl `sbt assembly` wird die Software compilert und
   das jar-File erzeugt.

Man sieht bereits, dass man vorhandenen Java-Code nicht anfassen muss, um die Möglichkeit zu schaffen, mit Scala weiterzuentwickeln. Bereits vorhandener Code ist also
nicht wertlos, sondern kann sukzessive an den Stellen, bei denen die Software verbessert oder mit neuen Funktionen versehen wird, durch Scala-Code ausgetauscht 
oder ergänzt werden. Auch für doch recht umfangreiche Frameworks wie Hibernate stellt es kein Problem dar, dass man dieses über Scala anspricht. 

Bei uns besteht EQUALS mitterweile etwa nur noch zu 50% aus Java, der Rest wurde durch Scala sukzessive abgelöst. Das bedeutet, dass an einigen Stellen sehr häufig zwischen
Java und Scala-Code interagiert wird. Dies läuft recht problemlos, welche Dinge aber bei der Entwicklung einer Software, die Java und Scala verwendet 
noch vernünftigerweise zu beachten sind, werden wir in einem späteren Blogposting behandeln.
