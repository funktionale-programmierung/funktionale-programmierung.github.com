---
layout: post
description: Java und Scala in einem Projekt
title: "Gemeinsame Verwendung von Scala und Java in einem Projekt"
author: helmut-dobretzberger
tags: ["Scala", "Java", "SBT", "EQUALS"]
page_title: "Java und Scala in einem Projekt"
---

Die Active Group entwickelt die Software [EQUALS](http://www.equals.ch) für das gleichnamige Gemeinschaftsprojekt von [INTEGRAS](http://www.integras.ch)
und der [KJPK Basel](http://www.upkbs.ch/patienten/ambulantes-angebot/kinder-und-jugend/Seiten/default.aspx). 
EQUALS (eine Kurzform für "Ergebnisorientierte Qualitätssicherung in sozialpädagogischen Einrichtungen") hilft zur Abklärung
der psychischen Gesundheit von jungen Menschen, sowie zur pädagogischen Dokumentation der (Heim)-Erziehungshilfen.

In diesem Projekt haben wir 2012 begonnen, die Java-Codebasis Schritt für Schritt nach Scala zu migrieren.
Dieser Blogpost zeigt dazu die prinzipellen Schritte die notwendig sind, um Scala und Java zusammen in einer Software zu verwenden.

<!-- more start --> 

Bevor wir uns aber damit beschäftigen, zunächst ein Überblick über die Funktionen der Software:

- Mehrsprachigkeit (deutsch, schweizerdeutsch, italienisch, französisch)
- Userverwaltung
- viele unterschiedliche Fragebögen, die von Jugendlichen, Betreuern, Lehrern oder Eltern ausgefüllt werden
- Datenauswertung inklusive Diagramm-Erstellung, PDF und CSV-Export, Vergleich von Antworten uvm.
- Erfassung einer umfassenden Anamnese der Jugendlichen
- Ex- und Import der Programmdaten zu anderen EQUALS-Installationen
- Versionen für Windows und MacOS, inkl. Server für die Datenbank oder lokaler Installation

EQUALS wurde ab Ende 2010 von einem Subunternehmen der Active Group
entwickelt, das einen relativ üblichen Design-Ansatz aus der Java-Welt
wählte. Hibernate und Spring wurden als Basis verwendet, und mit jedem
Änderungswunsch des Kunden enstanden immer komplexere
Klassenhierarchien, eine Vermischung von Datenbank- und Datenmodell,
und immer mehr Duplikation von Code, um nur einige Punkte zu nennen.
 
Im Zuge der Übernahme der Weiterentwicklung von EQUALS durch
Entwickler der Active Group, wurde Ende 2012 daher auch entschieden, EQUALS
mit Scala weiterzuentwickeln, um diesen Problemen mittelfristig Herr
zu werden, und um zukünftige Anforderungen mithilfe von funktionaler Programmierung schneller und robuster umsetzen zu können.
Dabei war klar, dass wir nicht auf einen Schlag eine komplette Übersetzung des Java-Codes machen können oder wollen - das Projekt besteht
aus über 100.000 Zeilen Code - sondern über längere Zeit sowohl Java als auch Scala verwenden müssen.

## Buildtool für gemischte Projekte

Die gemeinsame Verwendung von Java und Scala in einem Projekt wird durch [SBT (Scala Build Tool)](http://www.scala-sbt.org) ermöglicht.
Es sind tatsächlich nur wenige Schritte notwendig, um die Umstellung durchzuführen:

1. SBT installieren: Genaue Infos für ihr jeweiliges System liefert die [Dokumentation von sbt](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html)
2. Die eigene Software an die geforderte Ordner-Struktur von SBT anpassen: 
   - SBT setzt standardmäßig voraus, dass der Java-Programm-Code im Ordner `src/main/java`, sowie der Code der Unit-Tests in `src/test/java` liegt.
   - Analog dazu wird der Scala-Code in `src/main/scala` sowie in `src/test/scala` erwartet.
3. SBT konfigurieren: SBT verfügt über eine Konfigurationsdatei `build.sbt`, die es erlaubt, die gewünschte Scala-Version, Java-Compile-Flags, 
   Abhängigkeiten der Software zu anderen Bibliotheken und ähnliches anzugeben: [http://www.scala-sbt.org/release/docs/Detailed-Topics/Java-Sources.html](http://www.scala-sbt.org/release/docs/Detailed-Topics/Java-Sources.html).
   Man darf hier aber natürlich nicht verschweigen, dass das SBT auch Schwierigkeiten mit sich bringt und ein bisschen Einarbeitungszeit erfordert. Das gilt aber
auch für andere Build-Tools des Java Ökosystems.
4. Um nun aus dem Java/Scala-Programm ein lauffähiges \*.jar zu erzeugen, muss SBT verwendet werden. Mit dem Befehl `sbt assembly` wird die Software compilert und
   das jar-File erzeugt.
   
Man sieht bereits, dass man vorhandenen Java-Code nicht anfassen muss, um die Möglichkeit zu schaffen, mit Scala weiterzuentwickeln. Bereits vorhandener Code ist also
nicht wertlos, sondern kann sukzessive an den Stellen, bei denen die Software verbessert oder mit neuen Funktionen versehen wird, durch Scala-Code ausgetauscht 
oder ergänzt werden. Auch für doch recht umfangreiche Frameworks wie Hibernate stellt es kein Problem dar, dass man dieses über Scala anspricht. 


## Übersetzung

Bei uns besteht EQUALS mitterweile etwa nur noch zu ca. 70% aus Java,
der Rest wurde durch Scala sukzessive abgelöst, bzw. neue Features
gleich in Scala entwickelt. Das bedeutet, dass an einigen Stellen sehr
häufig zwischen Java und Scala-Code interagiert wird. Dies läuft
ziemlich problemlos.

Eine Übersetzung des vorhandenen Java-Codes nach Scala wäre zwar auch relativ
direkt und mit automatischen Tools möglich, aber wir wollen natürlich
den stark imperativen, objekt-orientierten Code in _funktionalen_
Scala-Code übertragen. Dort wo dabei Methodenüberschreibungen,
Mutation von Objekten und Collections oder gar statischen Variablen
und komplexe Klassenhierarchien vorhanden sind (und das ist leider
fast immer der Fall), wird dies schnell enorm aufwändig. Umfangreiche
und häufig manuelle Vorher-/Nachher-Tests sind hier erforderlich. Nach
und nach haben wir es aber bisher immer geschafft beim Übersetzen nach
Scala mehr Struktur in den Code zu bekommen, Intentionen von
Implementationsdetails zu trennen, und die Software robuster für
zukünftige Änderungen zu machen.

Hier im Blog ist bereits beschrieben, wie wir neue Anforderungen an die Benutzeroberfläche mithilfe einer [GUI-Monade](/2013/05/29/gui-monade.html) umgesetzt haben,
und wie wir mithilfe einer eigens entwickelten, [funktionalen API für JasperReports](2013/06/13/funktionale-api-jasper.html) die Reporting-Funktionalität der Software programmieren.
Es wird in Zukunft weitere Artikel zu dem Thema geben, z.B. werden wir noch einmal konkreter darauf eingehen, wie man bestimmte typische
Java-Entwurfsmuster und Bibliotheken (wie z.B. [Hibernate](http://hibernate.org/)) los werden kann.

## Ausblick

Neben der fortlaufenden Umstellung von Java auf Scala ist heuer auch eine Webversion von EQUALS in Planung,
bei der wir dann clientseitig wahrscheinlich auf ClojureScript und React setzen; eine kleine
Einführung hierzu gibt es bereits hier im Blog: [Erste Schritte mit ClojureScript](/2014/02/14/clojurescript-react.html)

## Zusammenfassung

Wir hatten bei der Umstellung der Entwicklung von Java auf Scala mit keinen größeren Problemen zu kämpfen, im Gegenteil: Scala und Java können mithilfe von SBT bequem gemeinsam verwendet werden und es
ist möglich, eine Änderung der Codebasis Schritt für Schritt durchzuführen. Da kein bestehender Code zeitaufwendig migriert werden muss, können wir Scala auch für bereits existierende, umfangreiche Java-Projekte empfehlen. 
