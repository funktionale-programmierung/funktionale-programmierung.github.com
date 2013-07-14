---
layout: post
description:
title: "Medizinische Datenverarbeitung mit funktionaler Programmierung - Ein Beispiel aus der Praxis"
author: stefan-wehr
tags: ["haskell", "Medizin", "Praxis", "kommerziell", "Scala", "funktional"]
---

Funktionale Programmierung ist längst den Kinderschuhe entwachsen.
Inzwischen setzen auch Firmen wie
z.B. [Twitter](http://cufp.org/conference/sessions/2011/large-scale-internet-services-scala-twitter)
oder [Microsoft](http://research.microsoft.com/en-us/projects/fsharp/)
auf funktionale Programmierung, um damit sichere, korrekte und performante
Software in kurzer Zeit zu erstellen.

Heute zeigen wir anhand eines Beispiels aus der Praxis, welche
Produkte als Firma mit funktionaler Programmierung realisieren
Konkret geht es um das Produkt *Checkpad MED*
([Film](http://www.youtube.com/watch?v=yvq_EuEmQrk),
[Homepage](http://www.lohmann-birkner.de/de/Checkpad-MED/index.php)),
eine digitale und mobile Krankenakte für Krankenhausärzte. Checkpad MED
wird unter technischer Federführung der [factis research
GmbH](http://www.factisresearch.com/) fast ausschließlich
in funktionalen Programmiersprachen entwickelt.

In diesem Artikel
soll es aber erstmal weniger um technische Details gehen sondern
vielmehr um einen Überblick über das System und seine Architektur.
Denn auch auf der Architekurebene kann man Prinzipien der funktionalen
Programmierung anwenden und damit Vorteile bzgl. guter Testbarkeit und
Nachvollziehbarkeit genießen.
In späteren Artikeln werden wir dann auf Grundlage des vorliegenden
Artikels auch auf technische Details und die Vorzüge der
funktionalen Programmierung beim Lösen spezifischer Probleme eingehen.

<!-- more start -->

Checkpad MED ist eine digitale, mobile Krankenakte, die Ärzten im
Krankheithaus jederzeit und überall Zugriff auf die relevanten
Patientendaten, wie die Krankengeschichte, Arztbriefe, Laborwerte,
Röntgenbilder, OP Berichte und vieles mehr erlaubt. Darüberhinaus bietet
die Anwendung durch ein Aufgaben- und Notizsystem die Möglichkeit,
Arbeitsabläufe im Klinikalltag besser zu organisieren.
Der Client für Checkpad MED ist als iOS-App realisiert, die auf iPad,
iPhone und iPod läuft. Die folgenden Screenshots zeigen verschiedene
Ansicht des Clients.

<div id="center">
<img src="/files/medizin-funktional/checkpad-screenshots.jpg">
</img>
</div>
<br/>

Der weitaus größere Teil des Checkpad-Systems ist allerdings
serverseitig realisiert, wobei auf dem Server mit
[Haskell](http://www.haskell.org/) und [Scala](http://www.scala-lang.org/)
fast ausschließlich funktionale Programmiersprachen zum Einsatz kommen.

<div id="center">
<img src="/files/medizin-funktional/checkpad-architektur.png">
</img>
</div>
<br/>

<!-- Local Variables: -->
<!-- mode: text -->
<!-- End: -->
