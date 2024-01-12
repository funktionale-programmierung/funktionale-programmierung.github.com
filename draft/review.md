---
layout: post
title: "Rezension von Sandy Maguire: Algebra-Driven Design"
author: michael-sperber
tags: []
---

Maguires Buch handelt von Domänenmodellierung mit Algebra und
funktionaler Programmierung, oder, anders gesagt, vom konzipieren von
Kombinatorenbibliotheken. Dieses Thema ist bereits in zahllosen Papers
gut untersucht worden, angefangen mit (vermutlich) Peter Hendersons
funktionaler Geometrie [Henderson(1982)]. Und tatsächlich schrieben
Simon Peyton Jones und Jean-Marc Eber im Jahr 2008:

<ul><blockquote>Zu diesem Zeitpunkt sollte jeder funktionale
Programmierer, der etwas auf sich hält, anfangen, vor Wut zu schäumen
und "Bau eine Kombinatorenbibliothek!" zu schreien.</blockquote> -- [Peyton
Jones et al.(2000)Peyton Jones, Eber, & Seward]</ul>

Wenn also "jeder funktionale Programmierer, der etwas auf sich hält"
bereits vor zwanzig Jahren davon wusste, warum schreibt man jetzt ein
Buch darüber? Nun ja, Kombinatorenbibliotheken haben die
Softwareentwicklungswelt nicht gerade im Sturm erobert, und zwar aus
mindestens zwei Gründen:

<ul>
<li>Es wurden zwar zahllose Paper geschrieben, diese waren aber
vor allem Experten der funktionalen Programmierung zugänglich, nicht
der breiten Allgemeinheit der Softwareentwickler.</li>
<li>Es haben zwar viele Paper Kombinatorenbibliotheken vorgestellt, aber
meist war keine Erklärung dabei, nach welcher Methodik die Autoren die
jeweiligen Bibliotheken entwickelt hatten.</li></ul>

Damit wurden Kombinatorenbibliotheken effektiv zur Folklore verbannt; sie
sind abseits der funktionalen Programmierung nach wie vor
weitestgehend unbekannt. Maguires Buch erklärt, wie man
Kombinatorenbibliotheken nach algebraischen Prinzipien konzipieren kann.
Es ist deutlich zugänglicher geschrieben als die mesiten ICFP-Paper zu
Kombinatoren und damit ein wichtiger Beitrag dazu, dieses Versäumnis
der Gemeinschaft der funktionalen Programmierung anzugehen.

### Übersicht ###

Das Buch beginnt mit einem Vorwort, das erklärt, wie sinnvoll es ist,
verständliche Abstraktionen auf algebraischen Gesetzen aufzubauen und
das die Notationskonventionen für den folgenden Haskellcode angibt.

Die erste Hälfte des Buchs handelt von der Abstraktionsgestaltung
sowie den Gesetzen, die ihre Semantik beschreiben. Sie erklärt
Methodik anhand zweier Beispiele. Das erste, relativ einfache, ist
eine Abwandlung von Hendersons funktionaler Geometrie, "tiles"
("Kacheln") genannt. Das zweite ist ein Softwaresystem, das ein
Schatzsuchenspiel unterstützen soll.

Mit "tiles" beginnt das Buch mit den üblichen Kombinatoren zur
Rotation und zum Spiegeln, ergänzt ihre Typsignaturen mit
algebraischen Gesetzen und führt nach und nach fortgeschrittene
Kombinatoren, was schließlich zu einem applikativen Funktor
führt. Die Darstellung wird sorgfältig als gleichwertige
Beobachtung wiedergegeben.

Eine Schatzssuche ist ein Spiel, das von (echten) Menschen gespielt
wird, die durch die Stadt laufen, Aufgaben lösen (gelegentlich
mithilfe von Hinweisen), Nachweise über erledigte Aufgaben im System
hinterlegen und dafür Punkte bekommen. Dies ist ein deutlich
umfangreicheres Beispiel, dessen Konzeption nicht nur vorgestellt
wird, sondern im Laufe mehrerer Revisionen, die die Kombinatorennatur
der Domäne nach und nach herausstellen, erarbeitet.

Die zweite Hälfte des Buchs beschäftigt sich mit der Ableitung von
Implementierungen und nutzt dafür wieder die beiden Beispiele der
ersten Hälfte. Sie beginnt mit einer ersten Codierung ("deep
embedding") und simplistischer Implementierung des "tile"-Beispiels,
die sie dann mit einer allgemeineren, effizienteren funktionalen
Repräsentation ersetzt. Desweiteren zeigt das Buch, wie man Gesetze in
QuickCheck-Test überführt und passende Generatoren schreibt, damit
die Tests effektiv werden. Dankenswerterweise führt sie auch aus, wie
QuickSpec automatisch viele der Gesetze entdecken kann, die in der
ersten Hälfte des Buches "händisch" konstruiert wurden.

Das gleiche macht das Buch dann auch für das Schatzsuchen-Beispiel,
was schließlich zu einer effizienten, monadenbasierten Implementierung
des Backends für das Spiel führt.

Die dritte Hälfte des Buchs nennt sich "Anschauungsmaterial" und
enthält ein Tutorial für QuickCheck und QuickSpec sowie eine
Zusammenfassung gemeiner algebraischer Gesetze.

### Zielgruppe ###

Die erste Hälfte des Buchs legt das Augenmerk sorgfältig auf die
allgemeine Beschreibung Kombinatorenmodelle und ihrer
Gesetzmäßigkeiten. Die kurze Einführung in Haskellnotation aus dem
Vorwort sollte ausreichen, um diesen Teil auch für Menschen mit nur
wenig Haskell- oder gar funktionalem Programmierkenntnissen
verständlich zu machen. Bei den Implementierungen kommt irgendwann der
Punkt, an dem dieses "basic Haskell" nicht mehr ausreicht und
fortgeschrittene Eigenschaften-- insbesondere Typklassen -- ins Bild
treten: das Verständnis dieses Abschnitts und des enthaltenen
Materials erfordert bessere Haskellkenntnisse sowie ein grundlegendes
Verständnis der dar Konzepte, auf denen zum Beispiel die `Functor`- oder
`Applicative`-Typklassen aufbauen.

### Bewertung

Maguires Buch sammelt Ideen, die schon seit langem in der Gemeinschaft
der funktionalen Programmierung herumschwirren, und fasst sie in einer
umfassenden Methodik mit eingängigen Beispielen zusammen.

Maguire geht weiter als nur "Konzeption" sondern bis hin zur
"Implementierung" und nimmt sich die Feinheiten von
QuickCheck-eneratoren, monadenbasierten Implementierungen und der
effizienten Verwendung von SmallCheck vor. Dies ist nicht nur als
Ressource für funktionale Programmierer wertvoll, sondern auch als
Goldgrube an Lehrmaterialien und -ideen.

Ein
[Reddit-Thread](https://www.reddit.com/r/haskell/comments/uunhic/how_broadly_applicable_is_algebradriven_design/)
zu dem Buch beginnt mit der Frage "Wird sich dies in irgendeiner Art
auf meine alltägliche Enterprise-Software-Entwicklung auswirken?"
(“Will this have any bearing on my day-to-day development of
enterprise software?”). Algebra-basierte Kombinatorenmodelle finden in
vielen Domänen Anwendung und können Eckpfeiler einer flexiblem
Softwarearchitekture sein, die zukünftige Anforderungen vorausahnt.
Doch obwohl diese schon lange anwendbar sind, werden sie selten
implementiert. Leider heißt das, dass die Antwort auf die oben
genannte Frage "kommt drauf an" lauten muss: Wenn Sie sich in einer
Position befinden, in der Sie Einfluss auf die Architektur einer
Enterprise-Software nehmen können, wird die im Buch dargelegte
Methodik massive Auswirkungen auf die alltägliche Entwicklung haben.
Wenn nicht, dann vermutlich nicht. Folglich wünschte ich, das Buch
ginge noch weiter und würde Algebra-basierte Konstruktionskonzepte mit
traditionellen, objektorientierten Konstruktionskonzepten vergleichen.
Zur Schatzsuche erwähnt Maguire ein echtes, ähnliches Projekt, das
"viermal umgeschrieben" werden musste und trotzdem nie ein
"angemessenes Konzept, das alle Anforderungen erfüllte und
gleichzeitig einfach zu benutzen war" hervorbrachte. Zu wissen, was für Rewrites das waren und warum sie fehlschlugen würde das Argument dieses Buchs stützen. Zwangsläufig würden sich Kombinatorenmodelle auch radikal anders als objektorientierte Modelle herausstellen statt nur als etwas das "man so macht".

Die Darstellung ist gut organisiert und baut die Konzepte nach und nach auf. Die Sprache des Buchs ist informell, aber präzise und leicht verständlich. Der Code (der einen erheblichen Teil des Buchs einnimmt) baut sich ebenfalls nach und nach von einfachen Kombinatoren zu fortgeschrittenen Konzepten auf und ist als [Github-Repositorium](https://github.com/isovector/algebra-driven-design) erhältlich.

### Fazit ###

Maguires Buch ist ein wichtiger Beitrag dazu, den Mangel an
Literatur darüber zu beheben, wie komplexe Probleme aus der echten Welt mit funktionaler
Programmierung gelöst werden können. Durch den informellen Stil und die
sorgfältige Darstellung eignet sich die erste Hälfte des Buchs auch
für Leser, denen funktionale Programmierung neu ist. Und selbst wenn Sie sich mit Haskell, Kombinatorenmodellen und Algebra auskennen, wird Maguires Buch Ihnen ermöglichen, diese Ideen effektiver auf Probleme in der echten Welt anzuwenden. Unbedingt lesen.

<small>Aus dem Englischen von Sibylle Hasse.</small>
