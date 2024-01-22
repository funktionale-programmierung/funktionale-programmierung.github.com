---
layout: post
title: "Rezension von Sandy Maguire: Algebra-Driven Design"
author: michael-sperber
tags: []
---

Maguires Buch handelt von Domänenmodellierung mit Algebra und
funktionaler Programmierung, oder, anders gesagt, vom konzipieren von
*Kombinatormodellen*.  Dieses Thema ist bereits in zahllosen Papers
gut untersucht worden, angefangen (vermutlich) mit Peter Hendersons
*Functional Geometry*
[[Henderson(1982)](https://dl.acm.org/doi/10.1145/800068.802148)]. 

### Kombinatormodelle

Worum geht's?  Kombinatormodelle sind Domänenmodelle, in dem
Domänenobjekte durch Kombination oder Veränderung schon bestehender
Domänenobjekte entstehen.  In *Functional Geometry* gibt es einen
Datentyp für Bilder namens `Picture`.  Henderson konstruiert zunächst
das Bild einer menschlichen Figur, genannt `man` durch Striche.  Dann
definiert er Funktionen wie `flip` und `overlay`, hier mit
Haskell-Typsignaturen:

```haskell
flip :: Picture -> Picture
overlay :: Picture -> Picture -> Picture
```

Die `flip`-Funktion spiegelt ein Bild an der horizontalen Achse,
`overlay` legt zwei Bilder aufeinander - *kombiniert* sie also.  Bei
Kombinatormodelle gelten häufig Gleichungen wie zum Beispiel, dass
zweimal hintereinander spiegeln wieder das ursprüngliche Bild erzeugt:

```haskell
flip (flip p) == p
```

Für `overlay` gilt ein Assoziativgesetz:

```haskell
overlay a (overlay b c) == overlay (overlay a b) c
```

Die Lehre der Gleichungen in der Mathematik heißt
*Algebra*—entsprechend heißt das Entwerfen von Kombinatormodelle
zusammen mit algebraischen Gleichungen *algebra-driven design*, das
Thema von Maguires Buch.

Simon Peyton Jones und Jean-Marc Eber schrieben schon im im
Jahr 2000:

<ul><blockquote>Zu diesem Zeitpunkt sollte jede funktionale
Programmierer, der etwas auf sich hält, anfangen, vor Wut zu schäumen
und "Bau eine Kombinatorbibliothek!" zu schreien.</blockquote> -- [<a href="https://dl.acm.org/doi/10.1145/351240.351267">Peyton Jones et al.(2000) Peyton Jones, Eber, & Seward</a>]</ul>

Wenn also "jeder funktionale Programmierer, der etwas auf sich hält"
bereits vor zwanzig Jahren davon wusste, warum schreibt man jetzt ein
Buch darüber? Nun ja, Kombinatorbibliotheken (beziehungsweise die
durch sie bereitgestellten Kombinatormodelle) haben die
Softwareentwicklungswelt nicht gerade im Sturm erobert, und zwar aus
mindestens zwei Gründen:

<ul>
<li>Es wurden zwar zahllose Paper geschrieben, diese waren aber
vor allem Expert:innen der funktionalen Programmierung zugänglich, nicht
der breiten Allgemeinheit der Softwareentwickler:innen.</li>
<li>Es haben zwar viele Paper Kombinatormodelle vorgestellt, aber
meist war keine Erklärung dabei, nach welcher Methodik die Autor:innen die
jeweiligen Bibliotheken entwickelt hatten.</li></ul>

Damit wurden Kombinatormodelle effektiv zur Folklore verbannt; sie
sind abseits der funktionalen Programmierung nach wie vor
weitestgehend unbekannt. Maguires Buch erklärt, wie man
Kombinatormodelle nach algebraischen Prinzipien konzipieren kann.
Es ist deutlich zugänglicher geschrieben als die meisten ICFP-Paper zu
Kombinatoren und damit ein wichtiger Beitrag dazu, dieses Versäumnis
der FP-Community anzugehen.

### Übersicht ###

Das Buch beginnt mit einem Vorwort, das erklärt, warum es sinnvoll ist,
verständliche Abstraktionen auf algebraischen Gesetzen aufzubauen und
das die Notationskonventionen für den folgenden Haskell-Code angibt.

Die erste Hälfte des Buchs handelt von der Gestaltung von
Abstraktionen sowie den Gesetzen, die deren Semantik beschreiben. Sie
erklärt die Methodik anhand zweier Beispiele. Das erste Beispiel ist
eine Abwandlung von Hendersons funktionaler Geometrie, "tiles"
("Kacheln") genannt. Das zweite ist ein Softwaresystem, das eine
Schatzsuche unterstützen soll.

Mit "tiles" beginnt das Buch mit den üblichen Kombinatoren zur
Rotation und zum Spiegeln, ergänzt ihre Typsignaturen mit
algebraischen Gesetzen und führt nach und nach fortgeschrittene
Kombinatoren ein, was schließlich zu einem applikativen Funktor
führt.

Ein Thema, das oft bei Erklärungen von Kombinator-Modellen vergessen
wird, ist die Gleichheit von Domänenobjekten: Wenn Bilder durch
wiederholte Aufrufe von Kombinatoren entstehen, dann sind zwei Bilder
sinnvollerweise dann gleich, wenn sie gleich *aussehen*, auch wenn sie
möglicherweise von unterschiedlichen Aufrufe von Kombinatoren erzeugt
wurden.  Maguire behandelt das Thema sorgfältig und führt den Begriff
der *Beobachtung* (*observation*) ein, um diese Idee umzusetzen.

Eine Schatzsuche ist ein Spiel, das von (echten) Menschen gespielt
wird, die durch die Stadt laufen, Aufgaben lösen (gelegentlich
mithilfe von Hinweisen), Nachweise über erledigte Aufgaben im System
hinterlegen und dafür Punkte bekommen. Dies ist ein deutlich
umfangreicheres Beispiel.  Glücklicherweise beschreibt Maguire nicht
nur das Endprodukt, sondern seinen Werdegang entlang mehrerer
Revisionen. Jede Revision bringt die Kombinator-Affinität der Fachlichkeit
ein Stück weiter an die Oberfläche.

Die zweite Hälfte des Buchs beschäftigt sich mit der Ableitung von
Implementierungen und nutzt dafür wieder die beiden Beispiele der
ersten Hälfte. Sie beginnt mit einer ersten Codierung ("deep
embedding") und einfachen Implementierung des "tile"-Beispiels,
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
Zusammenfassung allgemeiner algebraischer Gesetze.

### Zielgruppe ###

Die erste Hälfte des Buchs legt das Augenmerk sorgfältig auf die
allgemeine Beschreibung von Kombinatormodellen und deren
Gesetzmäßigkeiten. Die kurze Einführung in die Programmiersprache
Haskell aus dem Vorwort sollte ausreichen, um diesen Teil auch für
Menschen mit nur wenig Haskell- oder gar funktionalen
Programmierkenntnissen verständlich zu machen. Bei den
Implementierungen kommt irgendwann der Punkt, an dem dieses "basic
Haskell" nicht mehr ausreicht und fortgeschrittene Eigenschaften --
insbesondere Typklassen -- ins Bild treten: das Verständnis dieses
Abschnitts und des enthaltenen Materials erfordert bessere
Haskellkenntnisse sowie ein grundlegendes Verständnis der
Konzepte, auf denen zum Beispiel die `Functor`- oder
`Applicative`-Typklassen aufbauen.

### Bewertung

Maguires Buch sammelt Ideen, die schon seit langem in der Community
der funktionalen Programmierung herumschwirren, und fasst sie in einer
umfassenden Methodik mit eingängigen Beispielen zusammen.

Maguire geht weiter als nur "Konzeption" sondern bis hin zur
"Implementierung" und nimmt sich die Feinheiten von
QuickCheck-Generatoren, monadenbasierten Implementierungen und der
effizienten Verwendung von SmallCheck vor. Dies ist nicht nur als
Ressource für funktionale Programmierer wertvoll, sondern auch als
Goldgrube an Lehrmaterialien und -ideen.

Ein
[Reddit-Thread](https://www.reddit.com/r/haskell/comments/uunhic/how_broadly_applicable_is_algebradriven_design/)
zu dem Buch beginnt mit der Frage "Wird sich dies in irgendeiner Art
auf meine alltägliche Enterprise-Software-Entwicklung auswirken?"
(“Will this have any bearing on my day-to-day development of
enterprise software?”). Algebra-basierte Kombinatormodelle finden in
vielen Domänen Anwendung und können Eckpfeiler einer flexiblen
Softwarearchitektur sein, die zukünftige Anforderungen vorausahnt.
Doch obwohl diese schon lange anwendbar sind, werden sie selten
implementiert. Leider heißt das, dass die Antwort auf die oben
genannte Frage "kommt drauf an" lauten muss: 

Wenn Sie sich in einer
Position befinden, in der Sie Einfluss auf die Architektur einer
Enterprise-Software nehmen können, wird die im Buch dargelegte
Methodik massive Auswirkungen auf die alltägliche Entwicklung haben.
Wenn nicht, dann vermutlich nicht. 

Folglich wünschte ich, das Buch
ginge noch weiter und würde Algebra-basierte Konstruktionskonzepte mit
traditionellen, objektorientierten Konstruktionskonzepten vergleichen.
Zur Schatzsuche erwähnt Maguire ein echtes, ähnliches Projekt, das
"viermal umgeschrieben" werden musste und trotzdem nie ein
"angemessenes Konzept, das alle Anforderungen erfüllte und
gleichzeitig einfach zu benutzen war" hervorbrachte. Zu wissen, was
für Rewrites das waren und warum sie fehlschlugen, würde das Argument
dieses Buchs stützen. Zwangsläufig würden sich Kombinatormodelle
auch radikal anders als objektorientierte Modelle herausstellen statt
nur als etwas das "man so macht".

Die Darstellung ist gut organisiert und baut die Konzepte nach und
nach auf. Die Sprache des Buchs ist informell, aber präzise und leicht
verständlich. Der Code (der einen erheblichen Teil des Buchs einnimmt)
baut sich ebenfalls nach und nach von einfachen Kombinatoren zu
fortgeschrittenen Konzepten auf und ist als
[Github-Repositorium](https://github.com/isovector/algebra-driven-design)
erhältlich.

### Fazit ###

Maguires Buch ist ein wichtiger Beitrag dazu, den Mangel an Literatur
darüber zu beheben, wie komplexe Probleme aus der echten Welt mit
funktionaler Programmierung gelöst werden können. Durch den
informellen Stil und die sorgfältige Darstellung eignet sich die erste
Hälfte des Buchs auch für Leser:innen, denen funktionale Programmierung neu
ist. Und selbst wenn Sie sich mit Haskell, Kombinatormodellen und
Algebra auskennen, wird Maguires Buch Ihnen ermöglichen, diese Ideen
effektiver auf Probleme in der echten Welt anzuwenden. Unbedingt
lesen.
