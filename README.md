Schreiben von Artikeln
==

Header
=== 

Hier ein Beispiel für einen Post-Header:

	layout: post
	description: Rein funktionale Programmierung - Teil 2
	title: "Eine kleine Einführung in die rein funktionale Programmierung - Teil 2"
	author: <a href="https://www.active-group.de/unternehmen/sperber.html">Michael Sperber</a>
	tags: ["rein funktional", "Racket", "Schneckenwelt"]

Zu beachten besonders die Syntax für den Tags-Eintrag:  Es
funktionieren auch Leerzeichen-separierte Wortlisten, aber Kommata
landen dann beispielsweise im Tag selbst.

Verkürzung des Artikels für die Übersicht
===

Das Verkürzen muß manuell passieren, und geschieht mithilfe von eines HTML-Kommentars im Artikel:

    <h1>Mein Artikel</h1>

    Mein Artikel behandelt... bla bla.

    <!-- more start -->

    Die Details sind folgende...

Der Teil nach dem Kommentar erscheint dann nicht auf der Übersichtsseite, sondern stattdessen ein "Weiterlesen"-Button.

