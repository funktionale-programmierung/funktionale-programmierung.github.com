Schreiben von Artikeln
==

Header
=== 

Hier ein Beispiel für einen Post-Header:

	layout: post
	description: Rein funktionale Programmierung - Teil 2
	title: "Eine kleine Einführung in die rein funktionale Programmierung - Teil 2"
	author: michael-sperber
	tags: ["rein funktional", "Racket", "Schneckenwelt"]

Zu beachten besonders die Syntax für den Tags-Eintrag:  Es
funktionieren auch Leerzeichen-separierte Wortlisten, aber Kommata
landen dann beispielsweise im Tag selbst.

Zum Feld `author` siehe Abschnitt "Autoren".

Verkürzung des Artikels für die Übersicht
===

Das Verkürzen muß manuell passieren, und geschieht mithilfe von eines HTML-Kommentars im Artikel:

    <h1>Mein Artikel</h1>

    Mein Artikel behandelt... bla bla.

    <!-- more start -->

    Die Details sind folgende...

Der Teil nach dem Kommentar erscheint dann nicht auf der Übersichtsseite, sondern stattdessen ein "Weiterlesen"-Button.

Autoren
==

Der Autor eines Artikels muß im Header-Feld `author` mit einem
Bezeichner, wie z.B. "michael-sperber" angegeben werden.

Für jeden Autor muß in den beiden Dateien

    _includes/author_name.html
    _includes/author_about.html

ein Name für den Anfang eines Artikels, bzw. eine Kurzbeschreibung für
das Ende des Artikels definiert sein. Außerdem muß ein kleines Bild unter

    author/<name>.jpg

abgelegt werden.

Ist unter _includes nichts definiert, dann erscheint der unter
`author` definierte Text am Anfang des Artikels, und es gibt keinen
"Zum Autor" Abschnitt am Ende.