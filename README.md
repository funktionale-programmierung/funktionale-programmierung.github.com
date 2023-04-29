# Schreiben von Artikeln

## Entwürfe

Der Entwurf eines neuen Artikels landet erstmal im Unterverzeichnis
`draft`. Dort gibt es auch einen Template für einen neuen Artikel. Bitte das
Template kopieren und eine sinnvollen Dateinamen für den neuen Artikel
vergeben. Der Dateiname für den Artikel muss mit `.md` enden. Für einen Artikel
in `draft/foobar.md` ist nach einem entsprechenden `git commit` und `git push`
der Artikel unter http://funktionale-programmierung.de/draft/foobar.html
einsehbar.

## Header

Hier ein Beispiel für einen Post-Header:

	layout: post
	description: Rein funktionale Programmierung - Teil 2
	title: "Eine kleine Einführung in die rein funktionale Programmierung - Teil 2"
	author: michael-sperber
	tags: ["rein funktional", "Racket", "Schneckenwelt"]
	meta_description: >
	  Mehrzeiliger Text bei dem der Whitespace entfernt wird.
	page_title: "Einführung rein funktionale Programmierung 2"

Zu beachten besonders die Syntax für den Tags-Eintrag: Es funktionieren auch
Leerzeichen-separierte Wortlisten, aber Kommata landen dann beispielsweise im
Tag selbst.

Zum Feld `author` siehe Abschnitt "Autoren".

Das Feld meta-description ist auch sehr wichtig, es enthält eine Zusammenfassung
für Suchmachschinen.

Das Feld `page_title` ermöglicht die Angabe eines anderen Textes für den
HTML-Titel der Seite; ohne diese Angabe wird das Feld `title` verwendet, das
aber oft zu lang für den Seitentitel ist. Der Text "Funktionale Programmierung -
" wird dem Titel immer noch voran gestellt.

## Verkürzung des Artikels für die Übersicht

Das Verkürzen muß manuell passieren, und geschieht mithilfe von eines
HTML-Kommentars im Artikel:

    <h1>Mein Artikel</h1>

    Mein Artikel behandelt... bla bla.

    <!-- more start -->

    Die Details sind folgende...

Der Teil nach dem Kommentar erscheint dann nicht auf der Übersichtsseite,
sondern stattdessen ein "Weiterlesen"-Button.

# Autoren

Für jeden Autor muss in der Datei

    _config.yml

im Mapping `authors` unter einem frei wählbaren Bezeichner ein Name,
eine URL (`href`) für den Anfang eines Artikels, optional ein Bild,
sowie eine Kurzbeschreibung für das Ende des Artikels definiert
sein. Beispiel:

    authors:
      michael-sperber:
        name: Michael Sperber
        image: /author/michael-sperber.jpg
        href: "http://www.example.com"
        about: 'Kurzbeschreibung hier, kann auch HTML enthalten'

Falls `image` gesetzt wurde, muss in [author](./author) eine
entsprechende Datei abgelegt werden:

    author/michael-sperber.jpg

Der Autor eines Artikels muss im Header-Feld `author` mit dem vorher
vergebenen Bezeichner referenziert werden.

# Search-Engine-Optimization

Artikel dazu:

http://blog.shareaholic.com/2012/01/how-to-optimize-your-blog-posts-for-seo/
http://www.seomoz.org/blog/perfecting-keyword-targeting-on-page-optimization

# Blog lokal anschauen

Um den Blog lokal zu erzeugen, benötigst du `jekyll` in Version
3.9. Installationsanleitung gibt es hier:
https://jekyllrb.com/docs/installation/

Sobald `jekyll` installiert ist, kannst du aus dem
Toplevel-Verzeichnis mittels `make serve` den Blog generieren und
einen Webserver starten. Der Blog ist dann über http://127.0.0.1:4000
verfügbar.

## Nix

Wenn du `nix` installiert hast, kannst du den Blog lokal mittels `nix
run` hosten.  Alternativ bringt dich `nix develop` in eine Umgebung,
in der du `jekyll` direkt verwenden kannst.

# Veröffentlichen

Zum Veröffentlichen wird der Artikel aus dem `draft`-Verzeichnis in
das `_posts`-Verzeichnis bewegt. Dabei wird das Veröffentlichungsdatum
dem Dateinamen vorangestellt. Vor dem Veröffentlichen bitte unbedingt
mit den Verantwortlichen des Blogs Kontakt aufnehmen.
