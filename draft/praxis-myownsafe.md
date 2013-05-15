---
layout: post
description: 
title: "MyOwnSafe - Funktionale Programmierung in der Praxis"
author: david-frese
tags: ["Praxis", "MyOwnSafe", "OCaml"]
---

Die Active Group hat die Webanwendung
[MyOwnSafe](http://www.myownsafe.de/) für die MyOwnSafe GmbH
entwickelt. MyOwnSafe ist ein "intelligenter Tresor mit
Nachlassfunktion". Der Anwender kann in dieser Anwendung Informationen
und Dokumente zu seinen Versicherungen, und seinem Vermögen, sowie
sonstige persönlichen Informationen ablegen und pflegen. Außerdem kann
er Vorkehrungen treffen um diese Informationen im Todes- oder
Krankheitsfall bestimmten Personen zugänglich zu machen.

Dieser Artikel beschreibt das Projekt und seine Architektur,
Hintergründe zu den Entscheidungen die dabei getroffen wurde, sowie
den Erfahrungen die damit gemacht wurden.

<!-- more start -->

## Idee und Vorgaben

Der hatte die persönliche Erfahrung gemacht, dass der Nachlass einer
Person häufig sehr schlecht geregelt ist, und die Angehörigen sehr
viel Arbeit damit haben, überhaupt erstmal in Erfahrung zu bringen was
zu tun ist. Versicherungen müssen gekündigt oder umgeschrieben werden,
Konten aufgelöst, Verträge geändert werden. Auch im "digitalen Leben"
eines Verstorbenen muss viel geregelt werden, wie zuletzt auch Google
mit seiner Nachlass-Automatik gezeigt hat.

Die einzige technische Vorgabe für das Projekt war dabei, dass es ein
Online-Dienst sein sollte, der über eine leicht zugängliche
Webanwendung bedienbar ist.

## Verschlüsselung

Zentrales Element eines _Tresors_ oder _Schließfachs_ ist, dass der
Inhalt _vertraulich_ bleibt. Das bedeutet, dass niemand ausser den
vorgesehenen Personen Einsicht in die Informationen erlangen kann. Es
war daher schnell klar, dass nur eine Client-seitige Verschlüsselung
der Daten mit einem Public-Private-Key-Verfahren das notwendige Mass
an Vertraulichkeit sicherstellen kann.

Das Verschlüsselungsverfahren selbst ist dabei nichts Neues, sondern
eine übliche Kombination von symmetrischer und asymmetrischer
Verschlüsselung mit den etablierten und standartisierten Verfahren
[AES](http://de.wikipedia.org/wiki/Advanced_Encryption_Standard) und
[RSA](http://de.wikipedia.org/wiki/RSA-Kryptosystem). Außergewöhnlich
ist allerdings, dass diese Verschlüsselung der Informationen und
Dateien im Browser des Kunden stattfindet. Der private Schlüssel aus
dem RSA-Verfahren wird auf dem Rechner des Kunden generiert und
verlässt diesen niemals.

Es muss also nicht versucht werden, die Vertraulichkeit durch die
Sicherheit der Serversoftware gegenüber Hacker-Angriffen zu
gewährleisten (wie oft dies nicht gelingt hört man ja immer wieder in
der Presse), sondern die Vertraulichkeit ergibt sich in erster Linie
aus der extrem großen Schwierigkeit die Verschlüsselung zu _knacken_.

Dieser Vorteil der Architektur ist gleichzeitig ein ungewohnter
Nachteil: da der Kunde als einziger im Besitz des Schlüssels bzw.
Passworts ist, können seine Daten bei Verlust des Schlüssels nicht
wiederhergestellt werden. Eine "Passwort vergessen"-Funktion wie in
vielen anderen Online-Diensten ist unmöglich. An diese Eigenschaft von
MyOwnSafe müssen sich die Kunden sicherlich erst gewöhnen.

## Client-Server-Architektur

Da klar war, dass innerhalb des Browsers ohnehin eine so komplexe
Rechenleistung wie die Verschlüsselung von Daten geleistet werden
muss, war es keine weitere Einschränkung die Anwendung mit einem
sogenannten "Fat-Client" umzusetzen.

Bis vor einigen Jahren waren Webanwendungen meist so realisiert, dass
in einer Anwendung auf dem Server bereits das konkrete Aussehen des
Clients (also der Webseite), bzw. die Verbindung verschiedener
Komponenten zu einer Webseite bestimmt und berechnet wurde.

Mit der starken Verbesserung der Programmierbarkeit von Browsern, die
insbesondere durch Google vorangetrieben wurde, ist es heute
allerdings möglich, ganz andere Webanwendungen zu schreiben. Der
Browser wird dabei nicht genutzt um über Hyperlinks von einer Webseite
zur nächsten zu navigieren, sondern die integrierte JavaScript-Engine
wird als komplette Programmier-Plattform für die grafische
Benutzeroberfläche und die Benutzerführung der Anwendung genutzt.

Die Kommunikation zwischen Client und Server besteht dann nur noch aus
einfachen _Remote-Procedure-Calls_, die sich allein auf die
auszuführenden Aktionen und die notwendigen Daten beschränken. Die
Darstellung und Navigation bleibt ganz dem Client überlassen.

## Einfacher Server

Die gewählte Client-Server-Architektur machte es auch möglich, den
Server mit einfachen und "kleinen" Programmen zu realisieren. Er
besteht aus einer Fast-CGI-Anwendung hinter dem
[Lighttpd-Webserver](http://www.lighttpd.net/). Durch diese "kleine
Oberfläche" des Webservers bietet er auch keine große Angriffsfläche
für Hacker-Angriffe.

Zur Implementierung der Fast-CGI-Anwendung wurde die funktionale
Programmiersprache OCaml gewählt. OCaml ist eine bereits seit über
fünfzehn Jahren etablierte und gefestigte Variante aus der
ML-Sprachfamilie. Sie unterstützt neben dem funktionalen Programmieren
auch imperative und objekt-orientierte Paradigmen, ein statisches
Typsystem mit besonders mächtiger Typinferenz, sowie ein gutes
Modulsystem. Es gibt eine Vielzahl von Bibliotheken, insbesondere zur
Web- und Netzwerkprogrammierung, und der Compiler erzeugt sehr
performanten nativen Code. Letzteres gab auch den Ausschlag zu der
Entscheidung für diese Sprache in diesem Projekt.

## Cloud

Höchste Vertraulichkeit dieser sehr sensiblen Daten der Kunden ist in
dieser Architektur bereits durch die Client-seitige RSA- und
AES-Verschlüsselung hergestellt. Nicht einmal der Betreiber selbst
kann die Informationen also einsehen. Dies machte die Entscheidung für
ein Cloud-basiertes Hosting und Datenhaltung leichter, das zwar nicht
ganz so gut absicherbar ist wie ein eigenes Rechenzentrum, aber immens
niedriegere Kosten verursacht und gleichzeitig gut skalierbar auf eine
schwer abzuschätzende, wachsende Anzahl von Kunden ist. Ein großer
Kostenvorteil für ein Start-Up-Unternehmen.

Die Entscheidung für die Dienste von [Amazon
Webservices](http://aws.amazon.com/) fiel in erster Linie aufgrund der
zum Zeitpunkt der Einführen einzigartigen Breite des Angebots durch
Amazon. Die "Elastic Compute Cloud" bietet eine breite Palette von
virtuellen Servern, die "Simple DB" eine flexible NoSQL-Datenbank, und
"Simple Storage Service" einen nahezu unbegrenzten "Container" für
größere Datenpakete.

## NoSQL

Sowohl die "Simple DB" als auch der "Simple Storage Service" von
Amazon fallen in die Kategorie der "NoSQL"-Datenbanken, wobei ersteren
ein sogenannter Document-Storage, und letzterer ein Key-Value-Storage
darstellt. Diese Datenbanken benötigen keine starre Definition der
abzuspeichernden Daten (ein sogenanntes Schema), sondern erlauben dass
jeder Eintrag in der Datenbank andere Felder besitzt.

Diese Freiheit hat sich als sehr positiv für die Entwicklung der
Anwendung herausgestellt, da sie es erlaubt die Migration der
Datenbestände "nebenbei" zu programmieren und auszuführen. Wenn man
die Funktionen zum Lesen der Daten so programmiert, dass sie
Datensätze im alten und neuen Format lesen können, dann bekommt man
die Migration geschenkt, die in relationalen Datenbanksystemen immer
einen erheblichen Aufwand und Fehlerrisiko darstellt.

## Zahlungsdienstleister

Ein weiterer Baustein der Architektur der die Kosten für den Kunden
stark reduziert hat, ist die Nutzung eines externen Dienstleisters für
die Abwicklung von Konto- und Kreditkartenabbuchung. Nicht zuletzt
aufgrund des immer wieder vorkommenden Diebstahls von
Kreditkartendaten aus den Datenbanken von Unternehmen, verlangen die
Kreditkarten-Unternehmen inzwischen aufwändige Zertifizierungen und
regelmäßige Sicherheitsprüfungen bei Firmen, die Kartendaten ihrer
Kunden verarbeiten.

Ein MyOwnSafe-Kunde gibt jedoch seine Konto- oder Kreditkartendaten
niemals direkt an MyOwnSafe, sondern der Web-Client leitet ihn zu
einer Seite des Dienstleisters [ExperCash](http://www.expercash.de)
weiter. Der MyOwnSafe-Server erhält am Ende nur einen eindeutigen
Schlüssel, mit dem er über eine separate Schnittstelle zu einem
ExperCash-Server die Abbuchungen durchführen kann. Weil MyOwnSafe
dadurch nie die Kreditkartendaten der Kunden "sieht", ist keine
teure Zertifizierung notwendig.

## Fazit

Diese sehr moderne Anwendung mit komplexen Anforderungen konnte von
der Active Group extrem kostengünstig und termingerecht umgesetzt
werden. Entscheidend dafür war der Einsatz moderner Techniken, wie die
funktionaler Programmierung in der Sprache [OCaml](http://ocaml.org/)
und den flexiblen NoSQL-Datenbanken, sowie klar getrennte
Aufgabenverteilung zwischen Client und Server, und die sinnvolle
Nutzung externer Serviceanbieter.

Die Active Group setzt daher auch weiterhin auf OCaml und andere
mächtige funktionale Programmiersprachen, um robuste Programme so
effizient wie möglich zu entwickeln.
