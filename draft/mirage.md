---
layout: post
description: "Mirage"
title: "BOB-Keynote 2015: Towards Functional Operating Systems"
author: michael-sperber
tags: ["OCaml", "BOB"]
---

Die [BOB 2015](http://bobkonf.de/2015/") ist passiert!  Wir hatten
einen tollen Konferenztag mit [vielen Vorträgen und
Tutorials](http://bobkonf.de/2015/programm.html)
zum Besten in der Software-Entwicklung.

Besonders beeindruckt hat uns der Vortrag von unserem Eröffnungsredner
[Anil Madhavapeddy](http://anil.recoil.org/) von der Universität
Cambridge, der über das Projekt [Mirage](http://www.openmirage.org/)
berichtet ein - ein Framework, komplette Betriebssystem-Images aus
OCaml-Code zusammenzustellen.  Mirage ist damit eine spektakuläre
Anwendung funktionaler Programmierung in der Praxis.  Ein
Vortragsvideo sowie die Folien zu Dr. Madhavapeddys Vortrag stehen
[auf der BOB-Seite](http://bobkonf.de/2015/keynote.html).  Dieses
Posting fasst die wichtigsten Aspekte von Mirage zusammen.

<!-- more start -->

# Hintergrund

Wer heutzutage einen Dienst im Internet anbieten muss, greift oft auf
eine Kombination von Betriebssystem und Webserver wie den
[LAMP-Stack](http://de.wikipedia.org/wiki/LAMP_%28Softwarepaket%29)
zu.

Mit LAMP kommt allerdings ein komplettes Linux mit
Multi-User-Verwaltung, Prozessverwaltung, jeder Menge
Betriebssystemtreiber und haufenweise weiterer Funktionalität, die der
LAMP-Stack gar nicht benötigt.  Dies wäre nicht so schlimm, wenn nicht
der größte Teil dieser Funktionalität in C geschrieben und damit
anfällig für Sicherheitsprobleme wie
[Heartbleed](http://heartbleed.com/) oder
[Shellshock](http://de.wikipedia.org/wiki/Shellshock_%28Sicherheitsl%C3%BCcke%29)
sind.  Je mehr Code im System vorhanden ist, desto anfälliger ist das
System.  Muss das sein, wenn ein Dienst im Internet doch all diese
Funktionalität des drumherumliegenden Systems gar nicht benötigt?

Natürlich nicht.

# Mirage

Mirage ist ein Framework, um sogenannte *Unikernels* zu bauen, also
Betriebssystem-Images, die jeweils auf eine bestimmte Aufgabe
spezialisiert sind.  Dazu wird der Code, der den Dienst implementiert,
in [https://ocaml.org/](OCaml) geschrieben.  Mirage macht daraus ein
Image, das direkt unter [Xen](http://xenproject.org/) laufen kann -
ohne dass ein Linux oder anderes Betriebssystem drumherum benötigt
wird.  Diese Images nehmen typischerweise nur wenige Megabytes ein
(manchmal sogar weniger als ein Megabyte), verglichen mit den
Gigabytes, die eine moderne Linux-Installation verschlingt.  Xen ist
um mehrere Größenordnungen weniger komplex als z.B. Linux und isoliert
virtuelle Maschinen deutlich besser als Linux Prozesse voneinander
trennt.  Diese virtuellen Maschinen können so schnell gestartet
werden, dass es sogar möglich ist, Internet-Services so zu bauen, dass
die VM pro Anfrage neu gestartet wird.  (Ein Mirage-Image kann
innerhalb weniger Millisekunden hochfahren.)

Zu Mirage gehört ein kompletter TCP/IP-Stack und inzwischen eine ganze
Reihe von Internet-Protokollen, insbesondere eine [Implementierung von
TLS](https://github.com/mirleft/ocaml-tls).  Damit können eine Reihe
von Internet-Services auf Basis solcher Unikernels aufgesetzt werden,
angefangen mit dem [Mirage-Web-Server](http://www.openmirage.org/).

Die TLS-Implementierung ist exemplarisch für die weiteren
entscheidenden Vorteile, die in OCaml geschriebene Software mit sich
bringt:

- mehr Produktivität
- weniger Fehler
- exzellente Unterstützung für Modularität
- insbesondere sind
  [Buffer-Overflow-Probleme](http://de.wikipedia.org/wiki/Puffer%C3%BCberlauf)
  von vornherein ausgeschlossen - das OCaml-TLS ist deswegen natürlich
  nicht anfällig für [Heartbleed](http://heartbleed.com/) und ähnliche
  Probleme

Das Modulsystem von OCaml wird insbesondere benutzt, um Mirage sehr
weitgehend konfigurierbar zu machen.  So ist es möglich, einen
Mirage-Kernel zu Testzwecken erst einmal als "normales" Programm
laufen zu lassen, indem die Implementierungen der Module für den
TCP/IP-Stack etc. durch solche ausgetauscht werden, welche das
drumherumliegende Betriebssystem benutzen.

Mirage ist Open Source und seit 2013 produktiv.  Die
[Web-Seite](http://openmirage.org/) enthält umfangreiche
Dokumentation, und die Community um das Projekt [wächst
täglich](http://openmirage.org/blog/2014-in-review).

Vielleicht probieren Sie es auch mal aus?

<!-- more end -->

