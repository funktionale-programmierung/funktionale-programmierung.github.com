---
layout: post
description: Buildsysteme mit Haskell
title: "Buildsysteme mit Haskell"
author: stefan-wehr
tags: ["build system", "make", "shake", "Haskell"]
---

Größere Softwareprojekte benutzen (fast) alle ein Buildsystem, um aus Quellcode
automatisch ein fertiges Softwareprodukt zu erstellen. Dazu gehört z.B.
das Kompilieren von Quelldatein, das Linken von Objektdateien, das Generieren
von Dokumentation oder das Zusammenstellen von Distributions-Archiven.

Dieser Blogartikel gibt eine Einführung in das in Haskell geschriebenes Buildsystem
[shake](http://community.haskell.org/~ndm/shake/). Dieses System hat den Vorteil, 
dass Abhängigkeiten
zwischen Build-Artefakten dynamisch, d.h. während das Buildsystem läuft, entstehen können.
Bei [make](...), dem wohl bekanntesten Buildsystem, müssen Abhängigkeiten hingegen
vor Aufruf des Buildsystems bekannt sein, was in der Praxis häufig zu Einschränkungen
und Problemen führt. Die Regeln zur Erstellung von Build-Artefkate werden mit
shake als Haskell-Code aufgeschrieben, was die Formulierung komplexer Regeln 
cIncludes x =
    do s <- readFile' x
       return $ mapMaybe parseInclude (lines s)
    where
      parseInclude line =
          do rest <- List.stripPrefix "#include \"" line
             return $ takeWhile (/= '"') rest
vereinfacht.

Wir benutzen bei uns in der Firma shake, um unser Produkt 
[Checkpad MED](/2013/07/17/medizin-funktional.html) zu kompilieren. Hier spielt
shake seine Stärken voll aus, denn ein wichtiger Bestandteil der Checkpad-Infrastruktur
ist Codegenerierung. Mit shake dynamischen Abhängigkeiten ist es möglich, mit einem Aufruf des
Buildsystems das Programm zur Codegenerierung zu kompilieren, den Code selbst zu generieren
und den generierten Code zu kompilieren und zu linken. 

Neil Mitchell, der Autor von shake, hat eine Variante des Tools für den Einsatz
bei Standard Chartered entwickelt, um wirklich große Softwareprojekte effizient
kompilieren zu können. Details hierzu sowie detaillierte Infos zur internen
Architektur von shake finden Sie in [diesem Artikel](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf).
Nachfolgend wird die Funktionsweise von shake anhand eines einfachen Beispiels
erklärt.

<!-- more start -->

Wir möchten im folgenden ein Buildsystem für ein in der Sprache C geschriebenes
Projekt vorstellen. Um das Beispiel interessanter zu machen wird eine der `.c` Dateien
generiert, wobei der Quellcode des Generators selbst wiederum Teil des Projekts
ist. (Dies entspricht grob dem Setup, welches wir auch im Checkpad Projekt haben.
Zur Vereinfachung nehmen wir hier abetr ein in C geschriebenes Projekt, da
die Abhängkeiten bei der Kompilierung von Haskell-Quellcode deutlich komplizierter
als für C-Quellcode sind.)

Beginnen wir mit einer sehr einfachen Hilfsfunktion zum Extrahieren von `#include` Dateien
aus C-Quelldateien. Die Funktion liest eine `.c` Datei ein und gibt die Dateiname
aller mit `#include "` beginnenden Zeilen zurück.

{% highlight haskell %}
cIncludes :: FilePath -> Action [FilePath]
cIncludes x =
    do s <- readFile' x
       return $ mapMaybe parseInclude (lines s)
    where
      parseInclude line =
          do rest <- List.stripPrefix "#include \"" line
             return $ takeWhile (/= '"') rest
{% endhighlight %}

Der Typ `Action` ist eine in Shake definierte [Monade](/2013/04/18/haskell-monaden.html),
in der alle Build-Aktion ausgeführt werden. Ein wesentlicher Aspect der `Action`-Monade
ist das Tracken von Abhängigkeiten. So ist z.B. die oben verwendete Funktion `readFile'`
ein Wrapper um die `readFile` Funktion aus der Standardbibliothek, die neben
dem Einlesen der Datei sich auch noch eine Abhängigkeit auf die Datei merkt.

Jetzt kommen wir schon zu den eigentlichen Regeln unseres Buildsystems. Regeln
werden in der `Rules`-Monade definiert. Meistens wird dazu der in Shake definierte Operator
`*>` verwendet, welcher folgende Typsignatur hat:

{% highlight haskell %}
(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
{% endhighlight %}

Wir sehen diesen Operator gleich in Aktion:

{% highlight haskell %}
rules :: Rules ()
rules =
    do "*.o" *> \out ->
           do let c = replaceExtension out "c"
              need (cIncludes c)
              system' "gcc" ["-o", out, "-c", c]
{% endhighlight %}

Oben stehende Regel beschreibt, wie eine `.o` Datei erstellt wird. Das erste Argument von `*>`
ist dabei das Pattern das auf die Ausgabedatei matchen muss, dass zweite Argument ist eine
Funktion die diese Ausgabedatei erstellt. In der obigen Regel sehen wir, dass dazu die zur `.o`
gehörende `.c` Datei genommen und mittels `gcc` kompiliert werden. Zuvor wird noch mittels
`need` Abhängigkeiten auf die in der `.c` Datei referenzierten Header-Files eingeführt.


